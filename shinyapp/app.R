# renv::install("shiny")
# renv::install("shinyFeedback")
# renv::install("DBI")
# renv::install("duckdb")
# renv::install("gt")
# renv::install("tidyverse")
# renv::install("rsconnect")

library(shiny)
library(shinyFeedback)
library(DBI)
library(duckdb)
library(gt)
library(tidyverse)
library(rsconnect)
library(aws.s3)

# ------------------------------------------------------------------------------------------------

# since I can't store an nls object with vetiver, we load the data from the S3 bucket and refit
#   the model using the exact same code as in the Model chapter. 

con <- dbConnect(duckdb())

dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")

scal_ps <- dbGetQuery(con, "SELECT * 
                            FROM read_parquet('s3://trevor-stat468/scal_ps.parquet');")

nls_scal_ps <- nls(ps ~ SSlogis(log(overall), phi1, phi2, phi3), data = scal_ps)

num_picks <- 5

phis <- unname(coef(nls_scal_ps))
phi_1 <- phis[1]
phi_2 <- phis[2]
phi_3 <- phis[3]

pick <- function(value){
  round(ifelse(value >= 0, exp(phi_2) / ((phi_1 / value - 1)^phi_3), 
               -exp(phi_2) / ((phi_1 / (-value) - 1)^phi_3)))
}

value <- function(overall){
  ifelse(is.na(overall), 0, 
         phi_1 / (1 + (exp(phi_2) / overall)^(1 / phi_3)))
}

valid <- function(picks){
  all(picks %in% c(NA, seq(1,224)))
}

valid_A <- TRUE
valid_B <- TRUE 

pred_vals <- data.frame(overall = seq(1,224), pts = cbind(lapply(seq(1,224), value)))

last_pick_val <- round(value(224), 3)

# ------------------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  useShinyFeedback(),
  fluidRow(
    lapply(c("A", "B"), 
           \(t) column(6, titlePanel(str_glue("Team {t} Trades Away:")),
                       lapply(seq(1,num_picks), \(i)
                              numericInput(str_glue("{t}_{i}"), str_glue("Pick {i}"), 
                                           min = 1, max = 224, step = 1, value = NA))))),
  fluidRow(column(3, actionButton("eval", "Evaluate Trade!", class = "btn-lg btn-primary"))), 
  br(),
  textOutput("A_points"),
  textOutput("B_points"),
  br(),
  textOutput("equiv"), 
  br(), 
  "A full table of predicted values is given below:",
  gt_output("pred_gt")
)

server <- function(input, output, session){
  A_picks <- reactive({
    valid_A <- valid(c(input$A_1, input$A_2, input$A_3, input$A_4, input$A_5))
    shinyFeedback::feedbackWarning("A_1", !valid_A, 
                                   "Please ensure all picks are integers between 1 and 224")
    req(valid_A & valid_B)
    c(input$A_1, input$A_2, input$A_3, input$A_4, input$A_5)})
  B_picks <- reactive({
    valid_B <- valid(c(input$B_1, input$B_2, input$B_3, input$B_4, input$B_5))
    shinyFeedback::feedbackWarning("B_1", !valid_B, 
                                   "Please ensure all picks are integers between 1 and 224")
    req(valid_A & valid_B)
    c(input$B_1, input$B_2, input$B_3, input$B_4, input$B_5)})

  value_A <- eventReactive(input$eval, {round(sum(value(A_picks())), 3)})
  value_B <- eventReactive(input$eval, {round(sum(value(B_picks())), 3)})
  
  output$A_points <- renderText({
    str_glue("Team A trades away {value_A()} points")
  })
  output$B_points <- renderText({
      str_glue("Team B trades away {value_B()} points")
    })
  
  output$equiv <- renderText({
    diff <- value_A() - value_B()
    team <- ifelse(diff > 0, "A", "B")
    if(abs(diff) < last_pick_val){
      str_glue("Team {team} gives up {abs(diff)} more points than it 
      receives, which is less than the value of the last pick in the draft 
               ({last_pick_val} points).")
    }
    else{
      diff_pick <- pick(abs(diff))
      str_glue("Team {team} gives up {abs(diff)} more points than it 
      receives. This trade is roughly equivalent to Team {team} giving up pick 
               {diff_pick} in surplus value.")
    }
  })
  output$pred_gt <- pred_vals |> 
    mutate(pts = round(as.numeric(pts), 3)) |> 
    gt() |>   
    data_color(rows = which(overall %in% A_picks()), palette = "salmon") |> 
    data_color(rows = which(overall %in% B_picks()), palette = "dodgerblue") |> 
    render_gt()
}


shinyApp(ui, server)

# To do 
# use rsconnect::deployApp('shinyapp') to deploy
# - allow any # of picks
# - require picks are integers %in% seq(1,224), 
# - don't allow the same pick to be included on both sides
# - widen gt() object
# - reactively colour gt() cells included in trade by team
# - use dev ops stuff 
# - add logging stuff