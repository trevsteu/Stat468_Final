# renv::install("shiny")
# renv::install("shinyFeedback")
# renv::install("DBI")
# renv::install("duckdb")
# renv::install("gt")
# renv::install("tidyverse")

library(shiny)
library(shinyFeedback)
library(DBI)
library(duckdb)
library(gt)
library(tidyverse)

# ------------------------------------------------------------------------------------------------

con <- dbConnect(duckdb())

dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")

scal_ps <- dbGetQuery(con, "SELECT * 
                            FROM read_parquet('s3://trevor-stat468/scal_ps.parquet');")
pred_vals <- dbGetQuery(con, "SELECT * 
                            FROM read_parquet('s3://trevor-stat468/pred_vals.parquet');")

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

last_pick_val <- value(224)

# ------------------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  fluidRow(
    lapply(c("A", "B"), 
           \(t) column(6, titlePanel(str_glue("Team {t} Trades Away:")),
                       lapply(seq(1,num_picks), \(i)
                              numericInput(str_glue("{t}{i}"), str_glue("Pick {i}"), 
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
  
  val_A <- eventReactive(input$eval, {
    value(input$A1) + value(input$A2) + 
      value(input$A3) + value(input$A4) + value(input$A5)
  })
  
  val_B <- eventReactive(
    input$eval, {
      value(input$B1) + value(input$B2) + 
        value(input$B3) + value(input$B4) + value(input$B5)})
  
  output$A_points <- renderText({
    str_glue("Team A trades away {round(val_A(), 3)} points")
  })
  output$B_points <- renderText(
    {str_glue("Team B trades away {round(val_B(), 3)} points")
    })
  
  output$equiv <- renderText({
    diff <- val_A() - val_B()
    team <- ifelse(diff > 0, "A", "B")
    if(abs(diff) < last_pick_val){
      str_glue("Team {team} gives up {abs(round(diff,3))} more points than it 
      receives, which is less than the value of the last pick in the draft 
               ({round(last_pick_val, 3)} points).")
    }
    else{
      diff_pick <- pick(abs(diff))
      str_glue("Team {team} gives up {abs(round(diff,3))} more points than it 
      receives. This trade is roughly equivalent to Team {team} giving up pick 
               {diff_pick} in surplus value.")
    }
  })
  output$pred_gt <- pred_vals |> 
    mutate(pts = round(pts, 3)) |> 
    gt() |> 
    cols_label(overall= "Pick #", pts = "Points") |> 
    render_gt()
}

shinyApp(ui, server)


# To do 
# - allow any # of picks
# - require picks are integers %in% seq(1,224), and don't allow the same pick to be included on both sides
# - widen gt() object
# - colour gt() cells included in trade by team
# - use dev ops stuff 
# - add logging stuff