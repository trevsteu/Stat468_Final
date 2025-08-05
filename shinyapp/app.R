# renv::install("shiny")
# renv::install("shinyFeedback")
# renv::install("gt")
# renv::install("tidyverse")
# renv::install("rsconnect")
# renv::install("ggplot2")

library(shiny)
library(shinyFeedback)
library(gt)
library(tidyverse)
library(rsconnect)
library(ggplot2)

# ------------------------------------------------------------------------------------------------

# since I can't store an nls object with vetiver and the S3 credentials aren't working, 
#   we load the data from a CSV file and refit the model using the exact same code as 
#   in the Model chapter. 

scal_ps <- read.csv("scal_ps.csv")

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
         round(phi_1 / (1 + (exp(phi_2) / overall)^(1 / phi_3)), 3))
}

valid <- function(picks){
  all(picks %in% c(NA, seq(1,224)))
}

valid_A <- TRUE
valid_B <- TRUE 

pred_vals <- list()
for(i in 1:8){
  pred_vals[[i]] <- data.frame(overall = seq(1+28*(i-1), 28*i), 
                               pts = cbind(lapply(seq(1+28*(i-1), 28*i), value)))
}

pred_vals <- data.frame(overall = seq(1,224), pts = cbind(lapply(seq(1,224), value)))

last_pick_val <- round(value(224), 3)

# ------------------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  useShinyFeedback(),
  titlePanel("Draft Pick Trade Evaluator"),
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
  "A table of the picks is given below (it 
  populates as the user types). The picks highlighted in red are givem 
  up by Team A, the ones highlighted in blue are given up by Team B.",
  fluidRow(column(3, gt_output("pred_gt_A")), column(8, gt_output("pred_gt_B"))), 
  br(),
  plotOutput("plot", width = "950px")
)


server <- function(input, output, session){
  observe(
    for(t in c("A", "B")){
      for(i in seq(1, num_picks)){
        pick <- str_glue("{t}_{i}")
        shinyFeedback::feedbackWarning(pick, !valid(input[[pick]]), 
                                       "Ensure this pick is an integer between 1 and 224 (inclusive)")}})
  
  A_picks <- reactive({
    temp_A <- c(input$A_1, input$A_2, input$A_3, input$A_4, input$A_5)
    valid_A <- valid(temp_A)
    req(valid_A)
    temp_A})
  B_picks <- reactive({
    valid_B <- valid(c(input$B_1, input$B_2, input$B_3, input$B_4, input$B_5))
    req(valid_B)
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
    diff <- round(value_A() - value_B(), 3)
    team <- ifelse(diff > 0, "A", "B")
    if(abs(diff) < last_pick_val){
      str_glue("Team {team} gives up {abs(diff)} more points than it 
      receives, which is less than the value of the last pick in the draft 
               ({last_pick_val} points).")
    }
    else if(abs(diff) > 1000){
      str_glue("Team {team} gives up {abs(diff)} more points than it 
      receives, which is more than the value of the first pick in the draft 
               (1000 points).")
    }
    else{
      diff_pick <- pick(abs(diff))
      str_glue("Team {team} gives up {abs(diff)} more points than it 
      receives. This trade is roughly equivalent to Team {team} giving up pick 
               {diff_pick} in surplus value.")
    }
  })
  
  output$pred_gt_A <- pred_vals |> 
    mutate(pts = round(as.numeric(pts), 3)) |> 
    filter(overall %in% A_picks()) |> 
    gt() |>   
    cols_label(overall = "Pick #", pts = "Points") |> 
    data_color(palette = "salmon") |> 
    render_gt()
  
  output$pred_gt_B <- pred_vals |> 
    mutate(pts = round(as.numeric(pts), 3)) |> 
    filter(overall %in% B_picks()) |> 
    gt() |>   
    cols_label(overall = "Pick #", pts = "Points") |> 
    data_color(palette = "dodgerblue") |> 
    render_gt()
  
  output$plot <- renderPlot({
    temp_A <- filter(pred_vals, overall %in% A_picks())
    temp_B <- filter(pred_vals, overall %in% B_picks())
    ggplot(pred_vals, aes(x = overall, y = as.numeric(pts))) + 
      geom_point(alpha = 0.3) + 
      geom_point(data = temp_A, aes(x = overall, y = as.numeric(pts)), col = "salmon", size = 3) + 
      geom_point(data = temp_B, aes(x = overall, y = as.numeric(pts)), col = "dodgerblue", size = 3)
  })
}

shinyApp(ui, server)

# To do 
# use rsconnect::deployApp('shinyapp') to deploy
# - don't allow the same pick to be included on both sides (or twice on the same side)
# - use dev ops stuff 
# - add logging 
# - redo app with lm