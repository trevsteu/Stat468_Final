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

## THIS IS MY APP USING THE NLS MODEL

# ------------------------------------------------------------------------------------------------

# since I can't store an nls object with vetiver, we load the data from a CSV file 
#   refit and the model using the exact same code as in the Model chapter. 

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

pred_vals <- data.frame(overall = seq(1,224), pts = cbind(lapply(seq(1,224), value)))

last_pick_val <- round(value(224), 3)

# ------------------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  useShinyFeedback(),
  titlePanel("Draft Pick Trade Evaluator (NLS Version)"),
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
  "Two tables of the picks included in the trade are given below (they populate
  as the user types). The picks highlighted in red are givem up by Team A, the
  ones highlighted in blue are given up by Team B.",
  fluidRow(column(4, gt_output("pred_gt_A")), column(8, gt_output("pred_gt_B"))),
  br(),
  "A plot of the value of all picks in the draft is included below. The colour
  scheme is the same as in the tables above. Picks not included in the trade are in grey.",
  plotOutput("plot", width = "950px")
)


server <- function(input, output, session){
  observe({
    temp_A <- c(input$A_1, input$A_2, input$A_3, input$A_4, input$A_5)
    temp_B <- c(input$B_1, input$B_2, input$B_3, input$B_4, input$B_5)
    temp_A <- temp_A[!is.na(temp_A)]
    temp_B <- temp_B[!is.na(temp_B)]
    for(t in c("A", "B")){
      for(i in seq(1, num_picks)){
        pick <- str_glue("{t}_{i}")
        in_range <- valid(input[[pick]])
        is_dup <- sum(input[[pick]] == c(temp_A, temp_B)) > 1
        message <- ""
        errors <- FALSE
        if(! in_range & !is.na(input[[pick]]) & is_dup){
          message <- str_glue("Ensure this pick is an integer between 1 and 224 (inclusive). \n
                   This pick is also included more than once in the trade.")
          errors <- TRUE
        }
        else if(!in_range){
          message <- "Ensure this pick is an integer between 1 and 224 (inclusive)."
          errors <- TRUE
        }
        else if(!is.na(input[[pick]]) & is_dup){
          message <- "This pick is included more than once in the trade."
          errors <- TRUE
        }
        shinyFeedback::feedbackWarning(pick, errors, message)}}})

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
    winner_stat <- str_glue("Team {team} loses the trade since they give up {abs(diff)} more 
      points than they receive")
    if(abs(diff) < 0.001){
      str_glue("The point difference is less than 0.001 points, which is
               effectively nothing")
    }
    else if(abs(diff) < last_pick_val){
      str_glue("{winner_stat}, which is less than the value of the last 
      pick in the draft ({last_pick_val} points).")
    }
    else if(abs(diff) > first_pick_val){
      str_glue("{winner_stat}, which is more than the value of the first 
      pick in the draft ({first_pick_val} expected NHLers).")
    }
    else{
      diff_pick <- pick(abs(diff))
      str_glue("{winner_stat}. This trade is roughly equivalent to Team {team} giving up pick
               {diff_pick} in surplus value.")
    }
  })
  
  output$pred_gt_A <- pred_vals |> 
    mutate(pts = round(as.numeric(pts), 3)) |> 
    filter(overall %in% A_picks()) |> 
    gt() |>   
    tab_header("Picks Team A Gives Away") |> 
    cols_label(overall = "Pick #", pts = "Points") |> 
    data_color(palette = "salmon") |> 
    render_gt()
  
  output$pred_gt_B <- pred_vals |> 
    mutate(pts = round(as.numeric(pts), 3)) |> 
    filter(overall %in% B_picks()) |> 
    gt() |>  
    tab_header("Picks Team B Gives Away") |> 
    cols_label(overall = "Pick #", pts = "Points") |> 
    data_color(palette = "dodgerblue") |> 
    render_gt()
  
  output$plot <- renderPlot({
    temp_A <- filter(pred_vals, overall %in% A_picks())
    temp_B <- filter(pred_vals, overall %in% B_picks())
    ggplot(pred_vals, aes(x = overall, y = as.numeric(pts))) + 
      geom_point(alpha = 0.3) + 
      geom_point(data = temp_A, aes(x = overall, y = as.numeric(pts)), col = "salmon", size = 3) + 
      geom_point(data = temp_B, aes(x = overall, y = as.numeric(pts)), col = "dodgerblue", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(title = "Predicted Value of NHL Draft Picks", 
           subtitle = "Picks given away by Team A in red; picks given away by Team B in blue", 
           y = "Points", x = "Pick #", 
           caption = "Values predicted using a non-linear regression model")
  })
}

shinyApp(ui, server)

# rsconnect::deployApp('shiny_app_nls/') 