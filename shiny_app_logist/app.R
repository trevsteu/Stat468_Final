# renv::install("shiny")
# renv::install("shinyFeedback")
# renv::install("gt")
# renv::install("tidyverse")
# renv::install("rsconnect")
# renv::install("ggplot2")
# renv::install("log4r")

library(shiny)
library(shinyFeedback)
library(gt)
library(tidyverse)
library(rsconnect)
library(ggplot2)
library(log4r)

## THIS IS MY APP USING THE LOGISTIC MODEL

# ------------------------------------------------------------------------------------------------

log <- log4r::logger()

num_picks <- 5

api_url <- "http://18.223.111.213:8080/predict"

valid <- function(picks){
  all(picks %in% c(NA, seq(1,224)))
}

loading_opts <- c("\\", "|", "/", "-") # This allows us to create a loading circle for the log

get_val <- function(selection){
  # this function is needed to convert the predicted logistic value to a percentage,
  #   deal with NAs, and round the prediction
  log4r::info(log, str_glue("({loading_opts[selection %% 4 + 1]}) Creating pick {selection}"))
  temp_val <- httr2::request(api_url) |>
    httr2::req_body_json(list(list(overall = selection))) |>
    httr2::req_perform() |>
    httr2::resp_body_json()
  if(is.na(temp_val$.pred)){
    0
  }
  else{round(1/(1 + exp(-temp_val$.pred)), 7)}
}

log4r::info(log, "Creating pred_vals data frame...")
pred_vals <- data.frame(overall = seq(1,224), xnhls = cbind(lapply(seq(1,224), get_val)))
# Note that the geom_point() that shows up at the end needs all 224 predicted values, so
#   it makes the most sense to load them all at once and then just get them from the df.
log4r::info(log, "pred_vals data frame created successfully!")

first_pick_val <- pred_vals[1, 2]
last_pick_val <- pred_vals[224, 2]

# We need to find the parameters for our logistic model in order to
#   calculate and use its inverse
b0_plus_b1 <- httr2::request(api_url) |>
  httr2::req_body_json(list(list(overall = 1))) |>
  httr2::req_perform() |>
  httr2::resp_body_json()

b0_plus2b1 <- httr2::request(api_url) |>
  httr2::req_body_json(list(list(overall = 2))) |>
  httr2::req_perform() |>
  httr2::resp_body_json()

beta1 <- b0_plus2b1[[1]] - b0_plus_b1[[1]]
beta0 <- b0_plus_b1[[1]] - beta1

pick <- function(xnhlers){
  # Inverse of the get_val function
  round(-1/beta1 * (log(1/xnhlers - 1) + beta0))
}

# ------------------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  useShinyFeedback(),
  titlePanel("Draft Pick Trade Evaluator (Logistic Version)"),
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
  plotOutput("plot", width = "950px"), 
  br()
)

server <- function(input, output, session){
  log4r::info(log, "App Started")
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
          log4r::error(log, "Duplicated picks which are outside acceptable values 
                      (evaluations are not allowed)")
          errors <- TRUE
        }
        else if(!in_range){
          message <- "Ensure this pick is an integer between 1 and 224 (inclusive)."
          log4r::error(log, "Pick outside acceptable values (evaluations are not allowed)")
          errors <- TRUE
        }
        else if(!is.na(input[[pick]]) & is_dup){
          message <- "This pick is included more than once in the trade."
          log4r::warn(log, "Duplicated picks (evaluations are still allowed)")
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

  value_A <- eventReactive(input$eval, 
                           {
                           round(sum(unlist(filter(pred_vals, overall %in% A_picks())[,2])), 7)
                           })
  value_B <- eventReactive(input$eval, 
                           {
                           round(sum(unlist(filter(pred_vals, overall %in% B_picks())[,2])), 7)
                           })  
  
  output$A_points <- renderText({
    str_glue("Team A trades away {value_A()} expected NHLers")
  })
  output$B_points <- renderText({
    str_glue("Team B trades away {value_B()} expected NHLers")
  })
  
  output$equiv <- renderText({
    diff <- round(value_A() - value_B(), 7)
    team <- ifelse(diff > 0, "A", "B")
    winner_stat <- str_glue("Team {team} loses the trade since they give up {abs(diff)} more 
      expected NHLers than they receive")
    if(abs(diff) < 0.001){
      str_glue("The point difference is less than 0.001 expected NHLers, which is
               effectively nothing")
    }
    else if(abs(diff) < last_pick_val){
      str_glue("{winner_stat}, which is less than the value of the last 
      pick in the draft ({last_pick_val} expected NHLers).")
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
    mutate(xnhls = round(as.numeric(xnhls), 7)) |>
    filter(overall %in% A_picks()) |>
    gt() |>  
    tab_header("Picks Team A Gives Away") |> 
    cols_label(overall = "Pick #", xnhls = "Expected NHLers") |>
    data_color(palette = "salmon") |>
    render_gt()
  
  output$pred_gt_B <- pred_vals |>
    mutate(xnhls = round(as.numeric(xnhls), 7)) |>
    filter(overall %in% B_picks()) |>
    gt() |>  
    tab_header("Picks Team B Gives Away") |> 
    cols_label(overall = "Pick #", xnhls = "Expected NHLers") |>
    data_color(palette = "dodgerblue") |>
    render_gt()
  
  output$plot <- renderPlot({
    temp_A <- filter(pred_vals, overall %in% A_picks())
    temp_B <- filter(pred_vals, overall %in% B_picks())
    ggplot(pred_vals, aes(x = overall, y = as.numeric(xnhls))) +
      geom_point(alpha = 0.3) +
      geom_point(data = temp_A, aes(x = overall, y = as.numeric(xnhls)), col = "salmon", size = 3) +
      geom_point(data = temp_B, aes(x = overall, y = as.numeric(xnhls)), col = "dodgerblue", size = 3) + 
      theme_minimal(base_size = 16) + 
      labs(title = "Predicted Value of NHL Draft Picks", 
           subtitle = "Picks given away by Team A in red; picks given away by Team B in blue", 
           y = "Expected NHLers", x = "Pick #", 
           caption = "Values predicted using a logistic regression model")
  })
}

shinyApp(ui, server)

# rsconnect::deployApp('shiny_app_logist/') 