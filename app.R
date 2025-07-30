# renv::install("shiny")

library(shiny)

pick <- function(value){
  round(exp(phi_2) / ((phi_1 / value - 1)^phi_3))
}

value <- function(overall){
  phi_1 / (1 + (exp(phi_2) / overall)^(1 / phi_3))
}

ui <- fluidPage(
  fluidRow(
    column(6, titlePanel("Team A Acquires:"),
           textInput("A1", "Pick 1"),
           textInput("A2", "Pick 2"),
           textInput("A3", "Pick 3"),
           textInput("A4", "Pick 4"),
           textInput("A5", "Pick 5")), 
    column(6, titlePanel("Team B Acquires:"),
           textInput("B1", "Pick 1"),
           textInput("B2", "Pick 2"),
           textInput("B3", "Pick 3"),
           textInput("B4", "Pick 4"),
           textInput("B5", "Pick 5"))),
    actionButton("eval", "Evaluate Trade!", class = "btn-lg"))

server <- function(input, output, session) {
}

shinyApp(ui, server)
