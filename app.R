# renv::install("shiny")

library(shiny)

pick <- function(value){
  round(exp(phi_2) / ((phi_1 / value - 1)^phi_3))
}

value <- function(overall){
  phi_1 / (1 + (exp(phi_2) / overall)^(1 / phi_3))
}

num_picks <- 5

ui <- fluidPage(
  fluidRow(
    lapply(c("A", "B"), \(t) column(6, titlePanel(str_glue("Team {t} Acquires:")),
           lapply(seq(1,num_picks), \(i)
                  textInput(str_glue("{t}_{i}"), str_glue("Pick {i}")))))),
  fluidRow(column(3, actionButton("eval", "Evaluate Trade!", class = "btn-lg btn-primary"))))

server <- function(input, output, session){
  
  
  
}

shinyApp(ui, server)
