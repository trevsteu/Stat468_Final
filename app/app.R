# renv::install("shiny")
# renv::install("shinyFeedback")

library(shiny)
library(shinyFeedback)

# ------------------------------------------------------------------------------------------------

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
  ifelse(overall == 0, 0, 
         phi_1 / (1 + (exp(phi_2) / overall)^(1 / phi_3)))
}

# ------------------------------------------------------------------------------------------------

ui <- fluidPage(
  theme = bslib::bs_theme(bootswatch = "darkly"),
  fluidRow(
    lapply(c("A", "B"), \(t) column(6, titlePanel(str_glue("Team {t} Sends:")),
           lapply(seq(1,num_picks), \(i)
                  numericInput(str_glue("{t}_{i}"), str_glue("Pick {i}"), 
                               min = 0, max = 224, step = 1, value = 0))))),
  "Use Pick = 0 to indicate there is no pick\n",
  fluidRow(column(3, actionButton("eval", "Evaluate Trade!", class = "btn-lg btn-primary"))))


server <- function(input, output, session){
  lapply(c("A", "B"), \(t) lapply(seq(1,num_picks), \(i) 
                                  assign(str_glue("val_{t}{i}"), 
                                         eventReactive(input$eval, {
                                           value(get(str_glue("input${t}{i}")))}))))
}

shinyApp(ui, server)


