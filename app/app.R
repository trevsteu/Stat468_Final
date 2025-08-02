# renv::install("shiny")
# renv::install("shinyFeedback")

library(shiny)
library(shinyFeedback)

# ------------------------------------------------------------------------------------------------

con <- dbConnect(duckdb())

dbExecute(con, "INSTALL httpfs;")
dbExecute(con, "LOAD httpfs;")

all_data_adj <- dbGetQuery(con, "SELECT * 
                            FROM read_parquet('s3://trevor-stat468/all_data_adj.parquet');")


scal_ps <- all_data_adj$ps * C_m[[2]]

nls_scal_ps <- nls(scal_ps ~ SSlogis(log(overall), phi1, phi2, phi3))

num_picks <- 5

phis <- unname(coef(nls_scal_ps))
phi_1 <- phis[1]
phi_2 <- phis[2]
phi_3 <- phis[3]

pick <- function(value){
  round(exp(phi_2) / ((phi_1 / value - 1)^phi_3))
}

value <- function(overall){
  phi_1 / (1 + (exp(phi_2) / overall)^(1 / phi_3))
}


# ------------------------------------------------------------------------------------------------


ui <- fluidPage(
  fluidRow(
    lapply(c("A", "B"), \(t) column(6, titlePanel(str_glue("Team {t} Acquires:")),
           lapply(seq(1,num_picks), \(i)
                  textInput(str_glue("{t}_{i}"), str_glue("Pick {i}")))))),
  fluidRow(column(3, actionButton("eval", "Evaluate Trade!", class = "btn-lg btn-primary"))))

server <- function(input, output, session){
}

shinyApp(ui, server)
