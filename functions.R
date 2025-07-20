library(rvest)
library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)

start_year <- 1996
end_year <- 2020

# ---------------------------------------------------------------------------------------

import_draft <- function(year){
  url <- str_glue("https://www.hockey-reference.com/draft/NHL_{year}_entry.html")
  html <- read_html(url)
  Sys.sleep(5) # to avoid getting rate limited
  draft_year_table <- html |> 
    html_element("table") |> 
    html_table() |> 
    janitor::row_to_names(1) |> 
    janitor::clean_names()
  draft_year_table
}

# ---------------------------------------------------------------------------------------

tidy_draft <- function(year){
  draft_year_table <- import_draft(year) |> 
    filter(overall != "Overall" & overall != "" 
           & as.numeric(overall) < 225)|> # remove extra rows and players after pick 224
    type_convert() |> # fix types 
    mutate("year" = year, "ps" = pmax(coalesce(ps, 0), 0), 
           "gp" = coalesce(gp, 0), "to" = coalesce(to, year)) |> 
    select(year, overall, to, pos, gp, ps) # columns we care about
  draft_year_table
}

# ---------------------------------------------------------------------------------------

all_data <- read.csv("all_data.csv")

# ---------------------------------------------------------------------------------------

all_data_prop <- all_data |> 
  group_by(year) |> 
  mutate(prop_ps = ps/sum(ps)) |> 
  group_by(overall) |>  
  summarize(avg_prop_ps = mean(prop_ps),
            .groups = "drop")