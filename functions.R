# renv::install("rvest")
# renv::install("tidyverse")
# renv::install("janitor")
# renv::install("dplyr")
# renv::install("stringr")

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
    filter(overall != "Overall" & overall != "" & # remove extra rows
             as.numeric(overall) < 225 & # remove players drafted after pick 224
             amateur_team != "()") |> # remove invalid/forfeited picks 
    type_convert() |> # fix types 
    mutate("year" = year, "ps" = pmax(coalesce(ps, 0), 0), 
           "gp" = coalesce(gp, 0), "to" = coalesce(to, year), 
           "pos" = ifelse(str_count(pos, "G") == 1, "G", 
                          ifelse(str_count(pos, "D") == 1, "D", "F"))) |> 
    select(year, overall, to, pos, ps, gp) # columns we care about
  draft_year_table
}

# ---------------------------------------------------------------------------------------

all_data <- read.csv("all_data.csv")
all_data_adj <- read.csv("all_data_adj.csv")
all_data_comb <- read.csv("all_data_comb.csv")
scal_ps <- read.csv("scal_ps.csv")

pred_vals_nls <- read.csv("pred_vals_nls.csv")
pred_vals_logist <- read.csv("pred_vals_logist.csv")

# ---------------------------------------------------------------------------------------

metrics <- c("mean_ps", "mean_gp", "mean_adj_ps", "suc_rate")
names <- c("PS", "GP", "Adjusted PS", "Success Rate")

for(i in seq(1, length(metrics)-1)){
  assign(str_glue("plot_{metrics[i]}"), 
         ggplot(all_data_comb, aes_string(x = "overall", y = metrics[i])) + 
           geom_point() + 
           labs(title = str_glue("Mean {names[i]} verses Pick Number"), 
                x = "Pick Number", y = str_glue("{names[i]}")))
}

plot_suc_rate <- ggplot(all_data_comb, aes(x = overall,  y = suc_rate)) + 
  geom_point(position = position_jitter(width = 0, height = 0.015)) + 
  labs(title = "Success Rate verses Pick Number", 
       x = "Pick Number", y = "Success Rate", 
       caption = "Figure 7.2.1: Aggregated Metrics")

# ---------------------------------------------------------------------------------------