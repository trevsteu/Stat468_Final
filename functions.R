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
             !is.na(pos))  |> # remove forfeited picks
    type_convert() |> # fix types 
    mutate("year" = year, "ps" = pmax(coalesce(ps, 0), 0), 
           "gp" = coalesce(gp, 0), "to" = coalesce(to, year)) |> 
    select(year, overall, to, pos, gp, ps) # columns we care about
  draft_year_table
}

# ---------------------------------------------------------------------------------------

all_data <- read.csv("all_data.csv")

# ---------------------------------------------------------------------------------------

get_threshold <- function(year){
  ifelse(year <= 2017, 200, 82/3*(2025-year))
}

thresholds <- data.frame(year = seq(2015,2020)) |> 
  mutate(t = get_threshold(year))

# ---------------------------------------------------------------------------------------

all_data_ret <- all_data |> 
  filter(to != 2025) |> 
  mutate(thresh = ifelse(pos == "G", 100, 200), 
         reg = gp >= thresh) |> 
  select(-thresh)

all_data_act <- all_data |> 
  filter(to == 2025) |> 
  mutate(thresh = ifelse(pos == "G", get_threshold(year) / 2, 
                         get_threshold(year)),
         reg = gp >= thresh) |> 
  select(-thresh)

all_data_adj <- rbind(all_data_ret, all_data_act)

# ---------------------------------------------------------------------------------------

get_length <- function(len){ 
  all_data_adj |> 
    mutate(rem_career_len = to - year - len) |> 
    filter(year %in% 1996:2004 & rem_career_len >= 0) |> 
    summarize(mean = mean(rem_career_len)) |> 
    pull(mean)
}

est_yr <- data.frame(k = seq(1,22)) |> 
  rowwise() |> 
  mutate(yr = get_length(k)) |> 
  ungroup()

# ---------------------------------------------------------------------------------------

active_players <- all_data_adj |> 
  filter(to == 2025) |> 
  mutate(career_len = to - year) |> 
  rowwise() |> 
  mutate(adj_ps = ps + round(ps / career_len * get_length(career_len), 2)) |> 
  ungroup() |> 
  select(-career_len)

inactive_players <- all_data_adj |> 
  filter(to != 2025) |> 
  mutate(adj_ps = ps)

all_data_adj <- rbind(active_players, inactive_players)

# ---------------------------------------------------------------------------------------

all_data_comb <- all_data_adj |> 
  group_by(overall) |> 
  summarize(mean_ps = mean(ps), 
            mean_gp = mean(gp), 
            suc_rate = mean(reg), 
            mean_adj_ps = mean(adj_ps))

# ---------------------------------------------------------------------------------------