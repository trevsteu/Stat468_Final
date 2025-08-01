library(rvest)
library(tidyverse)
library(janitor)
library(dplyr)
library(stringr)
library(DBI)
library(duckdb)


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

# This is a little bit circular. In order to create the table in the database we need
#   to first read the data from a csv (for the sake of rendering time). We then run the  
#   same code as in the Tidy chapter. 

all_data <- read.csv("all_data.csv") 

con <- DBI::dbConnect(duckdb::duckdb(), dbdir = "trevor-stat468.duckdb")
DBI::dbWriteTable(con, "all_data", all_data, overwrite = TRUE)

sql <- 'SELECT * FROM "trevor-stat468"."all_data"'
all_data <- as.data.frame(dbGetQuery(con, sql))
DBI::dbDisconnect(con)

# ---------------------------------------------------------------------------------------

get_length <- Vectorize(function(len){ 
  all_data |> 
    mutate(rem_career_len = to - year - len) |> 
    filter(year %in% 1996:2004 & rem_career_len >= 0) |> 
    summarize(mean = mean(rem_career_len)) |> 
    pull(mean)
})

est_yr <- data.frame(k = seq(1,22)) |>
  mutate(yr = get_length(k))

# ---------------------------------------------------------------------------------------

active_players <- all_data |> 
  filter(to == 2025) |> 
  mutate(career_len = to - year) |> 
  mutate(adj_ps = ps + round(ps / career_len * get_length(career_len), 2)) |> 
  select(-career_len)

inactive_players <- all_data |> 
  filter(to != 2025) |> 
  mutate(adj_ps = ps)


all_data_adj <- rbind(active_players, inactive_players)

# ---------------------------------------------------------------------------------------

get_threshold <- function(year){  
  ifelse(year <= 2017, 200, 82/3*(2025-year)) 
} 

thresholds <- data.frame(year = seq(2015,2020)) |>    
  mutate(t = get_threshold(year))  

# ---------------------------------------------------------------------------------------

all_data_ret <- all_data_adj |>    
  filter(to != 2025) |>   
  mutate(thresh = ifelse(pos == "G", 100, 200),           
         reg = gp >= thresh) |>    
  select(-thresh)  

all_data_act <- all_data_adj |>    
  filter(to == 2025) |>    
  mutate(thresh = ifelse(pos == "G", get_threshold(year) / 2,                           
                         get_threshold(year)),         
         reg = gp >= thresh) |>    
  select(-thresh)  

all_data_adj <- rbind(all_data_ret, all_data_act)

# ---------------------------------------------------------------------------------------

all_data_comb <- all_data_adj |> 
  group_by(overall) |> 
  summarize(mean_ps = mean(ps), 
            mean_gp = mean(gp), 
            mean_adj_ps = mean(adj_ps),
            suc_rate = mean(reg))

# ---------------------------------------------------------------------------------------

metrics <- c("mean_ps", "mean_gp", "mean_adj_ps", "suc_rate")
names <- c("Mean PS", "Mean GP", "Mean Adjusted PS", "Success Rate")

for(i in 1:length(metrics)){
  assign(str_glue("plot_{metrics[i]}"), 
         ggplot(all_data_comb, aes_string(x = "overall", y = metrics[i])) + 
           geom_point() + 
           labs(title = str_glue("{names[i]} verses Overall"), 
                x = "Overall", y = str_glue("{names[i]}")))
}

# ---------------------------------------------------------------------------------------

