## This script is for data manipulation
  ## We have two sources of data
    ## Scraped data from matches (/data/matches.csv)
    ## FIFA rankings from kaggle: https://www.kaggle.com/cashncarry/fifaworldranking (/data/rankings.csv)

## Libraries
library(tidyverse)

## Upload the raw data
matches_raw <- read.csv(paste0(here::here(), "/data/matches.csv"), sep = ",")
rankings_raw <- read.csv(paste0(here::here(), "/data/rankings.csv"), sep = ",")

## First we clean the rankings data

## Rank_date to real date format
rankings$rank_date <- as.Date(rankings$rank_date)

rankings <- rankings_raw %>%
  filter(confederation == "UEFA") %>% 
  ## Create new ranking with only UEFA countries
  arrange(rank_date, desc(total_points), rank) %>% 
  ## Create a rolling UEFA rank (rank variable is now FIFA rank) 
  group_by(rank_date) %>% 
  mutate(rank_uefa = seq_along(total_points)) 




## Next we clean the matches data

## Match date to a real date format
#matches$match_time <- as.Date(matches$match_time, format = "%d %b %y")

matches <- matches_raw %>%
  select(-X)

## Create a duplicate dataframe so we have each match for each team
matches_duplicate <- data.frame(match_number = matches$match_number,
                       match_team1 = matches$match_team2,
                       match_team2 = matches$match_team1,
                       match_team1_goals = matches$match_team2_goals,
                       match_team2_goals = matches$match_team1_goals,
                       match_time = matches$match_time,
                       match_nature = matches$match_nature)

## Bind the original and duplicate match data together and sort
matches <- rbind(matches, matches_duplicate) %>% 
  arrange(desc(match_number))

















