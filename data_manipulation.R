## This script is for data manipulation
  ## We have two sources of data
    ## Scraped data from matches (/data/matches.csv)
    ## FIFA rankings from kaggle: https://www.kaggle.com/cashncarry/fifaworldranking (/data/rankings.csv)

## Libraries
library(tidyverse)
library(data.table)
library(broom)

## Upload the raw data
matches_raw <- read.csv(paste0(here::here(), "/data/matches.csv"), sep = ",")
rankings_raw <- read.csv(paste0(here::here(), "/data/rankings.csv"), sep = ",")

## First we clean the rankings data
rankings <- rankings_raw

## Rank_date to real date format
rankings$rank_date <- as.Date(rankings$rank_date)

rankings <- rankings_raw %>%
  filter(confederation == "UEFA") %>% 
  ## Create new ranking with only UEFA countries
  arrange(rank_date, desc(total_points), rank) %>% 
  ## Create a rolling UEFA rank (rank variable is now FIFA rank) 
  group_by(rank_date) %>% 
  mutate(rank_uefa = seq_along(total_points)) 

## Rank_date to real date format
rankings$rank_date <- as.Date(rankings$rank_date)

## Create a dataset for the latest rankings (this is needed in simulations)
latest_rankings <- rankings %>%
  ungroup() %>%
  mutate(rank_date_max = max(rank_date)) %>%
  filter(rank_date == rank_date_max) %>%
  select(country_full, total_points)

## Next we clean the matches data

## Remove one variable
matches <- matches_raw %>%
  select(-X)

## Match date to a real date format
## Create a function for this
date_conversion <- function(x){
  
  ## Split the input to three strings
  y <- strsplit(x, " ")
  
  ## Modify the second string
  if(y[[1]][2] == "Jan"){
    y[[1]][2] <- "01"
  } else if(y[[1]][2] == "Feb") {
    y[[1]][2] <- "02"
  } else if(y[[1]][2] == "Mar") {
    y[[1]][2] <- "03"
  } else if(y[[1]][2] == "Apr") {
    y[[1]][2] <- "04"
  } else if(y[[1]][2] == "May") {
    y[[1]][2] <- "05"
  } else if(y[[1]][2] == "Jun") {
    y[[1]][2] <- "06"
  } else if(y[[1]][2] == "Jul") {
    y[[1]][2] <- "07"
  } else if(y[[1]][2] == "Aug") {
    y[[1]][2] <- "08"
  } else if(y[[1]][2] == "Sep") {
    y[[1]][2] <- "09"
  } else if(y[[1]][2] == "Oct") {
    y[[1]][2] <- "10"
  } else if(y[[1]][2] == "Nov") {
    y[[1]][2] <- "11"
  } else if(y[[1]][2] == "Dec") {
    y[[1]][2] <- "12"
  } 
  
  ## Paste splitted string to a new string y
  y <- paste(y[[1]][3], y[[1]][2], y[[1]][1], sep = "-")
  
  ## Convert the string to date format
  y <- as.Date(y)
  
  ## Return date
  return(y)
}

## Use function to all the dates
matches$match_time <- date_conversion(matches$match_time)

## Create a duplicate dataframe so we have each match for each team
matches_duplicate <- data.frame(match_number = matches$match_number,
                       match_team1 = matches$match_team2,
                       match_team2 = matches$match_team1,
                       match_team1_goals = matches$match_team2_goals,
                       match_team2_goals = matches$match_team1_goals,
                       match_time = matches$match_time,
                       match_nature = matches$match_nature)

## Row bind the datasets 
matches <- rbind(matches, matches_duplicate) 

## Join the necessary data from rankings dataframe
## We need to do a rolling with data.tables package
setDT(matches) 
setDT(rankings)
matches <- rankings[matches, on = c(country_full = "match_team1", rank_date = "match_time"), roll = "nearest"]

## Bind the original and duplicate match data together and sort
matches <- matches%>% 
  arrange(desc(match_number))

## Next we do a self join to add rank and points for team 2
matches <- matches %>% 
  left_join(x = . ,y = ., by = c("match_number" = "match_number", "match_team2" = "country_full"))

## Arrange the datframe columns
matches <- matches %>% 
  select(match_number,
         country_full,
         match_team2,
         match_team1_goals.x,
         match_team2_goals.x,
         rank_uefa.x,
         rank_uefa.y,
         total_points.x,
         total_points.y)

## Change the column names
names(matches) <- c("match_id", "team1", "team2", "team1goals", "team2goals", 
                    "team1rank", "team2rank", "team1points", "team2points")

## Remove NA values
matches <- na.omit(matches)

## Create a variables to track differences between team ranks and points
matches <- matches %>% 
  mutate(diff_goal_abs = team1goals - team2goals,
         diff_goal_ratio = team1goals / team2goals,
         diff_rank = (team1rank - team2rank) * (-1),
         diff_point_abs = team1points - team2points,
         diff_point_ratio = team1points / team2points)

## Clean the data for modelling
data <- matches %>%
  select(team1, diff_point_ratio, diff_goal_abs) %>%
  arrange(team1)

## Rename columns
names(data) <- c("country", "diff_point_ratio", "diff_goal_abs")

## Create models trough a loop
models <- data %>%
  group_by(country) %>%
  do(model = lm(diff_goal_abs ~ diff_point_ratio, data = .))

## Create variables from models outputs
for(i in 1:nrow(models)){
  models[i,3] <- models$model[[i]]$coefficients[1]
  models[i,4] <- models$model[[i]]$coefficients[2]
  models[i,5] <- summary(models$model[[i]])$coeff[1,2]
  models[i,6] <- summary(models$model[[i]])$coeff[2,2]
}

## Rename new dataset
names(models) <- c("country", "model", "b0", "b1", "b0std", "b1std")

## Drop model from the data
models <- models %>%
  select(-model)

## Upload group stages matches
group_stage_matches <- readxl::read_excel(paste0(here::here(), "/data/group_stage_matches.xlsx"))

## Join the matches and models
data <- group_stage_matches %>%
  left_join(x = ., y = models, by = c("team1" = "country")) %>%
  left_join(x = ., y = models, by = c("team2" = "country"))

## Join the latest rankings
data <- data %>%
  left_join(x = ., y = latest_rankings, by = c("team1" = "country_full")) %>%
  left_join(x = ., y = latest_rankings, by = c("team2" = "country_full"))

## Rename columns
names(data) <- c("stage", "group", "match_id", "team1", "team2",
                 "team1b0", "team1b1", "team1b0std", "team1b1std",
                 "team2b0", "team2b1", "team2b0std", "team2b1std",
                 "team1points", "team2points")

## Create a diff_point_ratio (dpr) variable for simulation prediction
data$team1dpr <- data$team1points / data$team2points
data$team2dpr <- data$team2points / data$team1points

## Write a CSV file for the simulations
write.csv(data, paste0(here::here(), "/data/data_for_simulation.csv"))












