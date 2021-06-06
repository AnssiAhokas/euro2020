## This script is for data manipulation
  ## We have two sources of data
    ## Scraped data from matches (/data/matches.csv)
    ## FIFA rankings from kaggle: https://www.kaggle.com/cashncarry/fifaworldranking (/data/rankings.csv)

## Libraries
library(tidyverse)
library(data.table)

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

## Rank_date to real date format
rankings$rank_date <- as.Date(rankings$rank_date)

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

## Save the data to CSV so we can use it in creating models
write.csv(matches, paste0(here::here(), "/data/data_for_models.csv"))

## Just check the data quality with a single model
model <- lm(data = matches %>% filter(team1 == "Spain"), diff_goal_abs ~ diff_point_ratio)
summary(model)












