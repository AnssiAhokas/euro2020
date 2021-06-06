## This script is for creating a function to simulate the Euro 2020 group stage matches
## Function will have 2 inputs:
  ## 1. Matches data (this a separate excel file)
    ## This file is created by R-script "data_manipulation.R"
  ## 2. Randomness
    ## This tells how much we are going to vary from our models results
    ## This is x times std of the model

library(tidyverse)

data <- read.csv(paste0(here::here(), "/data/data_for_simulation.csv"), sep = ",")

simulate_group_stage <- function(data, randomness){

  ## Start the simulation
  data$team1pred <- round(data$team1b0 + data$team1b1 * data$team1dpr)
  data$team2pred <- round(data$team2b0 + data$team2b1 * data$team2dpr)

  ## Match results
  data$team1result <- ifelse(data$team1pred > data$team2pred, 1, 0)
  data$team2result <- ifelse(data$team2pred > data$team1pred, 1, 0)

  ## Match points
  data$team1points <- ifelse(data$team1result == 1 & data$team2result == 0, 3,
                             ifelse(data$team1result == 0 & data$team2result == 0, 1, 0))
  data$team2points <- ifelse(data$team2result == 1 & data$team1result == 0, 3,
                             ifelse(data$team2result == 0 & data$team1result == 0, 1, 0))

  ## Create a data with each groups and points
  data <- data %>%
    select(group, match_id, team1, team2, team1points, team2points)

  ## Create two duplicate datasets and bind them
  data1 <- data %>%
    select(group, match_id, team1, team1points) %>%
    distinct()
  names(data1) <- c("group", "match_id", "team", "teampoints")
  data2 <- data %>%
    select(group, match_id, team2, team2points) %>%
    distinct()
  names(data2) <- c("group", "match_id", "team", "teampoints")
  data <- rbind(data1, data2)

  ## Calculate points for each team grouped by groups
  data <- data %>%
    group_by(group, team) %>%
    mutate(points = sum(teampoints)) %>%
    select(-match_id, -teampoints) %>%
    distinct() %>%
    arrange(group, desc(points))

  ## Just check the function is working
  return(data)

}

final_data <- simulate_group_stage(data = data)



