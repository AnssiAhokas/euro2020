## This script is for creating a function to simulate the Euro 2020 group stage matches
## Function will have 2 inputs:
  ## 1. Matches data (this a separate excel file)
    ## This file is created by R-script "data_manipulation.R"
  ## 2. Randomness
    ## This tells how much we are going to vary from our models results
    ## This is x times std of the model

library(tidyverse)
library(purrr)

data <- read.csv(paste0(here::here(), "/data/data_for_simulation.csv"), sep = ",")

simulate_group_stage <- function(data, randomness){
  
  ## Start the simulation
  ## Get the raw results of the matches
  data$team1pred_raw <- rnorm(1, mean = data$team1b0, sd = data$team1b0std * randomness) + 
    rnorm(1, mean = data$team1b1, sd = data$team1b1std * randomness) * 
    data$team1dpr
  data$team2pred_raw <- rnorm(1, mean = data$team2b0, sd = data$team2b0std * randomness) + 
    rnorm(1, mean = data$team2b1, sd = data$team2b1std * randomness) * 
    data$team2dpr
  
  ## Round the results (these are the result where the points are calculated)
    ## Unless it is a tie situation 
  data$team1pred <- round(data$team1pred_raw)
  data$team2pred <- round(data$team2pred_raw)
  
  ## Match results
  data$team1result <- ifelse(data$team1pred > data$team2pred, 1, 0)
  data$team2result <- ifelse(data$team2pred > data$team1pred, 1, 0)

  ## Match points
  data$team1points <- ifelse(data$team1result == 1 & data$team2result == 0, 3,
                             ifelse(data$team1result == 0 & data$team2result == 0, 1, 0))
  data$team2points <- ifelse(data$team2result == 1 & data$team1result == 0, 3,
                             ifelse(data$team2result == 0 & data$team1result == 0, 1, 0))
  
  ## Store the result from each country facing each other
  
  data_stored <- data %>% 
    select(team1, team2, team1pred_raw, team2pred_raw) %>% 
    mutate(winner = ifelse(team1pred_raw > team2pred_raw, team1, team2),
           id = pmap_chr(list(team1, team2), ~paste(sort(c(...)), collapse = ""))) %>% 
    select(id, winner)
  
  ## Create a second cache for total goal difference by country
  
  data_stored2 <- data %>% 
    select(team1, team2, team1pred_raw, team2pred_raw) 
  
  ## Create two dataframes and join them
  data_stored2_1 <- data_stored2 %>%
    select(team1, team1pred_raw) %>%
    distinct()
  names(data_stored2_1) <- c("team", "teampred_raw")
  
  data_stored2_2 <- data_stored2 %>%
    select(team2, team2pred_raw) %>%
    distinct()
  names(data_stored2_2) <- c("team", "teampred_raw")
  
  data_stored2 <- rbind(data_stored2_1, data_stored2_2) 
  
  ## Create a summary of goal differences
  data_stored2 <- data_stored2 %>% 
    group_by(team) %>% 
    mutate(summary_teampred_raw = sum(teampred_raw)) %>% 
    select(-teampred_raw) %>% 
    distinct() %>% 
    ungroup()

  ## Create a match id for each country facing each other
    
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
    arrange(group, desc(points)) %>% 
    ungroup()
  
  ## Tie breakers
  data <- data %>% 
    group_by(group) %>% 
    mutate(lag_team = dplyr::lag(team),
           lag_points = dplyr::lag(points)) %>% 
    ungroup() %>% 
    mutate(tie = ifelse(points == lag_points, 1, 0)) %>% 
    mutate(id = pmap_chr(list(team, lag_team), ~paste(sort(c(...)), collapse = "")))
  
  ## Join the raw result data with the group data
  data <- data %>% 
    left_join(x = ., y = data_stored, on = c("id" = "id"))
  
  ## Additional points in tie situation
  data$additional_points <- ifelse(data$tie == 1 & data$team == data$winner, 0.4, 
                                  ifelse(data$tie == 1 & data$team != data$winner, -0.4, 0))
  
  ## Total points 
  data$total_points <- data$points + data$additional_points
  data$total_points <- ifelse(is.na(data$total_points) == TRUE, data$points, data$total_points)
  
  ## Arrange the data
  data <- data %>%
    arrange(group, desc(total_points))
  
  ## Create a rolling rank by each group to later assign roles for each country
  data <- data %>% 
    group_by(group) %>% 
    mutate(rank = seq_along(total_points)) %>% 
    ungroup %>% 
    select(group, team, rank, points, total_points) %>% 
    filter(rank != 4)
  
  data_3rd <- data %>% 
    filter(rank == 3)
  
  ## Join the raw result data with the group data
  data_3rd <- data_3rd %>% 
    left_join(x = ., y = data_stored2, on = c("team" = "team")) %>% 
    arrange(desc(points), desc(summary_teampred_raw)) %>% 
    top_n(4)
  
  ## Take a unique code of 3rd places
  code <- data_3rd %>% 
    select(group) %>% 
    distinct() %>% 
    arrange(group)
  code <- apply(code, 1, paste, collapse="")
  code <- do.call(paste, c(as.list(code), sep = ""))
  
  data <- data %>% 
    filter(rank %in% c(1, 2)) %>% 
    select(team, group, rank) 
  
  data <- rbind(data, data_3rd %>% select(team, group, rank)) %>% 
    arrange(group, rank)
  
  data <- list(data, code)
  
  ## Just check the function is working
  return(data)

}

final_data <- simulate_group_stage(data = data, randomness = 0.5)






