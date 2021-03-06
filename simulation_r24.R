## This script is for creating a function to simulate the Euro 2020 group stage matches
## Function will have 2 inputs:
  ## 1. Matches data (this a separate excel file)
    ## This file is created by R-script "data_manipulation.R"
  ## 2. Randomness
    ## This tells how much we are going to vary from our models results
    ## This is x times std of the model

library(tidyverse)
library(purrr)

teams_data <- read.csv(paste0(here::here(), "/data/teams.csv"), sep = ",")
matches_data <- readxl::read_excel(paste0(here::here(), "/data/euro2020_matches.xlsx"))

simulate_r24 <- function(teams_data, matches_data, randomness){
  
  ## Filter matches data
  matches_data <- matches_data %>%
    filter(stage == "group")

  ## Join the datasets
  data <- matches_data %>%
    left_join(x = ., y = teams_data, by = c("team1" = "team")) %>%
    left_join(x = ., y = teams_data, by = c("team2" = "team")) %>%
    select(-X.x, -X.y)

  names(data) <- c("stage", "group", "match_id", "team1", "team2",
                 "team1b0", "team1b1", "team1b0std", "team1b1std", "team1points",
                 "team2b0", "team2b1", "team2b0std", "team2b1std", "team2points")

  ## Create a diff_point_ratio (dpr) variable for simulation prediction
  data$team1dpr <- data$team1points / data$team2points
  data$team2dpr <- data$team2points / data$team1points

  ## Start the simulation
  ## Get the raw results of the matches
  data$team1pred_raw <- rnorm(1, mean = data$team1b0, sd = data$team1b0std * randomness) +
    rnorm(1, mean = data$team1b1, sd = data$team1b1std * randomness) * data$team1dpr
  data$team2pred_raw <- rnorm(1, mean = data$team2b0, sd = data$team2b0std * randomness) +
    rnorm(1, mean = data$team2b1, sd = data$team2b1std * randomness) * data$team2dpr

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
    left_join(x = ., y = data_stored, by = c("id" = "id"))

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
    select(group, team, rank, points, total_points)

  data_3rd <- data %>%
    filter(rank == 3)

  ## Join the raw result data with the group data
  data_3rd <- data_3rd %>%
    left_join(x = ., y = data_stored2, by = c("team" = "team")) %>%
    arrange(desc(points), desc(summary_teampred_raw)) %>%
    top_n(4) %>%
    select(team) %>%
    mutate(qualified = 1)

  data <- data %>%
    left_join(x = ., y = data_3rd, by = c("team" = "team")) %>%
    mutate(help = ifelse(rank %in% c(1, 2) | qualified == 1, 1, 0)) %>% 
    mutate(position = ifelse(is.na(help) == TRUE, "16-24", NA),
           role = paste0(group, rank, sep = "")) %>% 
    select(team, role, position)

  ## Write a CSV file for the simulations
  write.csv(data, paste0(here::here(), "/data/r24.csv"))

  ## Just check the function is working
  return(data)

}

#final_data_r24 <- simulate_r24(teams_data = teams_data, matches_data = matches_data, randomness = 1)






