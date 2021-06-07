
teams_data <- read.csv(paste0(here::here(), "/data/teams.csv"), sep = ",")
matches_data <- readxl::read_excel(paste0(here::here(), "/data/euro2020_matches.xlsx"))
r8_data <- read.csv(paste0(here::here(), "/data/r8.csv"), sep = ",")

simulate_r4 <- function(teams_data, matches_data, r8_data, randomness){
  
  library(tidyverse)
  
  ## Filter only matches regarding the round of 8
  matches_data <- matches_data %>%
    filter(stage == "r4")
  
  ## Join the matching teams with a match data and generate all round of 16 matches
  ## Matches data to long format for join
  data <- matches_data %>% 
    select(-group) %>% 
    gather(key = "team_id", value = "role", -stage, -match_id) %>% 
    left_join(x = ., y = select(r8_data, role, team), by = c("role" = "role")) %>% 
    select(-role) %>% 
    spread(key = "team_id", value = "team")
  
  ## Join the data for each match
  data <- data %>% 
    left_join(x = ., y = select(teams_data, -X), by = c("team1" = "team")) %>% 
    left_join(x = ., y = select(teams_data, -X), by = c("team2" = "team"))
  
  names(data) <- c("stage", "match_id", "team1", "team2",
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
  
  ## Match results
  data$winner <- ifelse(data$team1pred_raw > data$team2pred_raw, data$team1, data$team2)
  
  ## Rearrange dataframe 
  data <- data %>% 
    select(match_id, team1, team2, winner) %>% 
    gather(key = "team_id", value = "team", -match_id, -winner) %>% 
    mutate(role = ifelse(winner == team, 
                         paste("w", as.character(match_id), sep = ""), 
                         paste("l", as.character(match_id), sep = "")),
           position = ifelse(winner == team, NA, "4-2")) %>% 
    arrange(match_id) %>% 
    select(team, role, position)
  
  data <- rbind(r8_data %>% filter(is.na(position) == FALSE) %>% select(-X), data) %>% 
    arrange(position)
  
  ## Write a CSV file for the simulations
  write.csv(data, paste0(here::here(), "/data/r4.csv"))
  
  return(data)
  
}

final_data_r4 <- simulate_r4(teams_data = teams_data, 
                             matches_data = matches_data, 
                             r8_data = r8_data, 
                             randomness = 1)