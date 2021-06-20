
#teams_data <- read.csv(paste0(here::here(), "/data/teams.csv"), sep = ",")
#matches_data <- readxl::read_excel(paste0(here::here(), "/data/euro2020_matches.xlsx"))
#r24_data <- read.csv(paste0(here::here(), "/data/r24.csv"), sep = ",")

simulate_r16 <- function(teams_data, matches_data, r24_data, randomness){

  #randomness <- 0

  library(tidyverse)

  ## Get the unique code of the group 3rd place qualifiers
  code <- filter(r24_data, grepl("3", r24_data$role) & is.na(position) == TRUE)$role %>%
    substr(1, 1)

  ## Rearrange code to alphabethical order
  code <- do.call(paste, c(as.list(code), sep = ""))

  ## Fill the missing matches to the matches dataset
  ## Filter only matches regarding the round of 16
  matches_data <- matches_data %>%
    filter(stage == "r16")

  ##
  if(code == "abcd"){
    matches_data[3, 5] <- "a3" ## against b1
    matches_data[4, 5] <- "d3" ## against c1
    matches_data[5, 5] <- "c3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "abce") {
    matches_data[3, 5] <- "a3" ## against b1
    matches_data[4, 5] <- "e3" ## against c1
    matches_data[5, 5] <- "c3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "abcf") {
    matches_data[3, 5] <- "a3" ## against b1
    matches_data[4, 5] <- "f3" ## against c1
    matches_data[5, 5] <- "c3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "abde") {
    matches_data[3, 5] <- "d3" ## against b1
    matches_data[4, 5] <- "e3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "a3" ## against e1
  } else if(code == "abdf") {
    matches_data[3, 5] <- "d3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "abef") {
    matches_data[3, 5] <- "e3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "acde") {
    matches_data[3, 5] <- "e3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "acdf") {
    matches_data[3, 5] <- "f3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "acef") {
    matches_data[3, 5] <- "e3" ## against b1
    matches_data[4, 5] <- "f3" ## against c1
    matches_data[5, 5] <- "a3" ## against f1
    matches_data[7, 5] <- "c3" ## against e1
  } else if(code == "adef") {
    matches_data[3, 5] <- "b3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "bcde") {
    matches_data[3, 5] <- "b3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "bcdf") {
    matches_data[3, 5] <- "b3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "bcef") {
    matches_data[3, 5] <- "b3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "bdef") {
    matches_data[3, 5] <- "b3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  } else if(code == "cdef") {
    matches_data[3, 5] <- "b3" ## against b1
    matches_data[4, 5] <- "b3" ## against c1
    matches_data[5, 5] <- "b3" ## against f1
    matches_data[7, 5] <- "b3" ## against e1
  }

  ## Join the matching teams with a match data and generate all round of 16 matches
  ## Matches data to long format for join
  matches_data <- matches_data %>% 
    select(-group) %>% 
    gather(key = "team_id", value = "role", -stage, -match_id) %>% 
    left_join(x = ., y = select(r24_data, role, team), by = c("role" = "role")) %>% 
    select(-role) %>% 
    spread(key = "team_id", value = "team")
    
  ## Join the data for each match
  data <- matches_data %>% 
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
           position = ifelse(winner == team, NA, "16-8")) %>% 
    arrange(match_id) %>% 
    select(team, role, position)
  
  data <- rbind(r24_data %>% filter(is.na(position) == FALSE) %>% select(-X), data) %>% 
    arrange(position)
  
  ## Write a CSV file for the simulations
  write.csv(data, paste0(here::here(), "/data/r16.csv"))
  
  return(data)
  
}

#final_data_r16 <- simulate_r16(teams_data = teams_data, 
 #                        matches_data = matches_data, 
  #                       r24_data = r24_data, 
   #                      randomness = 1)



