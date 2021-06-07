
teams_data <- read.csv(paste0(here::here(), "/data/teams.csv"), sep = ",")
matches_data <- readxl::read_excel(paste0(here::here(), "/data/euro2020_matches.xlsx"))
r24_data <- read.csv(paste0(here::here(), "/data/r24.csv"), sep = ",")












simulate_r16 <- function(teams_data, matches_data, r24_data, randomness){

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




}



