## This script is for data manipulation
  ## We have two sources of data
    ## Scraped data from matches (/data/matches.csv)
    ## FIFA rankings from kaggle: https://www.kaggle.com/cashncarry/fifaworldranking (/data/rankings.csv)

## Libraries
library(tidyverse)

## Upload the raw data
matches_raw <- read.csv(paste0(here::here(), "/data/matches.csv"), sep = ",")
rankings_raw <- read.csv(paste0(here::here(), "/data/rankings.csv"), sep = ",")

## First we clean the matches data
matches <- matches_raw %>%
  select(-X)











## Next we clean the rankings data
rankings <- rankings_raw %>%
  filter(confederation == "UEFA")

  ## Create new ranking with only UEFA countries





