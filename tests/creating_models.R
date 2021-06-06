## This script is for creating country specific models and a dataset from them

## Libraries
library(tidyverse)
library(broom)

## Upload the raw data
data_raw <- read.csv(paste0(here::here(), "/data/data_for_models.csv"), sep = ",")

## Clean the data for modelling
data <- data_raw %>% 
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

## Write a CSV file for the simulations
write.csv(models, paste0(here::here(), "/data/models.csv"))

