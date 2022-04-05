# R code for the final project report
library(tidyverse)
library(ggplot2)

data <- read_csv("food_hygiene_rating_data.csv", col_names=TRUE)
data_eda <- data %>% 
  select(LocalAuthorityCode, RatingDate, BusinessType, RatingValue)
