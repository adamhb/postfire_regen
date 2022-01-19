library(tidyverse)
source('utils/system_settings.R')

options(max.print = 1000)
options(dplyr.print_max = 1e4)

line_int_data <- read_csv('data/pct_coverLineIntercept.csv')
#drone_data <- 

predictions <- read_csv('data/validationPlotPredictions.csv') %>%
  select(name,time,classification_mean) %>%
  drop_na(name) %>%
  rename(plot = name)

validationDF <- line_int_data %>%
  left_join(predictions, by = "plot") %>%
  drop_na(classification_mean)



ggplot(validationDF,
       mapping = aes(conifer_pct_cover,classification_mean)) +
  geom_point() +
  adams_theme

