library(tidyverse)
trainingAreaLabels <- read_csv('data/trainingAreas_visual_interpretation_and_dates_v2.csv')


nrow(trainingAreaLabels)
summary(trainingAreaLabels$area)
