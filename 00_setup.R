

#import libraries
library(tidyverse)
library(parallel)
library(ggcorrplot)

source('utils/system_settings.R') # source internal functions and settings

#set path to data on laptop


figuresPath <- '~/cloud/gdrive/fire_project/figures/'

#set path to data on cluster
if(remote_server == T){
  path <- "/home/rstudio/data/"
  outpath <- '/home/rstudio/output/'
  figuresPath <- '/home/rstudio/figures/'
}





summary_stats <- function(data){
  
  n_focal_areas <- length(unique(data$focalAreaID))
  focal_areas <- unique(data$focalAreaID)
  n_patches <- length(unique(data$patchID))
  n_pixels <- length(unique(data$pixelID))
  area <- n_pixels * (60*60) / 1e4 #hectares
  output <- tibble(stat = c("n Focal Areas", "n Patches","n Pixels","Area (ha)"),
                   value = c(n_focal_areas,n_patches,n_pixels,area))

  return(output)
}
