light_run <- T
write_csvs <- F
#import libraries
library(tidyverse)
library(parallel)
library(ggcorrplot)

source('utils/system_settings.R') # source internal functions and settings

#set path to data on cluster
#path <- "/home/rstudio/data/"
#outpath <- path
#figuresPath <- /home/rstudio/figures


#set path to data on laptop
path <- "~/cloud/gdrive/fire_project/local_data/fromGEE/"
outpath <- "~/cloud/gdrive/fire_project/local_data/"
figuresPath <- '~/cloud/gdrive/fire_project/figures/'
