light_run <- F
write_csvs <- T
remote_server <- T

#import libraries
library(tidyverse)
library(parallel)
library(ggcorrplot)

source('utils/system_settings.R') # source internal functions and settings

#set path to data on laptop
path <- "~/cloud/gdrive/fire_project/local_data/fromGEE/"
outpath <- "~/cloud/gdrive/fire_project/local_data/"
figuresPath <- '~/cloud/gdrive/fire_project/figures/'

#set path to data on cluster
if(remote_server == T){
  path <- "/home/rstudio/data/"
  outpath <- '/home/rstudio/output/'
  figuresPath <- '/home/rstudio/figures/'
}




