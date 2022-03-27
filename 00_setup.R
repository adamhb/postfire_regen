
remote_server <- F
#import libraries
library(tidyverse)
library(parallel)
library(ggcorrplot)

source('utils/system_settings.R') # source internal functions and settings

data_path <- '~/cloud/gdrive/fire_project/local_data/analysis/'

#set path to data on laptop

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
  area <- n_pixels * (30*30) / 1e4 #hectares
  output <- tibble(stat = c("n Focal Areas", "n Patches","n Pixels","Area (ha)"),
                   value = c(n_focal_areas,n_patches,n_pixels,area))

  return(output)
}

#############################
##read and write functions###
#############################
write_csv_to_temp <- function(obj,file_name,perPatch = F){
  write_csv(obj,file = paste0(tmpFolder,file_name,".csv"))
}

writePatchLevel <- function(df,var){
  my_sym <- sym(var)
  tmp <- df %>%
    mutate_at(.vars = "patchID", .funs = as.character) %>%
    select(-pixelID) %>%
    group_by(patchID,focalAreaID,year) %>%
    summarise(mean = mean(!!my_sym, na.rm = T),
              fireYear = mean(fireYear)) %>%
    rename(!!my_sym := mean) %>%
    ungroup()
  
  write_csv_to_temp(tmp, file_name = paste0(var,"PerPatch"))
}

readPatchLevel <- function(var){
  read_csv(paste0(tmpFolder,var,"PerPatch.csv"),show_col_types = FALSE)
}

readPixelLevel <- function(var){
  read_csv(paste0(tmpFolder,var,".csv"),show_col_types = FALSE)
}

