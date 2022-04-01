
remote_server <- F
#import libraries
library(tidyverse)
library(parallel)
library(ggcorrplot)
library(zoo)
library(mgcv)

source('utils/system_settings.R') # source internal functions and settings

data_path <- '~/cloud/gdrive/fire_project/local_data/analysis/'
fig_path <- '~/cloud/gdrive/fire_project/write_up/figures/'
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
  n_pixels <- length(unique(data$pixelID))
  area <- n_pixels * (270*270) / 1e4 #hectares
  output <- tibble(stat = c("n Focal Areas", "n Pixels","Area (ha)"),
                   value = c(n_focal_areas,n_pixels,area))

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


adams_theme <- theme(plot.title = element_text(hjust = 0.5, size = title_size),
                     strip.text.x = element_text(size = axis_size),
                     legend.title = element_blank (),
                     axis.title.x = element_text (size = axis_size), # change the axis title
                     axis.title.y = element_text (size = axis_size),
                     axis.title.y.right = element_text (size = axis_size, color = "blue"),
                     axis.text.x = element_text (size = axis_size, colour = "black"),
                     axis.text.y = element_text (size = axis_size, colour = "black"),
                     legend.text = element_text (size = axis_size),
                     legend.spacing.x = unit(0.3, 'cm'),
                     legend.spacing.y = unit(0.3, 'cm'), #this changes the spacing between groups of legend symbols
                     legend.key.size = unit(0.9, "cm"))
adams_theme <- theme_minimal() + adams_theme



makePNG <- function(fig, path_to_output.x = fig_path, file_name = "unamed_graph",
                    height=PNGheight,  width=PNGwidth, units=PNGunits, res = PNGres){
  

  model_run_time_stamp <- Sys.time() %>% 
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = " ", replacement = "-")
  
    png(paste0(path_to_output.x,file_name,"_",model_run_time_stamp,".png"), height=height, width=width, units=units, res = res)
    print(fig)
    dev.off()
  
}

makePDF <- function(fig, path_to_output.x = fig_path, file_name = "unamed_graph",
                    height= figHeight,  width= figWidge, units= figUnits, res = figRes){
  
  model_run_time_stamp <- Sys.time() %>% 
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = " ", replacement = "-")
  
  pdf(paste0(path_to_output.x,file_name,"_",model_run_time_stamp,".pdf"), height=height, width=width)
  print(fig)
  dev.off()
}


