#import libraries
library(tidyverse)
library(raster)
library(sf)
library(rgdal)
source('utils/system_settings.R')

#import data from GEE
path <- ("~/cloud/gdrive/fire_project/local_data/fromGEE/")
files <- list.files(path,pattern = "tif")
old2new_names <- read_csv(file = "tmp/names.csv")


rast <- stack(paste0(path,files[2]))

test <- as.data.frame(rast, na.rm = TRUE)


df <- tibble()
for(f in files){
  
  print(paste("processing",f))
  rast <- stack(paste0(path,f))
  
  #clean data
  n_bands <- length(names(rast))
  n_pixels <- dim(rast[[1]])[1] * dim(rast[[1]])[2]
  years <- as.character(1984:2020)
  
  df_tmp <- tibble(pixelID = 1:n_pixels)
  for(i in 1:n_bands){
    t <- tibble(values(rast[[i]])) #make faster? ignore empty cells?
    names(t) <- names(rast[[i]])
    df <- cbind(df_tmp,t)
  }
  
  names(df_tmp) <- c("pixelID",old2new_names$new_name)
  df <- rbind(df,df_tmp)
  print(paste("done with",f))
}

str(df)
#drop pixels that are not in the study

#ID Vars
IDvars <- c("pixelID","PatchID","focalAreaID")



#creates a new DF that shows how a time-varying variable (user-defined) changes over time for each pixel
CreateTimeTrajDF <- function(inputDF, timeVar){
  
  assign(x = timeVar,value = eval(as.name(timeVar)))
  
  df %>% select(IDvars, fireYear) %>%
    gather() %>%
    mutate(year2 = case_when(
      
    ))
  
  
  
}








names(rast[[164]])

summary(values(rast[[165]]))

plot(rast[[46]])








names(rast)

#explore and visualize data
df %>%
  drop_na(PatchID) %>%
  mutate_at(.vars = "PatchID", .funs = as.character) %>%
  group_by(PatchID) %>%
  summarise(n = length(PatchID)) %>% pull(n) %>% hist()

#create recovery trajectories
Recovery <- df %>%
  dplyr:: select(pixelID,FireYear,89:125) %>% 
  drop_na(ConProb2013) %>% 
  gather(ConProb2013:ConProb1998, key = "year",value = "ConProb") %>%
  mutate_at(.vars = "year",.funs = function(x){as.integer(substr(x,start = 8,stop = 12))} ) %>%
  arrange(pixelID,year)

#figure of recovery trajectory
Recovery %>% 
  filter(FireYear == 1992) %>%
  group_by(year) %>%
  summarise(ConProb = mean(ConProb),
            SD = sd(ConProb)) %>%
  ggplot(aes(year,ConProb)) +
  geom_line() +
  adams_theme
 

#pixel planted

  


































