library(raster)
library(rgdal)
library(tidyverse)

source('00_setup.R')

pathtoGEEdata <- '~/cloud/gdrive/fire_project/local_data/fromGEE/mostVars/'

list.files(pathtoGEEdata)

##functions
options(max.print = 1000)
options(dplyr.print_max = 1e4)

#get Valid XY
#input: r = raster stack
#output: matrix of valid (non-null) xy values
getValidXY <- function(r = r){
  cell.index <- 1:(dim(r)[1]*dim(r)[2])
  valid.cell.ids <- cell.index[!is.na(values(r[[1]]))]
  #values(r[[1]])[valid.cells]
  valid.cell.x <- as.numeric(xFromCell(r,valid.cell.ids))
  valid.cell.y <- as.numeric(yFromCell(r,valid.cell.ids))
  valid.cell.xy <- tibble(cellID = valid.cell.ids, x = valid.cell.x, y = valid.cell.y)
  return(valid.cell.xy)
}


#check dims of all rasters
for(fa in 1:3){
  mostVarsFile <- paste0(pathtoGEEdata,list.files(path = pathtoGEEdata,pattern = paste0('-FA-',fa,'.tif')))
  r <- stack(mostVarsFile)
  print(dim(r))
  rm(r)
  gc()
}

#load raster data into r object

#this function converts all the tifs in a focal area into a tibble of data
#input focal area
#analysis-ready output data frame
tifToTibble <- function(fa){
  print(paste("working on focal area:",fa))
  
 
  #get tif file path
  mostVarsFile <- paste0(pathtoGEEdata,list.files(path = pathtoGEEdata,pattern = paste0('-FA-',fa,'.tif')))
  
  #load raster data into r object
  r <- stack(mostVarsFile)
  
  #get field names as named vector
  fieldNames <- names(r)
  names(fieldNames) <- fieldNames
  
  #get xy coordinates and cell index number of non-masked pixels
  xy <- getValidXY(r)
  
  #extract data from non-masked pixels and put into a tibble
  extractValidData <- function(var){values(r[[var]])[xy$cellID]}
  output1 <- map_df(fieldNames,extractValidData)
  
  #add the focal area ID, pixel ID, and XY coordinates to tibble
  output2 <- output1 %>% add_column(focalAreaID = fa) %>% cbind(xy) %>% 
    mutate(pixelID = paste0("FA-",focalAreaID,"-",cellID)) %>% select(-cellID)
  
  #remove the raster stack from memory
  rm(r)
  gc()
 
  print(paste("finished focal area:",fa))
  return(output2)
}

#fiure out why focal area 1 doesn't have as many columns
focalAreas <- 1:3
#focalAreas <- focalAreas[c(5,6,7,8)]
focalAreasIndex <- 1:length(focalAreas)

d <- map(.x = focalAreas, .f = tifToTibble)

#move data from list into a df
df <- tibble()
for(i in focalAreasIndex){
  df <- rbind(df,tibble(d[[i]]))
}

model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")

write_csv(x = df, file = paste0(data_path,'FA_1-3.csv'))

