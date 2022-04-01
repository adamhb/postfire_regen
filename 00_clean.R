library(raster)
library(rgdal)
library(tidyverse)

source('00_setup.R')

basePath <- '~/cloud/gdrive/fire_project/local_data/fromGEE/'
conCovPath <- paste0(basePath,'conCov/')
sapPath <- paste0(basePath,'SAP/')
mostVarsPath <- paste0(basePath,'mostVars/')

#which fa's are ready
done <- as.integer(na.omit(str_extract(string = list.files(mostVarsPath),pattern = "[:digit:]{2}")))
remaining <- 10:55
remaining[10:55 %in% done == F]
exclude <- c(remaining[10:55 %in% done == F],27)

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


# for(fa in 1){
#   mostVarsFile <- paste0(pathtoGEEdata,list.files(path = pathtoGEEdata,pattern = paste0('-FA-',fa,'.tif')))
#   r <- stack(mostVarsFile)
#   print(dim(r))
#   rm(r)
#   gc()
# }

#load raster data into r object

#this function converts all the tifs in a focal area into a tibble of data
#input: focal area (integer)
#output: tibble
tifToTibble <- function(fa){
  print(paste("working on focal area:",fa))
  
  conCovFiles <- paste0(conCovPath,list.files(path = conCovPath,pattern = paste0('-FA-',fa,'.tif')))
  SAPFiles <- paste0(sapPath,list.files(path = sapPath,pattern = paste0('-FA-',fa,'.tif'))) #fix this
  mostVarsFiles <- paste0(mostVarsPath,list.files(path = mostVarsPath,pattern = paste0('-FA-',fa,'.tif')))
  rasterFiles <- c(conCovFiles,SAPFiles,mostVarsFiles)
  
  #get field names
  fieldNames <- c(names(stack(conCovFiles)),names(stack(SAPFiles)),names(stack(mostVarsFiles)))
  fieldNames[1:72] <- str_extract(string = fieldNames[1:72],pattern = '^.*\\.')
  names(fieldNames) <- fieldNames
  
  #load raster data into r object
  r <- stack(rasterFiles)
  
  #reset field names
  names(r) <- fieldNames
  str_extract(string = fieldNames[1:72],pattern = '^.*\\.')
  #get xy coordinates and cell index number of non-masked pixels
  xy <- as_tibble(getValidXY(r))
  
  #extract data from non-masked pixels and put into a tibble
  extractValidData <- function(var){values(r[[var]])[xy$cellID]}
  output1 <- as_tibble(map_df(fieldNames,extractValidData))
  
  #add the focal area ID, pixel ID, and XY coordinates to tibble
  output2 <- output1 %>% add_column(focalAreaID = fa) %>% cbind(xy) %>% 
    mutate(pixelID = paste0("FA-",focalAreaID,"-",cellID)) %>% dplyr::select(-cellID)
  
  #remove the raster stack from memory
  rm(r)
  gc()
 
  print(paste("finished focal area:",fa))
  return(output2)
}
tifToTibble_possibly <- possibly(.f = tifToTibble, otherwise = paste("ERROR:"))

focalAreas <- c(28,37,38,41,42,43,44,45,46,50,52)

d <- map(.x = focalAreas, .f = tifToTibble_possibly)

#move data from list into a df
focalAreasIndex <- 1:length(focalAreas)
df <- tibble()
for(i in focalAreasIndex){
  d2 <- dim(tibble(d[[i]]))[2]
  if(d2 == 317){
    df <- rbind(df,tibble(d[[i]]))
  }
}


write_csv(x = df, file = paste0(data_path,'FA_3_31_2022.csv'))







#scratch
# model_run_time_stamp <- Sys.time() %>% 
#   sub(pattern = ":", replacement = "-") %>%
#   sub(pattern = ":", replacement = "-") %>%
#   sub(pattern = " ", replacement = "-")
