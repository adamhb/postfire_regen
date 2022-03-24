library(raster)
library(rgdal)
library(tidyverse)

#paths
basePath <- '~/cloud/gdrive/fire_project/local_data/fromGEE/'
#conCovPath <- paste0(basePath,'conCov/')
#sapPath <- paste0(basePath,'SAP/')
mostVarsPath <- paste0(basePath,'mostVars/')

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


#this function converts all the tifs in a focal area into a tibble of data
#input focal area
#analysis-ready output data frame

fa = 2



tifToTibble <- function(fa = 2){
  print(paste("working on focal area:",fa))
  
  #get paths to all files for raster stack
  #conCovFiles <- paste0(conCovPath,list.files(path = conCovPath,pattern = paste0('-FA-',fa,'.tif')))
  #SAPFiles <- paste0(sapPath,list.files(path = sapPath,pattern = paste0('-FA-',fa))) #fix this
  mostVarsFiles <- paste0(mostVarsPath,list.files(path = mostVarsPath,pattern = paste0('-FA-',fa,'.tif')))
  #rasterFiles <- c(conCovFiles,SAPFiles,mostVarsFiles)
  
  #get field names
  #fieldNames <- c(names(stack(conCovFiles)),names(stack(SAPFiles)),names(stack(mostVarsFiles)))
  fieldNames <- names(stack(mostVarsFiles))
  #names(fieldNames) <- fieldNames
  
  #stack the raster files
  r <- stack(mostVarsFiles)
  
  #reset field names
  #names(r) <- fieldNames
  
  #get xy
  xy <- getValidXY(r)
  
  #extract data from valid cells and put into a tibble
  extractValidData <- function(var){values(r[[var]])[xy$cellID]}
  output1 <- map_df(fieldNames,extractValidData)
  
  #add the focal area as a variable
  output2 <- output1 %>% add_column(focalAreaID = fa) %>% cbind(xy) %>% 
    mutate(pixelID = paste0("FA-",focalArea,"-",cellID)) %>% select(-cellID)
  
  #remove the raster stack from memory
  rm(r)
  gc()
 
  return(output2)
  
  print(paste("finished focal area:",fa))
}

fa2 <- tifToTibble(2)

#add all focal areas together (unlist rbind)

