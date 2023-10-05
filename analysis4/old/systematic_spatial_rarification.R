source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')
library(tidyverse)
library(sf)
library(sp)


path = '~/cloud/gdrive/fire_project/postfire_regen/analysis4/'
df6 <- read_csv(paste0(path,'modelFittingData_030623.csv'))

d <- st_as_sf(x = df6 %>% 
                  #filter(wilderness == 1) %>% 
                  dplyr::select(pixelID,x,y), 
              coords = c("x","y"), crs = 3310)

check_if_point_close_others <- function(point,points,dist){
  t <- st_intersects(st_buffer(point,dist), points) %>% lengths > 0
  return(t)
}

spatial_rarify <- function(d,dist){
  include <- d[1,]
  for(p in 2:nrow(d)){
    test_point <- d[p,]
    close <- check_if_point_close_others(test_point,include,dist)
    if(close == FALSE){
      include <- rbind(include,test_point)
    }
  }
  return(include)
}

dists <- c(200,600,1000,1500,2000,3000,5000,10000,20000,50000)
for(dist in dists[3]){
  d2 <- spatial_rarify(d,dist)
  spat_rare_pixels <- d2$pixelID
  file_name = paste0("spat_rare_pixels_",as.character(dist),"_nf.csv")
  write_csv(d2,paste0(path,file_name))
}



#st_write(d2,"analysis1/A1pixA2domain_spatial_rare.geojson")

