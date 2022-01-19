library(sf)
library(tidyverse)

path <- '~/cloud/gdrive/fire_project/local_data/field_validation'
options(max.print = 1000)
options(dplyr.print_max = 1e4)
# testPoints <- tibble(id = c(1,2), y = c(33.639865873647324,33.63755431501583), x = c(-117.65960172490614,-117.65696086219377)) %>%
#   st_as_sf(coords = c("x","y")) %>% st_set_crs(value = 4326) %>% st_transform(3310)
# 
# testPoints

klamath_validation_points <- st_read(dsn = 'data/klamath_validation_points.geojson') %>% mutate(FID = paste0("B",id))



get92mPoints <- function(centerPoints){
  id <- centerPoints$FID
  coordinates <- st_coordinates(centerPoints)
  
  allPointsNSEW <- tibble()
  
  for(i in 1:length(id)){
    d <- tibble(id = id[i], idc = id[i], x = coordinates[i,1], y = coordinates[i,2])
    d$idc <- id[i]
    eastX <- d$x + 92
    westX <- d$x - 92
    northX <- southX <- d$x
    
    eastY <- westY <- d$y
    northY <- d$y + 92
    southY <- d$y - 92
    
    d_NSEW <- rbind(d,
                     tibble(id = paste0(d$id,'_N'), idc = d$id, x = northX, y = northY),
                     tibble(id = paste0(d$id,'_S'), idc = d$id, x = southX, y = southY),
                     tibble(id = paste0(d$id,'_E'), idc = d$id, x = eastX, y = eastY),
                     tibble(id = paste0(d$id,'_W'), idc = d$id, x = westX, y = westY))
    
    allPointsNSEW <- rbind(allPointsNSEW,d_NSEW)
    
  }
  
  
  return(allPointsNSEW)
}


klamathPointsNSEW <- get92mPoints(klamath_validation_points)
klamathPointsToDo <- read_csv('data/Points_to_do.csv')


klamathToDoSpatial <- klamathPointsToDo %>% rename(idc = FID) %>%
  left_join(klamathPointsNSEW, by = "idc") %>%
  st_as_sf(coords = c('x','y'),crs = 3310)

#export
st_write(klamathToDoSpatial,dsn = 'data/klamathPointsToDo_11_15_2021.geojson',driver = 'GeoJSON')
st_write(klamathToDoSpatial %>%
           select(id) %>% 
           rename(name = id) %>%
           st_transform(crs = 4326),
         dsn = 'data/klamathPointsToDo_11_15_2021.gpx',driver = 'GPX')

#export all feature as one KML
st_write(klamathToDoSpatial %>%
           select(id) %>% 
           rename(name = id) %>%
           st_transform(crs = 4326),
         dsn = 'data/klamathPointsToDo_11_15_2021.kml',driver = 'KML')

#export each point as its own KML
kl <- klamathToDoSpatial %>%
  select(id, idc) %>% 
  rename(name = id) %>%
  st_transform(crs = 4326) 

for(i in unique(kl$idc)){
  e <- kl %>% filter(idc == i) %>% select(name)
  st_write(e,paste0('data/forDroneLink/',e$name[1],'.kml'),driver="KML")
} 




st_write(klamathToDoSpatial,dsn = 'data/klamathPointsToDo_11_15_2021.geojson',driver = 'GeoJSON')


st_as_sf(klamathPointsNSEW,coords = c('x','y'),crs = 3310) %>% st_write(dsn = 'data/klamathPoints.kml',driver = 'KML')
st_as_sf(klamathPointsNSEW,coords = c('x','y'),crs = 3310) %>% st_write(dsn = 'data/klamathValidationPoints.geojson',driver = 'GeoJSON')

