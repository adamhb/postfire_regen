library(sf)
library(tidyverse)

path <- '~/cloud/gdrive/fire_project/local_data/field_validation'
sampleAreas <- read_sf(paste0(path,'/allSampleAreas.geojson'))

roads <- read_sf(paste0(path,'/paved_roads.geojson')) %>% st_transform(.,3311)
road_buff <- st_buffer(roads,90)

#Reproject to NAD83(HARN) / California Albers; EPSG:3311
sampleAreas <- st_transform(sampleAreas,3311) %>%
  st_buffer(dist = -90) %>% 
  mutate(area = st_area(.)) %>%
  mutate(nPoints = floor(area / (pi*90^2))) 


keep <- as.numeric(sampleAreas$nPoints) > 0
sampleAreas <- sampleAreas[keep,]
sampleAreas_noRoads <- st_difference(sampleAreas,road_buff)  


st_write(sampleAreas_noRoads,dsn = paste0(path,"/sampleArea_noRoads.geojson"))
st_write(sampleAreas,dsn = paste0(path,"/sampleArea.geojson"))
  
i <- 6
plot(sampleAreas[i,])
plot(sampleAreas_noRoads[i,])  


  
st_sample(sampleAreas) ,size = sampleAreas$nPoints,type = "regular",by_polygon = T)



testPoly <- sampleAreas[2,]
testPoly_buff <- st_buffer(sampleAreas[2,],-90)

ggplot(testPoly) +
  geom_sf() +
  geom_sf(data = testPoly_buff) +
  geom_sf(data = st_buffer(test1,-90))