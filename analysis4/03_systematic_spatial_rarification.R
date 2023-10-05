source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')
library(tidyverse)
library(sf)
library(sp)

redo_spat_rare = FALSE
output_file_name_suffix <- "_wilderness"
#don't forget to change filter on wilderness vs. NF

path = '~/cloud/gdrive/fire_project/postfire_regen/analysis4/'


#pixels on NF land, but not planted (to the best of our knowledge)
#extra layer of certainty that these pixels were not planted because there is a buffer around the facts data
notPlanted <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/not_planted_040323.csv') 
df6 <- read_csv(paste0(path,'modelFittingData_030623.csv'))


df6_filter <- df6

modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))
print(paste("sample size:",nrow(modData)))

#semivariogram to justify 1000 m spatial rarification

savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + s(mediumReburns, k = 6) + 
  s(burnSev, k = 6) + s(tpi, k = 6) + s(hli, k = 6) + s(postFireSAP, k = 6) + 
  te(postFireMaxT, postFirePrecip, k = 6)

mod <- gam(formula = ARI_end_smooth ~
             s(recoveryTime, k = 6) +
             s(mediumReburns, k = 6) +
             s(burnSev, k = 6) +
             s(elevation, k = 6) +
             s(tpi,k = 6) +
             te(postFireSAP,postFirePrecip, k = 6),
             data = modData, select = F, method = "REML")


mod <- gam(formula = savedForm,
           data = modData, select = F, method = "REML")


residual_df <- tibble(x = df6_filter$x, y = df6_filter$y, res = residuals(mod), response = df6_filter$ARI_end_smooth)
coordinates(residual_df) = ~x+y
plot(variogram(res~1, residual_df, cutoff = 3000, width = 100),xlab = "distance [m]", ylab = "semivariance")
#plot(variogram(response~1, residual_df, cutoff = 3000, width = 100))

#plot residuals geographically
residuals_tibble <- tibble(x = df6_filter$x, y = df6_filter$y, res = residuals(mod))
res_tib_spat <- st_as_sf(residuals_tibble, coords = c("x","y"), crs = 3310)
plot(res_points_lat_lon)
#st_write(res_tib_spat,"~/cloud/gdrive/fire_project/postfire_regen/analysis4/res_tib_spat.shp")



#spatial rarification below

d <- st_as_sf(x = df6 %>%
                  #filter(pixelID %in% notPlanted$pixelID) %>%
                  filter(wilderness == 1) %>% 
                  dplyr::select(pixelID,x,y), 
              coords = c("x","y"), crs = 3310)


check_if_point_close_others <- function(point,points,dist){
  t <- st_intersects(st_buffer(point,dist), points) %>% lengths > 0
  return(t)
}

spatial_rarify <- function(d,dist){
  include <- d[1,]
  for(p in 2:nrow(d)){
    print(paste(p/nrow(d) * 100, "% done"))
    test_point <- d[p,]
    close <- check_if_point_close_others(test_point,include,dist)
    if(close == FALSE){
      include <- rbind(include,test_point)
    }
  }
  return(include)
}

if(redo_spat_rare == T){
  d2 <- spatial_rarify(d,1000)
  file_name = paste0("pixels_1000m",output_file_name_suffix)
  write_csv(d2,paste0(path,file_name,".csv"))
  st_write(d2,paste0(path,file_name,".shp"))
}

#############################
#####Analyze Domain##########
#############################
wilderness_pixels_1000m <- read_csv("analysis4/pixels_1000m_wilderness.csv")
nf_pixels_1000m <- read_csv("analysis4/spat_rare_pixels_1000_nf_not_planted.csv")

#How many pixels per fire?
df6 %>%
  filter(pixelID %in% wilderness_pixels_1000m$pixelID) %>%
  #filter(pixelID %in% nf_pixels_1000m$pixelID) %>%
  group_by(FIRE_NAME) %>%
  summarise(n_per_fire = length(ARI_end_smooth)) %>%
  arrange(desc(n_per_fire))

######################################################
#restrict megram to 120 for NF and 50 for wilderness##
######################################################

set.seed(32)
megram_pix_WILD <- df6 %>%
  filter(pixelID %in% wilderness_pixels_1000m$pixelID) %>%
  #filter(pixelID %in% nf_pixels_1000m$pixelID) %>%
  filter(FIRE_NAME == "MEGRAM") %>%
  sample_n(50) %>% pull(pixelID)
to_drop_from_megram_WILD <- df6 %>%
  filter(pixelID %in% wilderness_pixels_1000m$pixelID) %>%
  #filter(pixelID %in% nf_pixels_1000m$pixelID) %>%
  filter(FIRE_NAME == "MEGRAM") %>%
  filter(!pixelID %in% megram_pix_WILD) %>% pull(pixelID) %>% tibble()
write_csv(to_drop_from_megram_WILD,file = "analysis4/to_drop_from_megram_WILD.csv")

set.seed(32)
megram_pix_NF <- df6 %>%
  #filter(pixelID %in% wilderness_pixels_1000m$pixelID) %>%
  filter(pixelID %in% nf_pixels_1000m$pixelID) %>%
  filter(FIRE_NAME == "MEGRAM") %>%
  sample_n(120) %>% pull(pixelID)
to_drop_from_megram_NF <- df6 %>%
  #filter(pixelID %in% wilderness_pixels_1000m$pixelID) %>%
  filter(pixelID %in% nf_pixels_1000m$pixelID) %>%
  filter(FIRE_NAME == "MEGRAM") %>%
  filter(!pixelID %in% megram_pix_NF) %>% pull(pixelID) %>% tibble()
write_csv(to_drop_from_megram_NF,file = "analysis4/to_drop_from_megram_NF.csv")


########################
####Spatial Viewing#####
########################

#wilderness with reducing MEGRAM
wild_megram_reduced <- df6 %>%
  filter(pixelID %in% wilderness_pixels_1000m$pixelID) %>%
  filter(!pixelID %in% to_drop_from_megram_WILD$.)
write_csv(wild_megram_reduced,file = "analysis4/wild_megram_reduced.csv")

wild_megram_reduced %>%
  dplyr::select(pixelID,x,y) %>%
  st_as_sf(x = .,coords = c("x","y"),crs = "EPSG:3310") %>% 
  st_write("analysis4/wild_pixels_megram_reduced.shp")

#wildernes without reducing MEGRAM
wild <- df6 %>%
  filter(pixelID %in% wilderness_pixels_1000m$pixelID) %>% 
  dplyr::select(pixelID,x,y)
write_csv(wild,file = "analysis4/wild.csv")

wild %>%
  st_as_sf(x = .,coords = c("x","y"),crs = "EPSG:3310") %>% 
  st_write("analysis4/wild_pixels.shp")

#NF with reducing MEGRAM
nf_megram_reduced <- df6 %>%
  filter(pixelID %in% nf_pixels_1000m$pixelID) %>%
  filter(!pixelID %in% to_drop_from_megram_NF$.)
write_csv(nf_megram_reduced,file = "analysis4/nf_megram_reduced.csv")

nf_megram_reduced %>%
  dplyr::select(pixelID,x,y) %>%
  st_as_sf(x = .,coords = c("x","y"),crs = "EPSG:3310") %>% 
  st_write("analysis4/NF_pixels_megram_reduced.shp")

#NF with without reducing MEGRAM
nf <- df6 %>%
  filter(pixelID %in% nf_pixels_1000m$pixelID) %>% 
  dplyr::select(pixelID,x,y)
write_csv(nf,file = "analysis4/nf.csv")

nf %>%
  st_as_sf(x = .,coords = c("x","y"),crs = "EPSG:3310") %>% 
  st_write("analysis4/NF_pixels.shp")

