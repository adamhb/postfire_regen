source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')
library(sf)

#df6 <- read_csv('analysis4/modelFittingData_021723.csv') #zero inflated
df6 <- read_csv('analysis4/modelFittingData_030623.csv') #ARI smooth can be negative

notPlanted <- read_csv('analysis4/not_planted_040323.csv')

#df6 <- df6 %>% filter(pixelID %in% notPlanted$pixelID)

####################################
#############Explore data below########################
#######################################################
hist(df6$recovery_rate)
hist(df6$ARI_end_smooth)
nrow(df6 %>%filter(wilderness ==1))


################################
###########filters##############
################################

df6 %>% group_by(OBJECTID) %>%
  summarise(n_per_fire = length(ARI_end_smooth)) %>%
  arrange(desc(n_per_fire)) %>%
  ggplot(aes(n_per_fire)) +
  geom_histogram() +
  scale_x_continuous(limits = c(0,300))
  


#
df6 %>%
  #filter(postFireSAP < 0.05) %>%
  filter(wilderness == 1) %>%
  #filter(pixelID %in% notPlanted$pixelID) %>%
  ggplot(aes(postFireSAP,ARI_end_smooth)) +
  geom_point() +
  geom_smooth(method = "loess")


df6 %>%
  #filter(postFireSAP < 0.05) %>%
  filter(wilderness == 1) %>%
  #filter(pixelID %in% notPlanted$pixelID) %>%
  ggplot(aes(postFirePrecip,ARI_end_smooth)) +
  geom_point() +
  geom_smooth(method = "loess")



df6 %>%
  #filter(postFireSAP < 0.05) %>%
  filter(wilderness == 1) %>%
  group_by(FIRE_NAME) %>%
  summarise(n = length(x)) %>%
  arrange(desc(n))

###Explore points that have high ARI smooth and low SAP.

#Many of these points are near FACTS or in FACTS zones.
problematic_points <- df6 %>%
  filter(postFireSAP < 0.05, ARI_end_smooth > 0.3) %>%
  dplyr::select(pixelID, FIRE_NAME, fireYear, x, y, ARI_end_smooth, postFireSAP, disturbanceSize, postFireConCov)

#st_write(st_as_sf(problematic_points, coords = c("x","y"), crs = 3310),"analysis4/problematic_points.shp")

#There is 

#wilderness meg red
df6_filter %>%
  ggplot(aes(fireYear,postFireSAP)) +
  geom_point() +
  geom_smooth(method = "lm")
  






