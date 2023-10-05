##Issues

#1. how do you decide on k:: one larger than effective degrees of freedom
#1. how do you check for spatial autocorrelation?: semivariogram (looks like 1,000 m should be cutoff; try spatial averaging instead of spatially rarifying if the sample size gets too low at 1,000 m)
#1. how do you interpret output of interaction term?: 3d box plot (this looks good actually for post-fire precip. and SAP)
#1. gamm or gam? (use GAM because not using random effects)
#2. adding x,y changes the variable relationships to a point that doesn't really make sense (maybe just don't include xy)
#3. Random effect in gams? (don't use them, like don't use a random effect for fire because it takes over what you're trying to predict)
#4. Model selection approach? (include many variables that are not correlated; )


#To try next / all at the same time
#rarify to 1,000 m and re-fit this test model; if doesn't work then try spatially averaging to 1,000 meters
#broader model selection / testing of other variables (and check spatial auto correlation with those); see if semi-variogram is the same for other models or if there is perhaps less spatial auto
#try with NF (not-planted) domain 


source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')
df6 <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/modelFittingData_030623.csv')
notPlanted <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/not_planted_040323.csv') #extra layer of certainty that these pixels were not planted because there is a buffer around the facts data
library(sf)
library(sp)

spatial_rare_pixel_list <- list()
dists <- c(200,600,1000,1500,2000,3000,5000,10000,20000,50000)
for(dist in dists[3]){
  f = paste0('~/cloud/gdrive/fire_project/postfire_regen/analysis4/spat_rare_pixels_',as.character(dist),".csv")
  sample_pixels <- read_csv(f)
  print(paste("sample size at",dist,"is",nrow(tmp)))
}

nf_pixels <- df6 %>%
  #filter(wilderness == 1) %>%
  filter(pixelID %in% notPlanted$pixelID) %>%
  filter(pixelID %in% sample_pixels$pixelID) #%>% #spatial rarification
  #filter(FIRE_NAME == "MEGRAM")

#export to look at pixel domain spatially
nf_pixels %>% dplyr::select(pixelID,x,y) %>%
  st_as_sf(x = .,coords = c("x","y"),crs = "EPSG:3310") %>% st_write("analysis4/nf_pixels.shp")

wilderness_pixels <- df6 %>%
  filter(wilderness == 1) %>%
  #filter(pixelID %in% notPlanted$pixelID) %>%
  filter(pixelID %in% sample_pixels$pixelID)

wilderness_pixels %>% dplyr::select(pixelID,x,y) %>%
  st_as_sf(x = .,coords = c("x","y"),crs = "EPSG:3310") %>% st_write("analysis4/wild_pixels.shp")

#set which data set to work with
df6_filter <- wilderness_pixels

modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))

print(paste("sample size:",nrow(modData)))

mod <- gam(formula = ARI_end_smooth ~ 
             s(recoveryTime, k = 6) + 
             #s(x, y, bs = "gp", k = 48, m = 2) +
             s(mediumReburns, k = 6) +
             s(burnSev, k = 6) +
             #s(TmaxHist, k = 15) +
             s(adj_fold_northness, k = 6) +
             #s(postFireSAP, k = 6) +
             #s(postFirePrecip, k = 6)
             te(postFireSAP,postFirePrecip, k = 6),
           data = modData, select = F, method = "REML")

#model results
summary(mod)
plot(mod)
paste0("AIC:",AIC(mod))

#see interaction
vis.gam(mod, view=c('postFireSAP', 'postFirePrecip'), n.grid=50, theta=-90, phi=10, zlab="", too.far=0.1)+title("SAP by Post-fire Precip")


#check gam
gam.check(mod)

#Moran's I
#pixel.dists <- as.matrix(dist(cbind(df6_filter$x, df6_filter$y)))
#pixel.dists[1:5,1:5]
# pixel.dists.inv <- 1/pixel.dists
# diag(pixel.dists.inv) <- 0
# print(Moran.I(residuals(mod),pixel.dists.inv))

#semivariogram
residual_df <- tibble(x = df6_filter$x, y = df6_filter$y, res = residuals(mod))
coordinates(residual_df) = ~x+y
plot(variogram(res~1, residual_df, cutoff = 2000, width = 100))




#plot residuals geographically
residuals_tibble <- tibble(x = df6_filter$x, y = df6_filter$y, res = residuals(mod))
res_tib_spat <- st_as_sf(residuals_tibble, coords = c("x","y"), crs = 3310)
plot(res_points_lat_lon)
#st_write(res_tib_spat,"~/cloud/gdrive/fire_project/postfire_regen/analysis4/res_tib_spat.shp")

