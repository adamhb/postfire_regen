source('~/cloud/gdrive/fire_project/postfire_regen/analysis1/00_setup_A1.R')
end_recovery_period <- 2019
#analysisTimeSinceFire <- 24
min_drop_in_pct_cover <- 0.20
min_preFire_conCov <- 0.30
fromFile <- T
makeFigs <- T
writeFile <- F

cleanedData <- 'analysisReadyDF_4_1_2022.csv'
###########################

if(fromFile == T){
  df2 <- read_csv(paste0(data_path,cleanedData))
  
}else{
  source('01_calculateVariables_A1.R')
}

length(unique(df2$pixelID))

#load snowpack
snowPack <- read_csv(paste0(data_path,'snowPack4_2_2022.csv')) %>%
  dplyr::select(x,y,snowPack,pixelID) %>% rename(pixelIDsnow = pixelID)


#############################################
#additional vars, filters, and cleaning #####
#############################################
#get conCov_end, ARI_end, and RRI_end and fire.
additionalVars <- df2 %>% 
  filter(year == end_recovery_period) %>% 
  #filter(timeSinceFire == analysisTimeSinceFire) %>%
  dplyr::select(pixelID,focalAreaID,fireYear,conCov,ARI,RRI) %>% 
  rename(conCov_end = conCov, ARI_end = ARI, RRI_end = RRI) %>%
  mutate(fire = paste0(focalAreaID,"-",fireYear)) %>%
  dplyr::select(-focalAreaID,-fireYear) 


df3 <- df2 %>%
  #add additional vars
  left_join(additionalVars, by = "pixelID") %>%
  left_join(snowPack, by = c("x","y")) %>%
  #cleaning
  mutate(across(forestType, as_factor)) %>% 
  mutate(across(focalAreaID, as_factor)) %>% 
  mutate(across(fire, as_factor)) %>% 
  #filters
  filter(disturbanceSize > min_drop_in_pct_cover,
         preFireConCov > min_preFire_conCov,
         forestType != 577, forestType != 2701)



timeVaryingVars <- c("year","timeSinceFire","conCov","ARI","RRI")
df4 <- df3 %>% dplyr::select(-all_of(timeVaryingVars)) %>% distinct()

if(writeFile == T){
  saveRDS(df4,"analysis1/A1_data.Rdata")
  write_csv(df4,"analysis1/A1_data.csv")
}
