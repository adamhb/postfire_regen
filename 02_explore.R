source('00_setup.R')

end_recovery_period <- 2019
#analysisTimeSinceFire <- 24
min_drop_in_pct_cover <- 0.20
min_preFire_conCov <- 0.30
fromFile <- T
makeFigs <- T
#cleanedData <- 'analysisReadyDF_4_1_2022.csv'
cleanedData <- 'analysisReadyDF_4_1_2022.csv'
###########################

if(fromFile == T){
  df2 <- read_csv(paste0(data_path,cleanedData))
  
}else{
  source('01_calculateVariables.R')
}


#load snowpack
snowPack <- read_csv(paste0(data_path,'snowPack4_2_2022.csv')) %>%
  select(x,y,snowPack,pixelID) %>% rename(pixelIDsnow = pixelID)


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
######################################
#NO MORE CHANGES TO DATA AFTER THIS###
######################################

##########################
####basic info############
##########################
print('focalAreas:')
print(unique(df2$focalAreaID))
focalAreasIn <- unique(df2$focalAreaID)
print("focal area n:")
print(length(unique(df2$focalAreaID)))


studyPeriod <- c(min(df2$year),max(df2$year))
print('studyPeriod')
print(studyPeriod)

#pixels per fire
print("pixels per fire:")
print(df3 %>% group_by(fire) %>% summarise(n = length(unique(pixelID))))

forSiteLevelAnalysis <- df3 %>% group_by(fire) %>% summarise(n = length(unique(pixelID))) %>% filter(n > 30) %>% pull(fire)

#pixel ids
pixelIDs <- unique(df3$pixelID)

#distribution over landfire forest types
df4 %>% ggplot(aes(forestType)) +
  geom_histogram(stat = "count") +
  theme_minimal()

####################
####functions#######
####################
makeDefaultHistogram <- function(d,var){
  #range <- summary(d %>% pull(symvar))
  d %>% 
    ggplot(aes_string(var)) +
    geom_histogram(binwidth = 0.05) +
    xlab(var) +
    labs(title = var) +
    ylab(label = "N Pixels") +
    theme_minimal()
}

getCorrelationMatrix <- function(d, varsForCorr = topoVars){
  corr <- d %>% select_at(.vars = varsForCorr) %>% cor() %>% round(1)
  Pmat <- d %>% select_at(.vars = varsForCorr) %>% cor_pmat()
  corrPlot <- ggcorrplot(corr, hc.order = TRUE, type = "lower",
                         lab = TRUE, p.mat = Pmat) +
    theme_minimal() +
    theme(legend.position = "none",
          axis.text.x.bottom = element_text(angle = 45, hjust=1))
  return(corrPlot)
}

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

fitGAM <- function(d,preds,response = "RRI_end", scaleData = T, includeXY = F, randomEffect = F){
  
  v <- c()
  for(p in preds){
    unique_vals <- d %>% pull(p) %>% unique() %>% length()
    if(unique_vals < 10){
      k <- unique_vals-1
      v <- append(v,paste0("s(",p,",k=",k,")"))
    }else{
      v <- append(v,paste0("s(",p,")"))
    }
  }
  
  if(randomEffect == T){
    v <- c(v,"s(fire,bs='re')") 
  }
  
  if(includeXY == T){
    v <- c(v,"s(x,y") 
  }
  
  X <- paste(v, collapse = "+")
  Y <- response
  form <- as.formula(paste(Y,"~",X))
  print(form)
  
  if(scaleData == T){
    forMod <- d %>%
      dplyr::select(response,x,y,all_of(preds)) %>%
      mutate(across(where(is.numeric),scale_this))
  }else{
    forMod <- d %>%
      dplyr::select(response,all_of(preds))
  }
  
  mod <- gam(formula = form, data = forMod) 
  return(mod)
}


#predictor vars
topoVars <- c("hli","tpi","northness","eastness","adjustedNorthness","adj_fold_northness","slope","elevation")
histClimateVars <- c("AEThist","CWDhist","TmaxHist","PPThist","PPThistSD","TmaxHistSD")
seedVars <- c("preFireConCov","disturbanceSize","burnSev","postFireConCov","postFireSAP","postFirePlanting")
postFireWeatherVars <- c("pptYr1_3_annual_mean","ppt_Yr1_3_mean_annual_anom",
                         "min_annual_ppt_Yr1_3", "min_annual_ppt_Yr1_3_anom",
                         "pptYr1_3_annual_mean_t_adjusted",
                         "min_annual_ppt_t_adjusted_Yr1_3")
otherVars <- c("x","y","wilderness","forestType","mediumReburns","lowReburns","recoveryTime","fire")

#getCorrelationMatrix(mod3_predictors)
# forGGPairs <- c("RRI_end",all_of(mod3_predictors))
# df4 %>% select(forGGPairs) %>% ggpairs()


#################
#working model###
#################
#vars to include
otherVars.x <- c("recoveryTime","mediumReburns","lowReburns","fire")
topoVar.x  <- c("slope","elevation","adjustedNorthness")
histClimateVars.x <- c("PPThist")
seedVars.x <- c("burnSev","postFireSAP","postFirePlanting")
postFireWeather.x <- c("ppt_Yr1_3_mean_annual_anom")
predictors.x <- c(otherVars.x,topoVar.x,histClimateVars.x,seedVars.x,postFireWeather.x)

getCorrelationMatrix(varsForCorr = c("recoveryTime","slope","elevation","hli","AEThist","postFireSAP","pptYr1_3_annual_mean","burnSev"))



modData <- df4 %>%
  mutate(across(where(is.numeric),scale_this)) 

k.default = 15

mod <- gam(formula = RRI_end ~ s(recoveryTime, k = 6) + s(fire, bs="re") + s(x,y,k = 100) +
             s(mediumReburns, k = k.default) +
             s(burnSev, k = k.default) +
             s(slope, k = k.default) + s(elevation, k = k.default) + s(hli, k = k.default) + 
             s(CWDhist, k = k.default) +
             s(AEThist, k = k.default) +
             s(postFireSAP, k = k.default) + 
             s(pptYr1_3_annual_mean, k = k.default),
           data = modData)

summary(mod)
gam.check(mod)
plot(mod)
################################################################
##visualizing variation in patch-level recovery trajectories ###
################################################################

# df3 %>%
#   ggplot(aes(timeSinceFire,RRI,color = pixelID)) +
#   geom_line() +
#   theme_minimal()

#visualize recovery trajectories stratified by postFire precip. and seed availability#

SAP_quantiles <- quantile(df3$postFireSAP, probs = c(0.33,0.66), na.rm = T)
PPT_quantiles <- quantile(df3$pptYr1_3_annual_mean, probs = c(0.33,0.66), na.rm = T)

recoveryTrajsStratFigDF <- df3 %>%
  #filter(conCov_end < 0.1) %>%
  mutate(SeedAvail = case_when(
    postFireSAP <= SAP_quantiles[1] ~ "low seed avail.",
    postFireSAP >= SAP_quantiles[2] ~ "high seed avail.",
    (postFireSAP > SAP_quantiles[1]) & (postFireSAP < SAP_quantiles[2]) ~ "medium seed avail."
  )) %>%
  mutate(PostFirePPT = case_when(
    pptYr1_3_annual_mean <= PPT_quantiles[1] ~ "low post-fire precip.",
    pptYr1_3_annual_mean >= PPT_quantiles[2] ~ "high post-fire precip.",
    (pptYr1_3_annual_mean > PPT_quantiles[1]) & (pptYr1_3_annual_mean < PPT_quantiles[2]) ~ "medium post-fire precip."
  )) %>% mutate(SeedAvail2 = factor(SeedAvail,levels = c("high seed avail.","medium seed avail.","low seed avail."))) %>%
  mutate(PostFirePPT2 = factor(PostFirePPT,levels = c("high post-fire precip.","medium post-fire precip.","low post-fire precip."))) %>%
  mutate(planted = case_when(
    postFirePlanting >= 0.3 ~ TRUE,
    postFirePlanting < 0.3 ~ FALSE
  ))

 meanRecov <- recoveryTrajsStratFigDF %>% filter(SeedAvail2 == "high seed avail.",
                                                 PostFirePPT == "high post-fire precip.") %>%
   ggplot() +
   geom_line(mapping = aes(timeSinceFire,RRI,group = pixelID, color = burnSev, linetype = planted)) +
   scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 3) +
   scale_y_continuous(limits = c(-0.2,1.2)) +
   scale_x_continuous(limits = c(-2,35)) +
   labs(title = "XYZ") +
   theme_minimal()
   
 recoveryTrajsStratFigDF %>%
   group_by(timeSinceFire,SeedAvail2,PostFirePPT2) %>%
   summarise(RRI_mean = RRI) %>% distinct() %>% group_by(SeedAvail2,PostFirePPT2,timeSinceFire) %>% 
   summarise(RRI_mean2 = RRI_mean) %>% print(n = 200)
   
recoveryTrajsStratFigDF %>%
  ggplot() +
  geom_line(mapping = aes(timeSinceFire,RRI,group = pixelID, color = burnSev, linetype = planted)) +
  #ggplot(aes(timeSinceFire,conCov,group = pixelID, color = burnSev)) +
  #geom_line(size = 1) +
  #geom_line(data = meanRecov, mapping = aes(timeSinceFire,conCov_mean), color = "black", size = 2) +
  facet_grid(rows = vars(SeedAvail2), cols = vars(PostFirePPT2),drop = FALSE) +
  scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 3) +
  scale_y_continuous(limits = c(-0.2,1.2)) +
  scale_x_continuous(limits = c(-2,35)) +
  labs(title = "XYZ") +
  theme_minimal()


####histograms of:
#response vars
#1. conCov_end
df4 %>%
  ggplot(aes(conCov_end)) +
  geom_histogram(binwidth = 0.05) +
  xlab("Conifer Cover (%)") +
  scale_x_continuous(limits = c(0,1), breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(title = "% Conifer Cover in 2019") +
  ylab(label = "N Pixels") +
  theme_minimal()

#2. ARI_end
df4 %>%
  ggplot(aes(RRI_end)) +
  geom_histogram(binwidth = 0.05) +
  xlab("RRI at end of trajectories") +
  #scale_x_continuous(limits = c(0,1), breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(title = "RRI at end of trajectories") +
  ylab(label = "N Pixels") +
  theme_minimal()

#3. RRI_end
df4 %>%
  ggplot(aes(ARI_end)) +
  geom_histogram(binwidth = 0.05) +
  xlab("ARI at end of trajectories") +
  scale_x_continuous(limits = c(0,1), breaks = seq(from = 0, to = 1, by = 0.05)) +
  labs(title = "ARI at end of trajectories") +
  ylab(label = "N Pixels") +
  theme_minimal()

###postfire planting
postFirePlantingHist <- df4 %>%
  ggplot(aes(postFirePlanting)) +
  geom_histogram(binwidth = 0.05) +
  xlab("The fraction of each pixel planted \n in the first 6 yrs after fire") +
  #scale_x_continuous(limits = c(0,1), breaks = seq(from = 0, to = 1, by = 0.05)) +
  #labs(title = "ARI at end of trajectories") +
  ylab(label = "N Pixels") +
  theme_minimal()

makePNG(fig = postFirePlantingHist, path_to_output.x = fig_path,file_name = "postFirePlantHist")



###postfire pptyr1-3
PrecipHist <- df4 %>%
  ggplot(aes(PPThist)) +
  geom_histogram(binwidth = 60, color = "black") +
  xlab(expression(paste("Mean annual precipitation [mm yr"^"-1","]"))) +
  #scale_y_log10() +
  #scale_y_continuous(limits = c(0,2500)) +
  #labs(title = "ARI at end of trajectories") +
  scale_x_continuous(breaks = c(0,1000,2000,3000), limits = c(0,3000)) +
  ylab(label = "N pixels") +
  scale_y_continuous(limits = c(0,400)) +
  adams_theme

#makePNG(fig = PrecipHist, path_to_output.x = fig_path,file_name = "PrecipHist", height = 3.5, width = 4)

THist <- df4 %>%
  ggplot(aes(TmaxHist)) +
  geom_histogram(binwidth = 0.3, color = "black") +
  xlab(expression(paste("Mean daily ","maximum temp. ["*degree*C,"]"))) +
  #scale_y_log10() +
  #scale_y_continuous(limits = c(0,2500)) +
  #labs(title = "ARI at end of trajectories") +
  ylab(label = "N pixels") +
  scale_x_continuous(breaks = c(10,15,20,25), limits = c(10,25)) +
  scale_y_continuous(limits = c(0,400)) +
  adams_theme

#makePNG(fig = THist, path_to_output.x = fig_path,file_name = "THist", height = 3.5, width = 4)


climate_inserts <- plot_grid(PrecipHist,THist, labels = c("(a)","(b)"), label_x = 0.2)
makePNG(fig = climate_inserts, path_to_output.x = fig_path,file_name = "climate_inserts", height = 3.5, width = 8)



df4 %>%
  ggplot(aes(PPThist)) +
  geom_histogram() +
  xlab("Post-fire precipitation in first 3 yrs \n after fire [mm yr-1]") +
  #scale_x_continuous(limits = c(0,1), breaks = seq(from = 0, to = 1, by = 0.05)) +
  #labs(title = "ARI at end of trajectories") +
  ylab(label = "N Pixels") +
  theme_minimal()



#######################################################
####exploring correlations among predictor variables###
#######################################################

terrainCorr <- getCorrelationMatrix()
histClimateCorr <- getCorrelationMatrix(histClimateVars)
postFireWeatherCorr <- getCorrelationMatrix(postFireWeatherVars)
seedVarsCorr <- getCorrelationMatrix(seedVars)



#histograms of topo vars, histClimate vars, seed vars and post fire weather vars
#varsForHist <- c(topoVars,histClimateVars,seedVars,postFireWeatherVars,otherVars)

# predictorHist <- df4 %>%
#   select(varsForHist) %>% 
#   gather(key = "var", value = "value", c(varsForHist)) %>%
#   ggplot(aes(value)) +
#   geom_histogram(aes(y = ..density..)) +
#   geom_density() +
#   facet_wrap(.~var, scales = "free",nrow = 3) +
#   theme_minimal()
# makePDF(fig = predictorHist, file_name = 'predictorVarsHist',height = 10, width = 15)

#############################################################
##########exploring correlations between pred and response###
#############################################################
medianSAP <- df3 %>%
  #filter(postFirePlanting < 0.1, recoveryTime == 32, focalAreaID == 10) %>%
  pull(postFireSAP) %>% median()

df4 %>% group_by(fire) %>%
  #filter(RRI_end < 1) %>%
  filter(postFirePlanting < 0.01, mediumReburns < 0.01) %>%
  summarise_at(.vars = c(predictors.x[-4],'x','y','RRI_end','pptYr1_3_annual_mean','ARI_end','CWDhist'),.funs = mean)%>%
  arrange(RRI_end) %>%
  #filter(pptYr1_3_annual_mean < 3000) %>%
  ggplot(aes(recoveryTime,RRI_end,color = burnSev)) +
  geom_point(size = 4) +
  #geom_smooth(method = "lm") +
  theme_minimal()


df4 %>%
  filter(fire != "15-1992") %>%
  ggplot(aes(postFireSAP, conCov_end)) +
  geom_point() +
  #facet_wrap(~fire) +
  geom_smooth(method = "lm") +
  theme_minimal()


df3 %>%
  filter(postFirePlanting < 0.1, recoveryTime == 32, focalAreaID == 6) %>%
  ggplot(aes(timeSinceFire,ARI, group = pixelID, color = postFireSAP)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = 0:20) +
  #scale_y_continuous(limits = c(0,2)) +
  scale_colour_gradient2(low = "red", mid = "orange", high = "blue", midpoint = medianSAP) +
  theme_minimal() +
  theme(legend.position = "none")

df4 %>%
  ggplot(aes(burnSev,postFireSAP)) +
  geom_point() +
  geom_smooth(method = "lm") +
  theme_minimal()

#######################################################
############exploring key study domain variables#######
#######################################################

makeDefaultHistogram(df4,"mediumReburns")
makeDefaultHistogram(df4,"lowReburns")
makeDefaultHistogram(df4,"postFirePlanting")

#584 & 551: Mediterranean California Dry-Mesic Mixed Conifer Forest and Woodland
#585 & 552: Mediterranean California Mesic Mixed Conifer Forest and Woodland
#588: California Montane Jeffrey Pine(-Ponderosa Pine) Woodland
#587: Mediterranean California Lower Montane Black Oak-Conifer Forest and Woodland
#549: Klamath-Siskiyou Lower Montane Serpentine Mixed Conifer Woodland
#550: Klamath-Siskiyou Upper Montane Serpentine Mixed Conifer Woodland
#577: Inter-Mountain Basins Sparsely Vegetated Systems (filtered out)
#589: Mediterranean California Red Fir Forest - Cascades
#2701: unk.

###################################################
####################fit models#####################
###################################################

#model to do
#1. spatial auto corr.
#2. plot output of relationships (partial residual plots)
#3. play with filtering / grouping by eco type, reburns, replanting
#4. fit models to specific fires to explore SAP, explore SAP var in GEE for a fire, try SAP 150 to see if it improves the models
#confirm that the "partial dependence plots" are what I'm looking / should be looking at for gam var relationships

####SAP issue
# variable seems to be working well; perhaps relationship is not as strong because I focused on areas of clumps of pixels, so the lone pixels are not as represented.gc()
#can fit to other dependent vars like con cov
#add / find interactions
#add a random effect of fire or fire X focal area
#try fitting the gam to the timeseries


makeDefaultHistogram(d = df4, var = "postFirePlanting")
