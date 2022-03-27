
#source('00_setup.R')
#source('00_validate_classifier.R')

fromFile <- T
makeFigs <- T
cleanedData <- 'analysisReadyDF.csv'
###########################

if(fromFile == T){
  df2 <- read_csv(paste0(data_path,cleanedData))
}else{
  source('01_calculateVariables.R')
}


###################################
#calculating additional variables##
###################################
endYr <- 2017 #final year of data
print("focalAreas:")
unique(df2$focalAreaID)


#add extra vars
df2$conDominant <- df2$conCov > 0.5 #adding if conifers are dominant
df2$recoveryTrajLength <- endYr - df2$fireYear #adding recovery length


str(df2)


hist(df2$disturbanceSize)

df2 %>%
  filter(disturbanceSize > 0.2) %>%
  ggplot(aes(timeSinceFire,conCov,color = pixelID)) +
  geom_line() +
  theme_minimal()















makeRRIatEndOfTraj <- function(patch = df3$patchID[1]){
  fireYear <- df3[df3$patchID == patch,]$fireYear[1]
  recoveryTrajLength <- df3[df3$patchID == patch,]$recoveryTrajLength[1]
  finalYrofTraj <- fireYear + recoveryTrajLength
  RRIFinalYear <- df3[df3$patchID == patch & df3$year == finalYrofTraj,]$RRI
  RR_per_year <- RRIFinalYear / recoveryTrajLength
  output <- tibble(pixelID = patch,
                   finalYrofTraj = finalYrofTraj,
                   RRIFinalYear = RRIFinalYear,
                   RR_per_year = RR_per_year)
  return(output)
}
makeRRIatEndOfTraj()

df4 <- df3 %>% 
  left_join(map_df(.x = unique(df3$pixelID)[1:1000], .f = makeRRIatEndOfTraj),by = "pixelID")


df3 %>% filter(year == 2016) %>% 
  ggplot(mapping = aes(RRI)) +
           geom_histogram(binwidth = 0.1)

##########################
####basic info############
##########################

#sample size per fire year
n_patch_per_fire_year <- distinct(df3,patchID,.keep_all = T) %>% pull(fireYear) %>% table()
print("fireYears:")
print(n_patch_per_fire_year)
n_per_recovery_length <- distinct(df3,patchID,.keep_all = T) %>% pull(recoveryTrajLength) %>% table()
n_per_recovery_length
#1987 and 1999 are the two most represented fire years

################################################################
##visualizing variation in patch-level recovery trajectories ###
################################################################




#get the mean recovery trajectory of all patches
meanRecoveryOfAllPatches <- df3 %>%
  group_by(timeSinceFire) %>%
  summarise_at(.vars = c("RRI","ConProb","pctCovCon"), .funs = function(x){mean(x,na.rm = T)})

#plot mean recovery over all patches
RRI_mean_allPatches <- ggplot(data = meanRecoveryOfAllPatches, mapping = aes(timeSinceFire,ConProb)) +
  geom_line() +
  #geom_line(data = meanRecoveryOfAllPatches, mapping = aes(timeSinceFire,RRI), color = "black", size = 3) +
  adams_theme +
  theme(legend.position = "none")
RRI_mean_allPatches 

#visualize RRI of all patches and the mean across patches


pixels <- unique(df3$pixelID)
rows <- as.integer(runif(n = 10, min = 1, max = length(pixels)))
pixel_sample <- pixels[rows]



#add rolling mean
test <- df3 %>%
  filter(pixelID == "11_263") 
  
roll <- roll_mean(test$ConProb, n = 3, align = "right", fill = NA)
rolldf <- tibble(test$timeSinceFire)

recoveryTrajsFig <- df3 %>%
  #filter(pixelID == "11_263") %>%
  filter(preFireConProb > 0.5) %>%
  ggplot(aes(timeSinceFire,RRI,color = pixelID)) +
  #geom_line() +
  geom_line(aes(y=rollmean(RRI, 7, na.pad=TRUE))) +
  #geom_line(data = meanRecoveryOfAllPatches, mapping = aes(timeSinceFire,RRI), color = "black", size = 3) +
  scale_y_continuous(limits = c(-0.3,1.2)) +
  scale_x_continuous(limits = c(0,35)) +
  adams_theme +
  theme(legend.position = "none")
recoveryTrajsFig

#visualize ConProb
recoveryTrajsFigConProb <- df3 %>%
  ggplot(aes(timeSinceFire,ConProb,color = patchID)) +
  geom_line() +
  scale_y_continuous(limits = c(0,100)) +
  scale_x_continuous(limits = c(0,35)) +
  adams_theme +
  theme(legend.position = "none")
recoveryTrajsFigConProb

#visualize pct cover
recoveryTrajsPctCovCon <- df3 %>%
  filter(year < 2017) %>%
  ggplot(aes(timeSinceFire,pctCovCon,color = patchID)) +
  geom_line() +
  scale_y_continuous(limits = c(0,0.53)) +
  scale_x_continuous(limits = c(0,35)) +
  adams_theme +
  theme(legend.position = "none")
recoveryTrajsPctCovCon


#what is the pctCover at the end of the trajectories?
pctCovConEnd <- ggplot(data = df3 %>% filter(year == 2016), mapping = aes(x = pctCovCon)) +
  geom_histogram() +
  xlab("Conifer Cover (%)") +
  labs(title = "%Pct Conifer Cover in 2016") +
  ylab(label = "N Patches") +
  adams_theme
#all plots have at least some conifer recovery


#distribution of pre-fire conProb
pctCovConEnd <- ggplot(data = df3 %>% filter(year == 2016), mapping = aes(x = preFireConProb)) +
  geom_histogram() +
  xlab("PreFireConProb") +
  labs(title = "PreFireConProb") +
  ylab(label = "N Patches") +
  adams_theme


######################################################################################
#visualize recovery trajectories stratified by postFire precip. and seed availability##
######################################################################################
SAP_quantiles <- quantile(df3$postFireSAP, probs = c(0.33,0.66), na.rm = T)
PPT_quantiles <- quantile(df3$pptYr0_3_sum, probs = c(0.33,0.66), na.rm = T)

recoveryTrajsStratFigDF <- df3 %>%
  filter(postFirePlanting > 0.1, burnSev < 5, burnSev > 2.5, year < 2017) %>%
  mutate(SeedAvail = case_when(
    postFireSAP <= SAP_quantiles[1] ~ "low seed avail.",
    postFireSAP >= SAP_quantiles[2] ~ "high seed avail.",
    (postFireSAP > SAP_quantiles[1]) & (postFireSAP < SAP_quantiles[2]) ~ "medium seed avail."
  )) %>%
  mutate(PostFirePPT = case_when(
    pptYr0_3_sum <= PPT_quantiles[1] ~ "low ppt Yrs 0-3",
    pptYr0_3_sum >= PPT_quantiles[2] ~ "high ppt Yrs 0-3",
    (pptYr0_3_sum > PPT_quantiles[1]) & (postFireSAP < PPT_quantiles[2]) ~ "medium ppt Yrs 0-3"
  )) %>% mutate(SeedAvail2 = factor(SeedAvail,levels = c("high seed avail.","medium seed avail.","low seed avail."))) %>%
  mutate(PostFirePPT2 = factor(PostFirePPT,levels = c("high ppt Yrs 0-3","medium ppt Yrs 0-3","low ppt Yrs 0-3"))) %>%
  mutate(planted = case_when(
    postFirePlanting >= 0.5 ~ TRUE,
    postFirePlanting < 0.5 ~ FALSE
  )) %>%
  ggplot(aes(timeSinceFire,ConProb,group = patchID, color = burnSev)) +
  geom_line(size = 1) +
  #geom_smooth(data = recoveryTrajsStratFigDF, mapping = aes(x = timeSinceFire, y = RRI, group = planted))
  facet_grid(rows = vars(SeedAvail2), cols = vars(PostFirePPT2),drop = FALSE) +
  #scale_colour_discrete(guide = FALSE) +
  scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 3) +
  scale_y_continuous(limits = c(0,1)) +
  scale_x_continuous(limits = c(-2,35)) +
  labs(title = " < 10% of patch planted in first 6 years after fire \n RRI") +
  theme_minimal()


makePNG(fig = recoveryTrajsStratFig,path_to_output.x = figuresPath,file_name = "RRI-Trajectory-NotPlanted",res = 600)

################################################################
#############create new time-invariant variables here############
################################################################



#############################################################
################histograms of time-invariant variables#######
#############################################################
patchLevelTimeInvariant <- df3 %>%
  select_if(function(col) length(unique(col)) <= length(PatchesIN)) %>%
  select(-ConDom) %>%
  group_by(patchID) %>%
  summarise_all(.funs = mean) %>%
  mutate(planted = postFirePlanting > 0.25) %>%
  mutate(plantedNamed = case_when(
    planted == TRUE ~ "planted",
    planted == FALSE ~ "notPlanted"
  )) 
  
  

cols <- patchLevelTimeInvariant %>%
  dplyr::select(-fireYear,-wilderness,-patchID,-focalAreaID,-year,-timeSinceFire,
                -fireYear,-ConDom,-recoveryTrajLength,-year) %>% colnames()


histograms <- ggplot(gather(patchLevelTimeInvariant %>% 
                  dplyr::select(-fireYear,-wilderness,-patchID,-year),
                cols,value),mapping = aes(x = value)) +
  #geom_histogram() +
  #geom_dotplot(aes(y = ..density..),method = "histodot") +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(.~cols, scales = "free",nrow = 3) +
  theme_minimal()

histograms

# makePNG(fig = histograms,
#         path_to_output.x = figuresPath,
#         file_name = "histograms_timeInvariantVars",
#         res = 100, width = 20, height = 10)

#############################################
####exploring associations among variables###
#############################################
#correlation plot of all time-invariant variables
corr <- patchLevelTimeInvariant %>% select_at(.vars = cols) %>% select(-CWDhist) %>% cor() %>% round(1)
Pmat <- patchLevelTimeInvariant %>% select_at(.vars = cols) %>% select(-CWDhist) %>% cor_pmat()

corrPlot <- ggcorrplot(corr, hc.order = TRUE, type = "full",
           lab = TRUE, p.mat = Pmat) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x.bottom  = element_text(angle = 45, hjust=1))



#exploring the relationships between RRI_per_year and driver vars

vars <- names(df3)[c(7,8,9,10,11,13,14,15,16,17,18,19,20,21,22,23,24)]
vars %in% names(patchLevelTimeInvariant)

varOfInterest <- vars[2]
makeCorrPlot <- function(varOfInterest = vars[1]){
  
  plot <- patchLevelTimeInvariant %>% 
    filter(RR_per_year < 0.2) %>%
    filter(burnSev < 5) %>%
    ggplot(aes_string(varOfInterest,"RR_per_year",color = "burnSev")) +
    geom_point() +
    geom_smooth(method = "lm") +
    scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 3) +
    facet_grid(~plantedNamed) +
    labs(title = varOfInterest) +
    adams_theme
  
  makePNG(fig = plot, path_to_output.x = paste0(figuresPath,"correlations/"), file_name = varOfInterest)
  #return(plot)
  
}

#makeCorrPlot(varOfInterest = vars[2])

patchLevelTimeInvariant %>%
  ggplot(aes(x = RR_per_year)) +
  geom_histogram() + 
  scale_x_continuous(limits = c(0,0.15)) +
  adams_theme

map(.x = vars, .f = makeCorrPlot)






# makePNG(fig = corrPlot,
#         path_to_output.x = figuresPath,
#         file_name = "corrPlot",
#         res = 100, width = 10, height = 10)








###########################################################
######MAKE SCATTER PLOTS HERE OF VARS THAT ARE CORRELATED##
###########################################################

#planting vs. RRI at Year 20
# patchLevelTimeInvariant %>%
#   ggplot(aes(postFirePlanting * 100, RRI_yr20)) +
#   geom_point() +
#   geom_smooth(method = "lm", se = FALSE, color = "red", lty = "dashed") +
#   #geom_smooth(se = FALSE, lty = "dashed") +
#   ylab(label = "RRI at Year 20") +
#   xlab(label = "% of Patch Planted After Fire") +
#   adams_theme






