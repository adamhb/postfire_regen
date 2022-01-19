
source('00_setup.R')

fromFile <- T
makeFigs <- F
###########################
if(fromFile == T){
  path <- "~/cloud/gdrive/fire_project/local_data/fromGEE/"
  outpath <- "~/cloud/gdrive/fire_project/local_data/"
  figuresPath <- '~/cloud/gdrive/fire_project/figures/'
  patchLevelTimeVarying <- read_csv(paste0(path,'fromR_patchLevelTimeVarying_2021-05-26_05-20-05.csv'),
                                    col_types = cols(.default = "?", patchID = "c"))
  PatchesIN <- patchLevelTimeVarying$patchID %>% unique()
}else{
  source('01_calculateVariables.R')
}


################################################################
##visualizing variation in patch-level recovery trajectories ###
################################################################

#get the mean recovery trajectory of all patches
meanRecoveryOfAllPatches <- patchLevelTimeVarying %>%
  group_by(timeSinceFire) %>%
  summarise(RRI = mean(RRI))

#visualize recovery trajectories of all patches and the mean across patches
recoveryTrajsFig <- patchLevelTimeVarying %>%
  ggplot(aes(timeSinceFire,RRI,color = patchID)) +
  geom_line() +
  geom_line(data = meanRecoveryOfAllPatches, mapping = aes(timeSinceFire,RRI), color = "black", size = 3) +
  adams_theme +
  theme(legend.position = "none")

makePNG(fig = recoveryTrajsFig,path_to_output.x = figuresPath,file_name = "recoveryTraj_noStrat")

######################################################################################
#visualize recovery trajectories stratified by postFire precip and seed availability##
######################################################################################
SAP_quantiles <- quantile(patchLevelTimeVarying$postFireSAP, probs = c(0.2,0.8))
PPT_quantiles <- quantile(patchLevelTimeVarying$pptYr0_3_sum, probs = c(0.2,0.8))


recoveryTrajsStratFig <- patchLevelTimeVarying %>%
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
  ggplot(aes(timeSinceFire,RRI,color = patchID,linetype = planted)) +
  geom_line() +
  facet_grid(rows = vars(SeedAvail2), cols = vars(PostFirePPT2),drop = FALSE) +
  scale_colour_discrete(guide = FALSE) +
  theme_minimal()
#theme(legend.position = "none")

makePNG(fig = recoveryTrajsStratFig,path_to_output.x = figuresPath,file_name = "recoveryTrajsStratFig",res = 600)



################################################################
#############creat new time-invariant variables here############
################################################################









#############################################################
################histograms of time-invariant variables#######
#############################################################

patchLevelTimeInvariant <- patchLevelTimeVarying %>%
  select_if(function(col) length(unique(col)) <= length(PatchesIN)) %>%
  group_by(patchID) %>%
  summarise_all(.funs = mean)

cols <- patchLevelTimeInvariant %>%
  dplyr::select(-fireYear,-wilderness,-patchID,-focalAreaID,-timeSinceFire,-year) %>%
  colnames()


histograms <- ggplot(gather(patchLevelTimeInvariant %>% 
                  dplyr::select(-fireYear,-wilderness,-patchID),
                cols,value),mapping = aes(x = value)) +
  geom_dotplot(aes(y = ..density..),method = "histodot") +
  #geom_histogram(aes(y = ..density..)) +
  geom_density() +
  facet_wrap(.~cols, scales = "free",nrow = 3) +
  theme_minimal()

makePNG(fig = histograms,
        path_to_output.x = figuresPath,
        file_name = "histograms_timeInvariantVars",
        res = 100, width = 20, height = 10)



#############################################
####exploring associations among variables###
#############################################

#correlation plot of all time-invariant variables
corr <- patchLevelTimeInvariant %>% select_at(.vars = cols) %>% cor() %>% round(1)
Pmat <- patchLevelTimeInvariant %>% select_at(.vars = cols) %>% cor_pmat()
corrPlot <- ggcorrplot(corr, hc.order = TRUE, type = "full",
           lab = TRUE, p.mat = Pmat) +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x.bottom  = element_text(angle = 45, hjust=1))
makePNG(fig = corrPlot,
        path_to_output.x = figuresPath,
        file_name = "corrPlot",
        res = 100, width = 10, height = 10)


###########################################################
######MAKE SCATTER PLOTS HERE OF VARS THAT ARE CORRELATED##
###########################################################

#planting vs. RRI at Year 20
patchLevelTimeInvariant %>%
  ggplot(aes(postFirePlanting * 100, RRI_yr20)) +
  geom_point() +
  geom_smooth(method = "lm", se = FALSE, color = "red", lty = "dashed") +
  #geom_smooth(se = FALSE, lty = "dashed") +
  ylab(label = "RRI at Year 20") +
  xlab(label = "% of Patch Planted After Fire") +
  adams_theme

print(summary_stats(patchLevelTimeInvariant))




