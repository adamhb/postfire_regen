###############################
####Libraries and functions####
###############################
source('analysis2/00_setup_A2.R')

fromFile <- T
makeFigs <- T

########################
#Load data and clean####
########################

#This data is generated from python scripts
cleanedData <- 'analysis2/analysisData_102722.csv'

#clean
df2 <- read_csv(cleanedData)[-1] %>%
  mutate_at(.vars = c("mediumReburns","lowReburns"), .funs = ~replace(., is.na(.), 0)) %>%
  filter(disturbanceSize > 0.2, RRI_end < 3) %>% # add these filters to the methods
  mutate_at(.vars = c("burnSev","mediumReburns","lowReburns","wilderness","forestType","BurnSev_of_worstBurn2014_2019","CAUSE"), .funs = as.factor) %>%
  mutate_at(.vars = "OBJECTID", .funs = as.character)

###################
#pixels per fire###
###################
#This is to see how many valid pixels there are in each fire
#and to filter to fires that have > x pixels
pixels_per_fire <- read_csv('/home/adam/cloud/gdrive/fire_project/postfire_regen/analysis2/n_per_fire.csv')
#print(arrange(.data = pixels_per_fire, desc(n_per_fire)))

#filter to fires that only have more than 1000 pixels per fire
fires_to_include <- filter(pixels_per_fire, n_per_fire > 1000) %>% pull(FIRE_NAME)
df3 <- filter(df2, FIRE_NAME %in% fires_to_include) #RRI_end > 0)


##############################################
#add post-fire precip. anomalies##############
##############################################

df4 <- df3 %>% mutate(ppt_Yr1_3_mean_annual_anom = (postFirePrecip - PPThist) / PPThistSD,
                      pptYr1_3_annual_mean_t_adjusted = postFirePrecip / postFireMaxT)


#####################
#create grouped df##
#####################
#function to split vars into groups (4 quantiles)
get_var_groups <- function(d,var){
  print(var)
  group_var = paste0(var,"_group")
  quants <- quantile(as.numeric(d[var][[1]]), probs = c(0.25,0.5,0.75), na.rm = T)
  print(quants)
  out <- mutate(.data = d, !!group_var := case_when(
    !!sym(var) < quants[1] ~ "1st",
    !!sym(var) < quants[2] & !!sym(var) >= quants[1] ~ "2nd",
    !!sym(var) < quants[3] & !!sym(var) >= quants[2] ~ "3rd",
    !!sym(var) > quants[4] ~ "4th")
  )
  return(out)
}

#created grouped df
#can only summarize by 4 variables?
grouped_df <- df4 %>% 
  get_var_groups(var = "elevation") %>%
  get_var_groups(var = "postFirePrecip") %>%
  get_var_groups(var = "postFireSAP") %>%
  get_var_groups(var = "adj_fold_northness") %>%
  mutate_at(.vars = c("burnSev","mediumReburns","lowReburns"), .funs = function(x){as.numeric(as.character(x))}) %>%
  group_by(burnSev, postFirePrecip_group, postFireSAP_group, adj_fold_northness_group, elevation_group) %>%
  summarise(RRI_end = mean(RRI_end),
            postFireSAP = mean(postFireSAP),
            adj_fold_northness = mean(adj_fold_northness),
            PPThist = mean(PPThist),
            TmaxHist = mean(TmaxHist),
            CWDhist = mean(CWDhist),
            AEThist = mean(AEThist),
            april_1st_snowpack = mean(april_1st_snowpack),
            elevation = mean(elevation),
            mediumReburns = mean(mediumReburns),
            lowReburns = mean(lowReburns),
            ppt_Yr1_3_mean_annual_anom = mean(ppt_Yr1_3_mean_annual_anom),
            postFirePrecip = mean(postFirePrecip),
            recoveryTime = mean(recoveryTime),
            aspect = mean(aspect),
            n = length(postFireSAP_group),
            x = mean(x),
            y = mean(y)) %>% 
  filter(n > 30)



############################################
#make histograms of predictor variables#####
############################################

#numeric vars
numeric_vars <- df2 %>% select_if(.predicate = is.numeric) %>% names()

# hists <- df2 %>% select_if(.predicate = is.numeric) %>%
#   gather(key = "var", value = "value", vars) %>%
#   ggplot(aes(value)) +
#   #geom_histogram(aes(y = ..density..)) +
#   geom_histogram() +
#   geom_density() +
#   facet_wrap(.~var, scales = "free",nrow = 11) +
#   theme_minimal()

#makePDF(fig = hists, file_name = 'predictorVarsHist',height = 10, width = 15)


#categorical vars

#distribution over landfire forest types
# df3 %>% ggplot(aes(forestType)) +
#   geom_histogram(stat = "count") +
#   theme_minimal()







################NO MORE CHANGES TO DF FOR MODEL FITTING############################

#######################################
#############create rarified df########
#######################################s
rarified_df <- c()
for (f in fires_to_include){
  set.seed <- 5
  tmp <- sample_n(filter(df4, FIRE_NAME == f), 500, replace = FALSE)
  rarified_df <- rbind(rarified_df,tmp)
}


#check the distribution of the data
hist(rarified_df$RRI_end, 100)



#fit.beta <- fitdist(rarified_df$RRI_end, "beta")
#fit.gamma <- fitdist(rarified_df$RRI_end, "gamma")





#alternate function to split vars into groups (5 quantiles)
# get_var_groups <- function(d,var){
#   print(var)
#   group_var = paste0(var,"_group")
#   quants <- quantile(as.numeric(d[var][[1]]), probs = seq(from = 0.2, to = 0.8, by = 0.2), na.rm = T)
#   print(quants)
#   out <- mutate(.data = d, !!group_var := case_when(
#     !!sym(var) < quants[1] ~ "1st",
#     !!sym(var) < quants[2] & !!sym(var) >= quants[1] ~ "2nd",
#     !!sym(var) < quants[3] & !!sym(var) >= quants[2] ~ "3rd",
#     !!sym(var) < quants[4] & !!sym(var) >= quants[3] ~ "4th",
#     !!sym(var) > quants[5] ~ "5th")
#     )
#   return(out)
# }







#fit.norm <- fitdist(grouped_df$RRI_end, "norm")
#plot(fit.norm)





################################
##########equal sample per fire#
################################

# equal_sample_per_fire <- df4 %>%
#   get_var_groups(var = "elevation") %>%
#   get_var_groups(var = "postFirePrecip") %>%
#   get_var_groups(var = "postFireSAP") %>%
#   get_var_groups(var = "adj_fold_northness") %>%
#   mutate_at(.vars = c("burnSev","mediumReburns","lowReburns"), .funs = function(x){as.numeric(as.character(x))}) %>%
#   group_by(FIRE_NAME, elevation_group,postFireSAP_group,adj_fold_northness_group) %>% 
#   summarise(RRI_end = mean(RRI_end),
#             postFireSAP = mean(postFireSAP),
#             adj_fold_northness = mean(adj_fold_northness),
#             PPThist = mean(PPThist),
#             TmaxHist = mean(TmaxHist),
#             CWDhist = mean(CWDhist),
#             AEThist = mean(AEThist),
#             april_1st_snowpack = mean(april_1st_snowpack),
#             elevation = mean(elevation),
#             mediumReburns = mean(mediumReburns),
#             lowReburns = mean(lowReburns),
#             ppt_Yr1_3_mean_annual_anom = mean(ppt_Yr1_3_mean_annual_anom),
#             postFirePrecip = mean(postFirePrecip),
#             recoveryTime = mean(recoveryTime),
#             burnSev = mean(burnSev),
#             x = mean(x),
#             y = mean(y)) %>%
#   drop_na()
 








#####################################################
#create df where the experimental unit is the fire###
#####################################################

# df3 %>% mutate_at(.vars = c("burnSev","mediumReburns","lowReburns"), .funs = function(x){as.numeric(as.character(x))}) %>%
#   #filter(disturbanceSize > 0.5, postFirePrecip < 2000) %>%
#   group_by(FIRE_NAME) %>%
#   summarise(RRI_end = mean(RRI_end),
#             postFireSAP = mean(postFireSAP),
#             adj_fold_northness = mean(adj_fold_northness),
#             burnSev = mean(burnSev),
#             PPThist = mean(PPThist),
#             mediumReburns = mean(mediumReburns),
#             postFirePrecip = mean(postFirePrecip),
#             recoveryTime = mean(recoveryTime)) %>%
#   arrange(desc(RRI_end)) %>%
#   ggplot(aes(PPThist, RRI_end)) +
#   geom_point() + 
#   geom_smooth(method = "lm") +
#   adams_theme




###########################
#predictor var categories##
###########################
topoVars <- c("hli","tpi","adjustedNorthness","adj_fold_northness","slope","elevation")
histClimateVars <- c("AEThist","CWDhist","TmaxHist","PPThist","april_1st_snowpack")
fireHistory <- c("preFireConCov","postFireConCov","disturbanceSize","burnSev","mediumReburns","lowReburns")
seedAvail <- c("postFireSAP")
postFireWeatherVars <- c("postFirePrecip","postFireMaxT","ppt_Yr1_3_mean_annual_anom","pptYr1_3_annual_mean_t_adjusted")
                         #"min_annual_ppt_Yr1_3", "min_annual_ppt_Yr1_3_anom",
                         #"min_annual_ppt_t_adjusted_Yr1_3")
otherVars <- c("x","y","wilderness","forestType","recoveryTime","FIRE_NAME")

#######################################################################
##########look at correlations among predictor variables###############
#######################################################################
#terrainCorr <- getCorrelationMatrix(d = df4, varsForCorr = topoVars)
#histClimateCorr <- getCorrelationMatrix(d = df4 %>% drop_na(), varsForCorr = histClimateVars)
#postFireWeatherCorr <- getCorrelationMatrix(d = df4 %>% drop_na(),postFireWeatherVars)

############################
#save df for model fitting##
############################

#saveRDS(df3, "analysis_fall_2022/for_model_fitting.Rds")





#########################################################################################
#visualize recovery trajectories stratified by postFire precip. and seed availability####
#########################################################################################

# SAP_quantiles <- quantile(df3$postFireSAP, probs = c(0.33,0.66), na.rm = T)
# PPT_quantiles <- quantile(df3$pptYr1_3_annual_mean, probs = c(0.33,0.66), na.rm = T)
# 
# recoveryTrajsStratFigDF <- df3 %>%
#   #filter(conCov_end < 0.1) %>%
#   mutate(SeedAvail = case_when(
#     postFireSAP <= SAP_quantiles[1] ~ "low seed avail.",
#     postFireSAP >= SAP_quantiles[2] ~ "high seed avail.",
#     (postFireSAP > SAP_quantiles[1]) & (postFireSAP < SAP_quantiles[2]) ~ "medium seed avail."
#   )) %>%
#   mutate(PostFirePPT = case_when(
#     pptYr1_3_annual_mean <= PPT_quantiles[1] ~ "low post-fire precip.",
#     pptYr1_3_annual_mean >= PPT_quantiles[2] ~ "high post-fire precip.",
#     (pptYr1_3_annual_mean > PPT_quantiles[1]) & (pptYr1_3_annual_mean < PPT_quantiles[2]) ~ "medium post-fire precip."
#   )) %>% mutate(SeedAvail2 = factor(SeedAvail,levels = c("high seed avail.","medium seed avail.","low seed avail."))) %>%
#   mutate(PostFirePPT2 = factor(PostFirePPT,levels = c("high post-fire precip.","medium post-fire precip.","low post-fire precip."))) %>%
#   mutate(planted = case_when(
#     postFirePlanting >= 0.3 ~ TRUE,
#     postFirePlanting < 0.3 ~ FALSE
#   ))
# 
#  meanRecov <- recoveryTrajsStratFigDF %>% filter(SeedAvail2 == "high seed avail.",
#                                                  PostFirePPT == "high post-fire precip.") %>%
#    ggplot() +
#    geom_line(mapping = aes(timeSinceFire,RRI,group = pixelID, color = burnSev, linetype = planted)) +
#    scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 3) +
#    scale_y_continuous(limits = c(-0.2,1.2)) +
#    scale_x_continuous(limits = c(-2,35)) +
#    labs(title = "XYZ") +
#    theme_minimal()
#    
#  recoveryTrajsStratFigDF %>%
#    group_by(timeSinceFire,SeedAvail2,PostFirePPT2) %>%
#    summarise(RRI_mean = RRI) %>% distinct() %>% group_by(SeedAvail2,PostFirePPT2,timeSinceFire) %>% 
#    summarise(RRI_mean2 = RRI_mean) %>% print(n = 200)
#    
# recoveryTrajsStratFigDF %>%
#   ggplot() +
#   geom_line(mapping = aes(timeSinceFire,RRI,group = pixelID, color = burnSev, linetype = planted)) +
#   #ggplot(aes(timeSinceFire,conCov,group = pixelID, color = burnSev)) +
#   #geom_line(size = 1) +
#   #geom_line(data = meanRecov, mapping = aes(timeSinceFire,conCov_mean), color = "black", size = 2) +
#   facet_grid(rows = vars(SeedAvail2), cols = vars(PostFirePPT2),drop = FALSE) +
#   scale_colour_gradient2(low = "blue", mid = "yellow", high = "red", midpoint = 3) +
#   scale_y_continuous(limits = c(-0.2,1.2)) +
#   scale_x_continuous(limits = c(-2,35)) +
#   labs(title = "XYZ") +
#   theme_minimal()


#################################################################################################
##############################Climate insert for Fig. 1##########################################
#################################################################################################


###postfire pptyr1-3
PrecipHist <- df4 %>%
  ggplot(aes(PPThist)) +
  geom_histogram(binwidth = 60, color = "black") +
  xlab(expression(paste("Mean annual precipitation [mm yr"^"-1","]"))) +
  #scale_x_continuous(breaks = c(0,1000,2000,3000), limits = c(0,3000)) +
  ylab(label = "N pixels") +
  #scale_y_continuous(limits = c(0,400)) +
  adams_theme
#makePNG(fig = PrecipHist, path_to_output.x = fig_path,file_name = "PrecipHist", height = 3.5, width = 4)

THist <- df4 %>%
  ggplot(aes(TmaxHist)) +
  geom_histogram(binwidth = 0.3, color = "black") +
  xlab(expression(paste("Mean daily ","maximum temp. ["*degree*C,"]"))) +
  ylab(label = "N pixels") +
  #scale_x_continuous(breaks = c(10,15,20,25), limits = c(10,25)) +
  #scale_y_continuous(limits = c(0,400)) +
  adams_theme
#makePNG(fig = THist, path_to_output.x = fig_path,file_name = "THist", height = 3.5, width = 4)


climate_inserts <- plot_grid(PrecipHist,THist, labels = c("(a)","(b)"), label_x = 0.2)
#makePNG(fig = climate_inserts, path_to_output.x = fig_path,file_name = "climate_inserts", height = 3.5, width = 8)

###########################
####post-fire precip hist##
###########################
# df4 %>%
#   ggplot(aes(PPThist)) +
#   geom_histogram() +
#   xlab("Post-fire precipitation in first 3 yrs \n after fire [mm yr-1]") +
#   ylab(label = "N Pixels") +
#   theme_minimal()


#584 & 551: Mediterranean California Dry-Mesic Mixed Conifer Forest and Woodland
#585 & 552: Mediterranean California Mesic Mixed Conifer Forest and Woodland
#588: California Montane Jeffrey Pine(-Ponderosa Pine) Woodland
#587: Mediterranean California Lower Montane Black Oak-Conifer Forest and Woodland
#549: Klamath-Siskiyou Lower Montane Serpentine Mixed Conifer Woodland
#550: Klamath-Siskiyou Upper Montane Serpentine Mixed Conifer Woodland
#577: Inter-Mountain Basins Sparsely Vegetated Systems (filtered out)
#589: Mediterranean California Red Fir Forest - Cascades
#2701: unk.
