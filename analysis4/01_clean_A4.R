###############################
####Libraries and functions####
###############################
source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')


makeFigs <- T
redo_trajectories <- F
min_pix_per_fire <- 1
redo_hist <- T
restrict_negative_ARI <- F
########################
#Load data and clean####
########################
check_columns_for_high_low <- function(d,value)
  for(i in 1:ncol(d)){
    
    if(is.numeric(pull(.data = d,var = i)[1])){
      print(paste("-",i, sum(d %>% pull(i) < -1*value, na.rm = T)))
    }
    
    if(is.numeric(pull(.data = d,var = i)[1])){
      print(paste("+", i, sum(d %>% pull(i) > value, na.rm = T)))
    }
    
  }

###################
#pixels per fire###
###################
#This is to see how many valid pixels there are in each fire
#and to filter to fires that have > x pixels
pixels_per_fire <- read_csv('/home/adam/cloud/gdrive/fire_project/postfire_regen/analysis4/n_per_fire.csv')
#print(arrange(.data = pixels_per_fire, desc(n_per_fire)))

#filter to fires that only have more than 1000 pixels per fire
fires_to_include <- filter(pixels_per_fire, n_per_fire >= min_pix_per_fire) %>% pull(FIRE_NAME)

#This data is generated from python scripts

#time series data
df5 <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/timeSeriesData_021623.csv')
df5 <- df5 %>% filter(conCov > -1e+30,
                      conCov < 1e+30,
                      ARI < 3e+20, ARI > -3e20,
                      disturbanceSize < 3e+20, disturbanceSize > -3e+20)

check_columns_for_high_low(df5,1e+20)


#time stable data
cleanedData <- '~/cloud/gdrive/fire_project/postfire_regen/analysis4/analysisData_030623.csv'
raw <- read_csv(cleanedData)


#convert NA value (3.4e34) to "NA"


#########################
#clean time stable data##
#########################
df2 <- raw[-1] %>%
  mutate_at(.vars = 
              c("mediumReburns","lowReburns","BurnSev_of_worstBurn2014_2019", "year_of_worstBurn2014_2019"), 
            .funs = function(x){replace(x, x < -3.4e+30, 0)}) %>%
  filter(mediumReburns <= 1) %>%
  #mutate_at(.vars = "ARI_end", .funs = ~replace(., 3.4e+38, 0)) %>%
  mutate_at(.vars = "CAUSE", .funs = as.factor) %>%
  dplyr::select(-index_right) %>%
  mutate_at(.vars = "OBJECTID", .funs = as.character) %>% 
  #filter(RRI_end > -10) %>%
  filter(
         disturbanceSize > 0.2, 
         preFireConCov >= 0.3, 
         FIRE_NAME %in% fires_to_include,
         n > 5
         ) %>%
  mutate(ppt_Yr1_3_mean_annual_anom = (postFirePrecip - PPThist) / PPThistSD,
         min_annual_ppt_Yr1_3_anom = (minPostFirePrecip - PPThist) / PPThistSD,
         pptYr1_3_annual_mean_t_adjusted = postFirePrecip / postFireMaxT,
         min_annual_ppt_t_adjusted_Yr1_3 = minPostFirePrecip / postFireMaxT)


pix2 <- df2$pixelID
pix5 <- df5$pixelID

df4 <- df2 %>% mutate_if(.predicate = is.numeric,funs(replace(., . > 3e+20 | . < -3e+20, 0))) %>%
  filter(pixelID %in% pix2[pix2 %in% pix5])

check_columns_for_high_low(df4,3e+20)


#################################################################
####functions to calculate recovery slopes and smooth ARI values#
#################################################################

#first need these two dfs to do this
slope_df <- df5 %>% dplyr::select(pixelID,year,ARI) %>%
  arrange(pixelID)
recovery_windows <- df4 %>% dplyr::select(pixelID,fireYear,mediumReburns,lowReburns,recoveryEnd)

#extract p values from model
lmp <- function (modelobject) {
  if (class(modelobject) != "lm") stop("Not an object of class 'lm' ")
  f <- summary(modelobject)$fstatistic
  p <- pf(f[1],f[2],f[3],lower.tail=F)
  attributes(p) <- NULL
  return(p)
}

get_recovery_rate <- function(p){
  
fYr_p <- recovery_windows %>% filter(pixelID == p) %>% pull(fireYear)
recovery_end_p <- recovery_windows %>% filter(pixelID == p) %>% pull(recoveryEnd)
mediumReburn_p <- recovery_windows %>% filter(pixelID == p) %>% pull(mediumReburns)
lowReburn_p <- recovery_windows %>% filter(pixelID == p) %>% pull(lowReburns)


year_postFire_min_p <- slope_df %>% filter(pixelID == p, ARI == 0) %>% pull(year)

p_recovery_traj <- slope_df %>% filter(pixelID == p) %>%
                                filter( (year >= year_postFire_min_p) & (year <= recovery_end_p) )

loess_mod <- loess(formula = ARI ~ year,data = p_recovery_traj, span = 1.2)
loess_preds <- predict(loess_mod)

smooth_ARI_end <- tail(loess_preds,1)

drop = max(loess_preds) - smooth_ARI_end
drop_pct_of_max <- drop / max(loess_preds)

mod <- lm(formula = ARI ~ year,data = p_recovery_traj)
modp <- lmp(mod) 
recovery_rate <- coef(mod)[2]


if(restrict_negative_ARI == T){
  if( (mediumReburn_p < 0.05 & lowReburn_p < 0.05) & (drop_pct_of_max > 0.5) ) { #this indicates major disturbance not captured by medium re-burn variable during the recovery traj. We want to ignore these cases.
    recovery_rate <- NA
    smooth_ARI_end <- NA
  } else if (modp > 0.05 | recovery_rate < 0 | (modp < 0.05 & recovery_rate > 0 & smooth_ARI_end < 0)){
    recovery_rate <- 0
    smooth_ARI_end <- 0
  }
}else {
  
  if( (mediumReburn_p < 0.05 & lowReburn_p < 0.05) & (drop_pct_of_max > 0.5) ) { #this indicates major disturbance not captured by medium re-burn variable during the recovery traj. We want to ignore these cases.
  recovery_rate <- NA
  smooth_ARI_end <- NA
  }
}



return(list(recovery_rate,smooth_ARI_end))
}
#####################################################################


###########################################################
#Calculate recovery slopes and smooth ARI for all pixels###
###########################################################

sample <- df4$pixelID[floor(runif(min = 1,max = nrow(df4),n = 200))]

if(redo_trajectories == T){

recovery_rate_ARI_end_smooth <- tibble()
j <- 0
for(p in df4$pixelID){
  j <- j + 1
  print(j/nrow(df4) * 100)
  print(paste("p:",p))
  output <- get_recovery_rate(p)
  
  tmp <- tibble(
    pixelID = p,
    recovery_rate = output[[1]],
    ARI_end_smooth = output[[2]]
  )
  
  recovery_rate_ARI_end_smooth <- rbind(recovery_rate_ARI_end_smooth,tmp)
  
}
write_csv(recovery_rate_ARI_end_smooth,"analysis4/recovery_rate_ARI_end_smooth_no_restricting_ARI_min.csv")

} else {
  recovery_rate_ARI_end_smooth <- read_csv("~/cloud/gdrive/fire_project/postfire_regen/analysis4/recovery_rate_ARI_end_smooth_no_restricting_ARI_min.csv")
}
#############################################################

####################################################
#joining ARI_smooth_end with the rest of the data###
df6 <- df4 %>% left_join(recovery_rate_ARI_end_smooth, by = "pixelID") %>%
  filter(recovery_rate < 1) %>% #get rid of erroneous recovery rates
  drop_na(ARI_end_smooth,recovery_rate) 

#check for remaining NAs
check_NA <- function(d){
  for(i in 1:ncol(d)){
    print(paste(i,sum(is.na(pull(.data = d,var = i)))))
  }
}



####################################################
#histograms of topo vars, histClimate vars, seed vars and post fire weather vars
#varsForHist <- c(topoVars,histClimateVars,seedVars,postFireWeatherVars,otherVars)

varsForHist <- df6 %>%
  select_if(is.numeric) %>% names()


if(redo_hist == T){
  predictorHist <- df6 %>%
    filter(wilderness == 1) %>%
    select_if(is.numeric) %>%
    dplyr::select(varsForHist) %>%
    gather(key = "var", value = "value", varsForHist) %>%
    ggplot(aes(value)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density() +
    facet_wrap(.~var, scales = "free",nrow = 5) +
    theme_minimal()
  makePDF(fig = predictorHist, file_name = 'predictorVarsHist_wilderness_only',height = 20, width = 30)
}




######################################################
###########Export for exploration and model fitting###
######################################################
write_csv(df6, "analysis4/modelFittingData_030623.csv")

#get the same pixels in the time series data as in the time stable data
df5 <- df5 %>% filter(pixelID %in% df6$pixelID)

write_csv(df5, "analysis4/timeSeriesData_030623.csv")
#############################################
#test and visualize individual recovery traj#
#############################################



#recovery_rate_ARI_end_smooth 
#negativeARI_smooth <- recovery_rate_ARI_end_smooth %>% filter(ARI_end_smooth < 0) %>% pull(pixelID)

#p <- df6$pixelID[floor(runif(min = 1,max = nrow(df6),n = 1))]

# x <- df6 %>% filter(ARI_end_smooth < 0.05)
# 
# p <- x$pixelID[floor(runif(min = 1,max = nrow(x),n = 1))]



# fYr_p <- recovery_windows %>% filter(pixelID == p) %>% pull(fireYear)
# recovery_end_p <- recovery_windows %>% filter(pixelID == p) %>% pull(recoveryEnd)
# year_postFire_min_p <- slope_df %>% filter(pixelID == p, ARI == 0) %>% pull(year)
# 
# p_recovery_traj <- slope_df %>% filter(pixelID == p) %>%
#   filter( (year >= year_postFire_min_p) & (year <= recovery_end_p) )
# 
# loess_mod <- loess(formula = ARI ~ year,data = p_recovery_traj, span =1.2)
# smooth_ARI_end <- tail(predict(loess_mod),1)
# preds <- tibble(year = p_recovery_traj$year, ARI = predict(object = loess_mod))
# 
# mod <- lm(formula = ARI ~ year,data = p_recovery_traj)
# modp <- lmp(mod) 
# recovery_rate <- coef(mod)[2]
# 
# quad_mod <- lm(formula = ARI ~ year + I(year^2),data = p_recovery_traj)
# summary(quad_mod)
# quad_preds <- tibble(year = p_recovery_traj$year, ARI = predict(object = quad_mod))
# 
# drop = max(preds$ARI) - smooth_ARI_end
# drop_pct_of_max <- drop /max(preds$ARI)
# 
# 
# p_recovery_traj %>%
#   ggplot(aes(year,ARI)) +
#   geom_point() +
#   geom_line(aes(year,ARI),data = preds) +
#   geom_line(aes(year,ARI),data = quad_preds, color = "red") +
#   labs(title = paste(p,"drop: ",drop_pct_of_max))
# 
# 
# 
# 
# recovery_rate_ARI_end_smooth %>% filter(pixelID == 'tile_1_11_-281025350715')



########################
##Look at year effects##
########################
# df5 %>% ggplot(aes(as.factor(year),conCov)) +
#   geom_boxplot()
# hist(df6$fireYear)








#####################
#create grouped df##
#####################
#function to split vars into groups (4 quantiles)
# get_var_groups <- function(d,var){
#   print(var)
#   group_var = paste0(var,"_group")
#   quants <- quantile(as.numeric(d[var][[1]]), probs = c(0.25,0.5,0.75), na.rm = T)
#   print(quants)
#   out <- mutate(.data = d, !!group_var := case_when(
#     !!sym(var) < quants[1] ~ "1st",
#     !!sym(var) < quants[2] & !!sym(var) >= quants[1] ~ "2nd",
#     !!sym(var) < quants[3] & !!sym(var) >= quants[2] ~ "3rd",
#     !!sym(var) > quants[4] ~ "4th")
#   )
#   return(out)
# }

#created grouped df
#can only summarize by 4 variables?
# grouped_df <- df4 %>% 
#   get_var_groups(var = "elevation") %>%
#   get_var_groups(var = "postFirePrecip") %>%
#   get_var_groups(var = "postFireSAP") %>%
#   get_var_groups(var = "adj_fold_northness") %>%
#   mutate_at(.vars = c("burnSev","mediumReburns","lowReburns"), .funs = function(x){as.numeric(as.character(x))}) %>%
#   group_by(burnSev, postFirePrecip_group, postFireSAP_group, adj_fold_northness_group, elevation_group) %>%
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
#             aspect = mean(aspect),
#             n = length(postFireSAP_group),
#             x = mean(x),
#             y = mean(y)) %>% 
#   filter(n > 30)

