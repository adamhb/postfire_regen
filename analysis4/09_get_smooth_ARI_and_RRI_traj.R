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
  
  #fire year
  fYr_p <- recovery_windows %>% filter(pixelID == p) %>% pull(fireYear)
  
  #recovery end
  recovery_end_p <- recovery_windows %>% filter(pixelID == p) %>% pull(recoveryEnd)
  
  #reburns
  mediumReburn_p <- recovery_windows %>% filter(pixelID == p) %>% pull(mediumReburns)
  lowReburn_p <- recovery_windows %>% filter(pixelID == p) %>% pull(lowReburns)
  
  # get the year with the minimum amount of canopy cover
  year_postFire_min_p <- slope_df %>% filter(pixelID == p, ARI == 0) %>% pull(year)
  
  # define the recovery trajectory, starting at year of min con cover
  p_recovery_traj <- slope_df %>% filter(pixelID == p) %>%
    filter( (year >= fYr_p) & (year <= recovery_end_p) )
  
  # fit a loess to the recovery traj
  loess_mod <- loess(formula = ARI ~ year,data = p_recovery_traj, span = 1.2)
  
  # get the smooth recovery traj
  loess_preds <- predict(loess_mod)
  
  #p_recovery_traj$pixel_id = p
  p_recovery_traj$ARI_smooth = loess_preds
  
  
  return(p_recovery_traj)
}

pixels_for_traj_fig <- read_csv('analysis4/08_pixels_for_traj_fig.csv')

pixels_for_traj_fig$pixels_for_traj_fig


smooth_trajectories <- c()
j = 0
for(p in pixels_for_traj_fig$pixels_for_traj_fig){
  j = j + 1
  print(paste(j,'of',length(pixels_for_traj_fig$pixels_for_traj_fig)))
  tmp = get_recovery_rate(p = p)
  smooth_trajectories <- rbind(smooth_trajectories,tmp)
}


unique(smooth_trajectories$pixelID)

write_csv(smooth_trajectories,file = "analysis4/08_smooth_trajectories_start_at_fire_year.csv")



#   
#   
#   smooth_ARI_end <- tail(loess_preds,1)
#   
#   drop = max(loess_preds) - smooth_ARI_end
#   drop_pct_of_max <- drop / max(loess_preds)
#   
#   mod <- lm(formula = ARI ~ year,data = p_recovery_traj)
#   modp <- lmp(mod) 
#   recovery_rate <- coef(mod)[2]
#   
#   
#   if(restrict_negative_ARI == T){
#     if( (mediumReburn_p < 0.05 & lowReburn_p < 0.05) & (drop_pct_of_max > 0.5) ) { #this indicates major disturbance not captured by medium re-burn variable during the recovery traj. We want to ignore these cases.
#       recovery_rate <- NA
#       smooth_ARI_end <- NA
#     } else if (modp > 0.05 | recovery_rate < 0 | (modp < 0.05 & recovery_rate > 0 & smooth_ARI_end < 0)){
#       recovery_rate <- 0
#       smooth_ARI_end <- 0
#     }
#   }else {
#     
#     if( (mediumReburn_p < 0.05 & lowReburn_p < 0.05) & (drop_pct_of_max > 0.5) ) { #this indicates major disturbance not captured by medium re-burn variable during the recovery traj. We want to ignore these cases.
#       recovery_rate <- NA
#       smooth_ARI_end <- NA
#     }
#   }
#   
#   
#   
#   return(list(recovery_rate,smooth_ARI_end))
# }
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
