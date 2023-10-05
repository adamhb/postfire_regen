###############################
####Libraries and functions####
###############################
source('~/cloud/gdrive/fire_project/postfire_regen/analysis2/00_setup_A2.R')

fromFile <- T
makeFigs <- T

########################
#Load data and clean####
########################

#This data is generated from python scripts
cleanedData <- '~/cloud/gdrive/fire_project/postfire_regen/analysis2/analysisData_102722.csv'

#clean
df2 <- read_csv(cleanedData)[-1] %>%
  mutate_at(.vars = c("mediumReburns","lowReburns"), .funs = ~replace(., is.na(.), 0)) %>%
  filter(disturbanceSize > 0.2,
         preFireConCov > 0.2999#,
         #RRI_end < 3
         ) %>% # add these filters to the methods
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


#Option to filter to only include fires with > 1,000 pixels 
#df3 <- filter(df2, FIRE_NAME %in% fires_to_include) 
df3 <- df2

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

