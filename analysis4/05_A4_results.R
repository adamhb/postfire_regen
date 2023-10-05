# This script is used for model fitting to explore various model forumations.
# The bottom half of the script saves different models for subsequent comparison
# in the next script 06_catalogue_results.

source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')
library(sf)
library(sp)
library(broom)

# Make table of model formulas, AIC, and R2
get_formula <- function(path_to_mod){
  load(path_to_mod)
  return(mod$formula)
}

#This dataset is created in 01_clean_A4
df6 <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/modelFittingData_030623.csv')

#load domain filters
nf <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf.csv')
nf_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf_megram_reduced.csv')
wild <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild.csv')
wild_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild_megram_reduced.csv')


warm_dry_pixel_IDs <- df6 %>% filter(postFirePrecip < 1000, postFireMaxT > 21) %>% pull(pixelID)
nf_megram_reduced_with_warm_dry <- c(nf_megram_reduced$pixelID,warm_dry_pixel_IDs)

#1. Fit Many Models Manually
##################
#####filter#######
##################

#modNum <- 0


## choose domain


####Domain######################################
domain_name <- "nf_megram_reduced_1000m"
df6_filter <- df6 %>%
  filter(pixelID %in% nf_megram_reduced_with_warm_dry)
print(paste("sample size:",nrow(df6_filter)))
modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))
print(paste("sample size:",nrow(modData)))

###############################################

print(paste("Mean ARI on full NF domain with spatial rarification",mean(df6_filter$ARI_end_smooth)))

####Domain######################################
domain_name <- "wild-megram_reduced_1000m"
df6_filter <- df6 %>%
  filter(pixelID %in% wild_megram_reduced$pixelID)
print(paste("sample size:",nrow(df6_filter)))
modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))
print(paste("sample size:",nrow(modData)))
###############################################
print(paste("Mean ARI on wild domain with spatial rarification",mean(df6_filter$ARI_end_smooth)))

####Model######################################
modNum <- 300


#### Note many model forms were fit here
#### See Table 2 of main text for list of all models tested

#This section of code fits a model specified manually here. This is for exploration purposes.
modNum <- modNum + 1
mod <- gam(formula = ARI_end_smooth ~
             s(recoveryTime, k = 6) +
             #s(x, y, bs = "gp", k = 48, m = 2) +

             #fire
             s(mediumReburns, k = 6) +
             s(burnSev, k = 6) +
             #s(TmaxHist, k = 15) +
             s(postFireSAP, k = 6) +

             #topo#
             #s(adj_fold_northness, k = 6) +
             #s(adjustedNorthness, k = 6) +
             s(hli, k = 6) +
             #s(hli, k = 6) +
             s(tpi,k = 6) +

             #historicalClimate
             #s(CWDhist, k = 6) +
             #s(AEThist, k = 6) +
            # s(PPThist, k = 6) +
             #s(TmaxHist, k = 6) +

             #postFireWeater
             #s(postFireMaxT, k = 6) #+
             #s(postFirePrecip, k = 6),

             #interactions
             te(postFireMaxT,ppt_Yr1_3_mean_annual_anom, k = 6)
           ,
           data = modData, select = F, method = "REML")

#model results
#summary(mod)
paste0("AIC:",as.numeric(AIC(mod)))

plot(mod)
#see interaction
vis.gam(mod, view=c('postFireMaxT', 'minPostFirePrecip'), n.grid=50, theta=25, phi=0, zlab="", too.far=0.1, plot.type = "contour")+title("")


model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")

save(mod,file = paste0("analysis4/NF_mods_with_warm_dry/mod_minPostFirePrecip",modNum,"_",domain_name,"_",model_run_time_stamp,".rda"))
#############################################################

# The code below is designed to 
# fit a pre-determined list of formulas automatically to new domain
# or to re-fit models that have already been fit
# for the purpose of recording all of the model performance metrics
# in one place.

#path to previously fit models
paths <- paste0("analysis4/mods/",list.files("analysis4/mods"))

modNum <- 0
####Domain######################################
domain_name <- "nf_megram_reduced_1000m"
df6_filter <- df6 %>%
  filter(pixelID %in% wild_megram_reduced$pixelID)
print(paste("sample size:",nrow(df6_filter)))
modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))
print(paste("sample size:",nrow(modData)))
###############################################



#####get list of fires in analysis
fires <- unique(df6_filter$FIRE_NAME)
write_csv(tibble(fires),'/home/adam/cloud/gdrive/fire_project/data_091722/fires_in_final_nf_domain.csv')


#histogram of elevation

#histograms for map plot
hist_for_map_wild <- ggplot(df6_filter, aes(elevation)) +
  geom_histogram(bins = 40) +
  scale_x_continuous(breaks = seq(500,3000,250),limits = c(500,2500)) +
  #scale_y_continuous(limits = c(0,500), breaks = seq(0,500,100)) +
  ylab(label = "N pixels") +
  xlab(label = "elelvation [m]") +
  adams_theme #+
  #theme(axis.title.x = element_text(size = 25),
  #      axis.title.y = element_text(size = 25),
  #      axis.text.x = element_text(size = 25),
  #      axis.text.y = element_text(size = 25))


elevation_figs <- plot_grid(plotlist = list(hist_for_map_wild, hist_for_map_NF),
          align = "h", 
          labels = c("(a)","(b)"),
          label_x = -0.02, label_y = 1,rel_widths = c(1,1.1))

makePNG(fig = elevation_figs,path_to_output.x = fig_path,file_name = "ele_hist_NF_vs_wild",width = 10, height = 6)


#histograms for map plot
hist_for_map <- ggplot(df6_filter, aes(PPThist)) +
  geom_histogram(bins = 80) +
  scale_x_continuous(breaks = seq(500,3000,500),limits = c(500,3100)) +
  scale_y_continuous(limits = c(0,500), breaks = seq(0,500,100)) +
  ylab(label = "N pixels") +
  xlab(label = "Mean annual precipitation [mm]") +
  adams_theme +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))

makePNG(fig = hist_for_map,path_to_output.x = fig_path,file_name = "precip_hist",width = 6, height = 6)

hist_for_map_t <- ggplot(df6_filter, aes(TmaxHist)) +
  geom_histogram(bins = 80) +
  scale_x_continuous(breaks = seq(10,24,4),limits = c(10,24)) +
  scale_y_continuous(limits = c(0,500), breaks = seq(0,500,100)) +
  ylab(label = "N pixels") +
  xlab(label = "Mean daily max temp. [°C]") +
  adams_theme +
  theme(axis.title.x = element_text(size = 25),
        axis.title.y = element_text(size = 25),
        axis.text.x = element_text(size = 25),
        axis.text.y = element_text(size = 25))

makePNG(fig = hist_for_map_t,path_to_output.x = fig_path,file_name = "tmax_hist",width = 6, height = 6)



#modNum <- 40

#modData <- df6_filter_wild_no_rare %>%
#  mutate(across(where(is.numeric),scale_this))

#for(f in paths){
  modNum <- 503
  
  #best wild model
   savedForm <- get_formula('analysis4/mods/mod68_wild_megram_reduced_1000m__notes_2023-04-04-13-53-31.rda')
   
   savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + s(mediumReburns, k = 6) + 
     s(burnSev, k = 6) + s(elevation, k = 6) + s(hli, k = 6) + 
     s(lowReburns, k = 6) + te(postFireSAP, pptYr1_3_annual_mean_t_adjusted, 
                               k = 6)
   
  # savedForm <- get_formula('mod70_wild_megram_reduced_1000m__notes_2023-04-04-17-17-03.rda')
  # 
  # savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + s(mediumReburns, k = 6) + 
  #   s(burnSev, k = 6) + s(elevation, k = 6) + s(hli, k = 6) + s(lowReburns, k = 6) +
  #   te(postFireSAP, minPostFirePrecip, k = 6)
  # 
  # savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + s(mediumReburns, k = 6) + 
  #   s(burnSev, k = 6) + s(hli, k = 6) + s(postFireMaxT, k = 6) +
  #   s(lowReburns, k = 6) + te(postFireSAP, minPostFirePrecip,k = 6)
  # 
  # #best nf model
  # savedForm <- get_formula('analysis4/mods/mod48_nf_megram_reduced_1000m__postFireTXpostFirePrecip_2023-04-04-12-39-33.rda')
  # savedForm <- get_formula('analysis4/mods2/mod_postFirePrecipAnom_nf_megram_reduced_1000m_2023-05-09-17-14-32.rda')
  # savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + s(mediumReburns, k = 6) + 
  #   s(burnSev, k = 6) + s(tpi, k = 6) + s(hli, k = 6) + s(postFireSAP, k = 6) + 
  #   te(postFireMaxT, postFirePrecip, k = 6)
  
  
  #NF ANOM
  #savedForm <- get_formula('analysis4/NF_mods_with_warm_dry/mod_postFirePrecipAnom_nf_megram_reduced_1000m_2023-05-09-17-14-32.rda')
  
  #NF min precip
  #savedForm <- get_formula('analysis4/NF_mods_with_warm_dry/mod_minPostFirePrecip212_nf_megram_reduced_1000m_2023-05-09-17-14-32.rda')
  
  #NF precip
  savedForm <- get_formula('analysis4/NF_mods_with_warm_dry/mod118_nf_megram_reduced_1000m__notes_2023-05-04-14-40-04.rda')
  
  savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + s(mediumReburns, k = 6) + 
    s(burnSev, k = 6) + s(tpi, k = 6) + s(hli, k = 6) + s(CWDhist, k = 6) + s(postFireSAP, 
                                                          k = 6)
  
  #best wild model with linear predictors where edf = 1
  # savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + mediumReburns + 
  #   burnSev + s(elevation, k = 6) + hli + lowReburns +
  #   te(postFireSAP, minPostFirePrecip, k = 6)
  # 
  # savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + s(mediumReburns, k = 6) + 
  #   s(burnSev, k = 6) + s(elevation, k = 6) + s(hli, k = 6) + 
  #   te(postFireSAP, minPostFirePrecip, k = 6)
  
  mod <- gam(formula = savedForm,
             data = modData, select = F, method = "REML")

  #model results
  summary(mod)
  plot(mod)
  tidy_gam_summary <- tidy(mod)
  #write_csv(tidy_gam_summary, "analysis4/wild_model_summary.csv")
  
  paste0("AIC:",as.numeric(AIC(mod)))
  AIC(mod)
  #save mod results
  model_run_time_stamp <- Sys.time() %>% 
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = " ", replacement = "-")
  save(mod,file = paste0("analysis4/mods/mod",modNum,"_",domain_name,"_","_notes_",model_run_time_stamp,".rda"))
  
  
  vis.gam(mod, view=c('postFireSAP', 'PPThist'), n.grid=50, theta=-45, phi=10, zlab="", too.far=0.1,plot.type = "persp")+title("SAP by Post-fire Precip")
  
  vis.gam(mod, view=c('postFireMaxT', 'postFirePrecip'), n.grid=50, theta=30, phi=15, zlab="", se = 1, too.far=0.1)+title("SAP by Post-fire Precip")
#}


##########Additional testing and checking###################
#Best wilderness model
#can apply this to NF domain with decent results

savedForm <- get_formula('analysis4/mods/mod63_wild_megram_reduced_1000m__notes_2023-04-04-13-34-02.rda')

bestWildModel <- ARI_end_smooth ~ s(recoveryTime, k = 6) + 
  s(mediumReburns, k = 6) + 
  s(lowReburns, k = 6) +
  s(burnSev, k = 6) + 
  s(elevation, k = 6) + 
  te(postFireSAP, postFirePrecip, k = 6)

mod <- gam(formula = savedForm,
           data = modData, select = F, method = "REML")

#model results
summary(mod)
paste0("AIC:",as.numeric(AIC(mod)))
plot(mod)

#see interaction
vis.gam(mod, view=c('postFireSAP', 'postFirePrecip'), n.grid=50, theta=-90, phi=10, zlab="", too.far=0.1)+title("SAP by Post-fire Precip")

#check gam
gam.check(mod)


#Additional model testing

savedForm <- get_formula("analysis4/mods/mod7_wild_megram_reduced_1000m.rda")

savedForm <- ARI_end_smooth ~ s(recoveryTime, k = 6) + 
  s(mediumReburns, k = 6) + 
  s(lowReburns, k = 6) +
  s(burnSev, k = 6) + 
  s(elevation, k = 6) + 
  te(postFireSAP, postFirePrecip, k = 6)

mod <- gam(formula = savedForm,
           data = modData, select = F, method = "REML")

#model results
summary(mod)
paste0("AIC:",as.numeric(AIC(mod)))
plot(mod)

#see interaction
vis.gam(mod, view=c('postFireSAP', 'postFirePrecip'), n.grid=50, theta=-90, phi=10, zlab="", too.far=0.1)+title("SAP by Post-fire Precip")

#check gam
gam.check(mod)




#Moran's I
pixel.dists <- as.matrix(dist(cbind(df6_filter$x, df6_filter$y)))
pixel.dists[1:5,1:5]
pixel.dists.inv <- 1/pixel.dists
diag(pixel.dists.inv) <- 0
print(Moran.I(residuals(mod),pixel.dists.inv))

