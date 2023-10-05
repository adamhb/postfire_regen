#make figs

source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')
library(sf)
library(sp)
library(directlabels)
library(metR)


#png options
PNGheight = 5
PNGwidth = 8 #usually 8
PNGunits = "in"
PNGres = 100

###############
###Functions###
###############
get_vars_of_mod_pred_data <- function(mod_pred_data){
  mod_pred_vars <- c()
  n_vars <- length(mod_pred_data)
  for(i in 1:n_vars){
    new_var <- mod_pred_data[[i]]$xlab
    mod_pred_vars <- c(mod_pred_vars,new_var)
  }
  return(mod_pred_vars)
}


unstandardize <- function(var,mean,sd){
  return((var * sd) + mean)
}

get_fig_data <- function(var_of_interest,unstandardized_data,mod_pred_data,y_position = -15,obs_data = FALSE){
  
  mod_pred_vars <- get_vars_of_mod_pred_data(mod_pred_data)
  var_index <- which(mod_pred_vars == var_of_interest)
  mean_var <- unstandardized_data %>% pull(var_of_interest) %>% mean()
  st_dev <- unstandardized_data %>% pull(var_of_interest) %>% sd()
  
  x <- mod_pred_data[[var_index]]$x
  x <- unstandardize(x,mean_var,st_dev)
  x.obs <- mod_pred_data[[var_index]]$raw
  x.obs <- unstandardize(x.obs,mean_var,st_dev)
  fit <- as.numeric(mod_pred_data[[var_index]]$fit)
  down95 <- fit - as.numeric(mod_pred_data[[var_index]]$se)
  up95 <- fit + as.numeric(mod_pred_data[[var_index]]$se)
  figData <- tibble(x = x, fit = fit, down95 = down95, up95 = up95)
  #names(figData) <- c("x","")
  obsData <- tibble(x = x.obs, y = -1)
  if(obs_data == TRUE){
    return(obsData)}else{
      return(figData)
    }
}

#un-standardizing the predictions here
get_fig_data <- function(var_of_interest,unstandardized_data,mod_pred_data,y_position,obs_data = FALSE){
  
  mod_pred_vars <- get_vars_of_mod_pred_data(mod_pred_data)
  var_index <- which(mod_pred_vars == var_of_interest)
  mean_var <- unstandardized_data %>% pull(var_of_interest) %>% mean()
  st_dev <- unstandardized_data %>% pull(var_of_interest) %>% sd()
  
  mean_fit <- unstandardized_data %>% pull(ARI_end_smooth) %>% mean()
  st_dev_fit <- unstandardized_data %>% pull(ARI_end_smooth) %>% sd()
  
  x <- mod_pred_data[[var_index]]$x
  x <- unstandardize(x,mean_var,st_dev)
  x.obs <- mod_pred_data[[var_index]]$raw
  x.obs <- unstandardize(x.obs,mean_var,st_dev)
  fit <- as.numeric(mod_pred_data[[var_index]]$fit)
  fit <- fit * st_dev_fit
  se <- as.numeric(mod_pred_data[[var_index]]$se)
  se <- se * st_dev_fit
  down95 <- fit - se
  up95 <- fit + se
  figData <- tibble(x = x, fit = fit, down95 = down95, up95 = up95)
  #names(figData) <- c("x","")
  obsData <- tibble(x = x.obs, y = y_position)
  if(obs_data == TRUE){
    return(obsData)}else{
      return(figData)
    }
}


plot_pred_var <- function(variable_to_plot, ymin, ymax){
  figureData <- get_fig_data(variable_to_plot,unstandardized_data,mod_pred_data,y_position = 1)
  obs_y_loc <- ymin
  
  observData <- get_fig_data(variable_to_plot,unstandardized_data,mod_pred_data,y_position = obs_y_loc,
                             obs_data = TRUE)
  
  
  
  Fig <- figureData %>%
    ggplot(aes(x,fit)) +
    geom_line() +
    geom_line(aes(x,down95), linetype = "dotted") +
    geom_line(aes(x,up95), linetype = "dotted") +
    geom_point(data = observData, mapping = aes(x,y), shape = 4) +
    xlab(x_axis_lables[variable_to_plot]) +
    ylab("s(x)") +
    scale_y_continuous(limits = c(obs_y_loc,ymax)) +
    #scale_y_continuous() +
    adams_theme
  
  makePNG(fig = Fig,path_to_output.x ="analysis4/figs/",file_name = paste0(variable_to_plot,"_",domain_name,"_"),height = 6,width = 6)
  
  return(Fig)
}



##Load data and filters
#This dataset is created in 01_clean_A4
df6 <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/modelFittingData_030623.csv')

#load domain filters
nf <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf.csv')
nf_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf_megram_reduced.csv')
wild <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild.csv')
wild_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild_megram_reduced.csv')

#CHOOSE DOMAIN#
domain_name <- "nf_megram_reduced_1000m"
df6_filter <- df6 %>%
  filter(pixelID %in%nf_megram_reduced$pixelID)
print(paste("sample size:",nrow(df6_filter)))
modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))
print(paste("sample size:",nrow(modData)))
unstandardized_data <- df6_filter

#CHOOSE model
#path_to_mod <- 'analysis4/mods/mod48_nf_megram_reduced_1000m__postFireTXpostFirePrecip_2023-04-04-12-39-33.rda'

path_to_mod <- 'analysis4/mods/mod68_wild_megram_reduced_1000m__notes_2023-04-04-13-53-31.rda'

load(path_to_mod)
mod_pred_data <- plot(mod, ask = FALSE)

#choose vars
modelVarsToMake2DPlots <- get_vars_of_mod_pred_data(mod_pred_data)[-7]

#x_axis_lables <- c("Recovery time [yrs]","Fraction of pixel that reburned \n at medium severity","Burn severity","Topographic position index","Heat load index","Post-fire SAP")

x_axis_lables <- c("Recovery time [yrs]","Fraction of pixel that reburned \n at medium severity","Burn severity","Elevation [m]","Heat load index","Fraction of pixel that reburned \n at low severity")

names(x_axis_lables) <- modelVarsToMake2DPlots






#################
###NF domain#####
#################

#################
#make 2D figs####
#################


#get y limits for panel
fig_data_for_whole_panel <- tibble()
for(v in modelVarsToMake2DPlots){
  tmp <- get_fig_data(v,unstandardized_data,mod_pred_data,y_position = 1,obs_data = FALSE)
  fig_data_for_whole_panel <- rbind(fig_data_for_whole_panel,tmp)
}
y_min_panel <- min(fig_data_for_whole_panel$down95)
y_max_panel <- max(fig_data_for_whole_panel$up95)


plot_list <- list()
i <- 0
for(varr in modelVarsToMake2DPlots){
  i <- i + 1
  plot_list[[i]] <- plot_pred_var(varr,y_min_panel,y_max_panel)
}




TwoD_vars_NF <- plot_grid(plot_list[[1]],
          plot_list[[2]],
          plot_list[[3]],
          plot_list[[4]],
          plot_list[[5]],
          plot_list[[6]], align = "h", 
          labels = c("(a)","(b)","(c)","(d)","(e)","(f)"),
          label_x = -0.02, label_y = 1)

makePDF(fig = TwoD_vars_NF,file_name = "2D_vars_wild_meagram", width = 12, height = 8,units = "in")
makePNG(fig = TwoD_vars_NF,file_name = "2D_vars_wild_meagram", width = 11, height = 7, units = "in")







#####################################
##########Interaction################


##Load data and filters
#This dataset is created in 01_clean_A4
df6 <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/modelFittingData_030623.csv')

#load domain filters
nf <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf.csv')
nf_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf_megram_reduced.csv')
wild <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild.csv')
wild_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild_megram_reduced.csv')

#CHOOSE DOMAIN#
domain_name <- "nf_megram_reduced_1000m"
df6_filter <- df6 %>%
  filter(pixelID %in% nf_megram_reduced$pixelID)
print(paste("sample size:",nrow(df6_filter)))
modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))
print(paste("sample size:",nrow(modData)))
unstandardized_data <- df6_filter

#CHOOSE model
path_to_mod <- 'analysis4/mods/mod90_nf_megram_reduced_1000m__notes_2023-05-03-10-42-42.rda'

#path_to_mod <- 'analysis4/mods/mod68_wild_megram_reduced_1000m__notes_2023-04-04-13-53-31.rda'

load(path_to_mod)
mod_pred_data <- plot(mod, ask = FALSE)



get_interaction_data <- function(x_axis_variable_to_plot, obs_data = F){
  
  #browser()
  mod_pred_vars <- get_vars_of_mod_pred_data(mod_pred_data)
  x_var_index <- which(mod_pred_vars == x_axis_variable_to_plot)
  mean_x_var <- unstandardized_data %>% pull(x_axis_variable_to_plot) %>% mean()
  y_var <- mod_pred_data[[x_var_index]]$ylab
  mean_y_var <- unstandardized_data %>% pull(y_var) %>% mean()
  
  
  st_dev_x <- unstandardized_data %>% pull(x_axis_variable_to_plot) %>% sd()
  st_dev_y <- unstandardized_data %>% pull(y_var) %>% sd()
  st_dev_pred <- unstandardized_data %>% pull(ARI_end_smooth) %>% sd()
  
  x <- as.numeric(mod_pred_data[[x_var_index]]$x)
  x <- unstandardize(x,mean_x_var,st_dev_x)
  
  y <- as.numeric(mod_pred_data[[x_var_index]]$y)
  y <- unstandardize(y,mean_y_var,st_dev_y)
  
  mod_pred_z <- as.numeric(mod_pred_data[[x_var_index]]$fit)
  mod_pred_z <- mod_pred_z * st_dev_pred
  
  obs <- tibble(mod_pred_data[[x_var_index]]$raw)
  names(obs) <- c("x_obs","y_obs")
  obs$x_obs <- unstandardize(obs$x_obs,mean_x_var,st_dev_x)
  obs$y_obs <- unstandardize(obs$y_obs,mean_y_var,st_dev_y)
  
  interaction_data <- tibble()
  j <- 1
  for(y_val in y){
    for(x_val in x){
      j <- j + 1
      tmp <- tibble(x = x_val, y = y_val, z = mod_pred_z[j])
      interaction_data <- rbind(interaction_data,tmp)
    }
  }
  names(interaction_data) <- c(x_axis_variable_to_plot,y_var,"z")
  if(obs_data == T){
    return(obs)
  }else{
    return(interaction_data)
  }
}

obs_nf <- get_interaction_data(x_axis_variable_to_plot = "postFireMaxT",obs_data = T)
obs_nf$z <- 0

#obs_data_nf <- get_interaction_data(x_axis_variable_to_plot = "postFireMaxT",obs_data = T)

plot_interaction <- function(x_var, y_var,x_units,y_units,x_max = 0.41,y_max = 2001,bin_width = 0.05){
  
  interaction_data_for_plot <- get_interaction_data(x_var)
  
  ggplot_data <- interaction_data_for_plot %>%
    filter(!!as.symbol(y_var) < y_max,
           !!as.symbol(x_var) < x_max) 
  
  Fig <- ggplot(ggplot_data,aes_string(x = x_var,
                                       y = y_var,
                                       z = "z"))  +
    xlab("Post-fire mean daily max temperture [°C]") +
    ylab("Post-fire precipitation [mm yr-1]") +
    metR::geom_contour_fill(binwidth = bin_width) +
    scale_fill_gradient2(low = "sienna", high = "blue4",midpoint = 0) +
    metR::geom_text_contour(aes(label = round(..level.., 10)), size = 5, colour = "black",
                            skip = 0,
                            min.size = 0) +
    #scale_color_continuous(name = "s(x)") +
    scale_x_continuous(limits = c(12,24),breaks = seq(12,24,1)) +
    scale_y_continuous(limits = c(500,3000),breaks = seq(500,3000,100)) +
    geom_point(data = obs_nf, aes(x_obs,y_obs), size= 2, alpha = 0.15, shape = 4, 
               position = position_dodge(width = 0.1)
               ) +
    labs(title = "", fill = "s(x,y)") +
    adams_theme 
  
  
  makePNG(fig = Fig,
          path_to_output.x ="analysis4/figs/",
          file_name = paste0(x_var,"_X_",y_var,"_"),height = 6,width = 6)
  return(Fig)
}



NF_interaction_fig <- plot_interaction(x_var = "postFireMaxT",y_var = "postFirePrecip",x_units = "[°C]",y_units = "[mm yr-1]",x_max = 24,y_max = 3000,bin_width = 0.01)
NF_interaction_fig


makePDF(fig = NF_interaction_fig,file_name = "NF_interaction_fig", width = 8, height = 6,units = "in")
makePNG(fig = NF_interaction_fig,file_name = "NF_interaction_fig", width = 8, height = 6,units = "in")



#plot interaction on wild best model
#plot_interaction(x_var = "postFireSAP",y_var = "minPostFirePrecip",x_units = "",y_units = "[mm]")

#plot interaction on nf best model
#plot_interaction(x_var = "postFireMaxT",y_var = "postFirePrecip",x_units = "[degree C]",y_units = "[mm yr-1]",x_max = 5000,y_max = 3000)





#CHOOSE DOMAIN#
domain_name <- "wild_megram_reduced_1000m"
df6_filter <- df6 %>%
  filter(pixelID %in% wild_megram_reduced$pixelID)
print(paste("sample size:",nrow(df6_filter)))
modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))
print(paste("sample size:",nrow(modData)))
unstandardized_data <- df6_filter

#CHOOSE model
path_to_mod <- 'analysis4/mods/mod68_wild_megram_reduced_1000m__notes_2023-04-04-13-53-31.rda' #this model puts edf = 1 vars as linear in the formula
load(path_to_mod)
mod_pred_data <- plot(mod)


obs_wild <- get_interaction_data(x_axis_variable_to_plot = "postFireSAP",obs_data = T)
obs_wild$z <- 0


plot_interaction <- function(x_var, y_var,x_units,y_units,x_max = 0.41,y_max = 2001,bin_width = 0.01){
  
  interaction_data_for_plot <- get_interaction_data(x_var)
  
  ggplot_data <- interaction_data_for_plot %>%
    filter(!!as.symbol(y_var) < y_max,
           !!as.symbol(x_var) < x_max) 
  
  Fig <- ggplot(ggplot_data,aes_string(x = x_var,
                                       y = y_var,
                                       z = "z"))  +
    xlab("Seed availability proxy (SAP)") +
    ylab("Precipitation in driest post-fire year [mm yr-1]") +
    metR::geom_contour_fill(breaks = MakeBreaks(binwidth = bin_width)) +
    metR::geom_text_contour(aes(label = round(..level.., 10)), size = 5, colour = "black",
                            skip = 0,
                            min.size = 0) +
    scale_fill_gradient2(low = "sienna", high = "blue4") +
    geom_point(data = obs_wild, aes(x_obs,y_obs), size= 2, alpha = 0.15, shape = 4, 
               position = position_dodge(width = 0.1)
    ) +
    #geom_point(mapping = aes_string(x_var,y_var)) +
    #scale_color_continuous(name = "s(x)") +
    scale_x_continuous(limits = c(0,0.45),breaks = seq(0,0.45,0.1)) +
    scale_y_continuous(limits = c(250,2100),breaks = seq(500,2000,500)) +
    labs(title = "", fill = "s(x,y)") +
    adams_theme 
  
  
  #makePNG(fig = Fig,
  #        path_to_output.x ="analysis4/figs/",
  #        file_name = paste0(x_var,"_X_",y_var,"_"),height = 6,width = 8)
  return(Fig)
}


wild_interaction_fig <- plot_interaction(x_var = "postFireSAP",y_var = "minPostFirePrecip",x_units = "",y_units = "[mm yr-1]",x_max = 0.45,y_max = 3000, bin_width = 0.01)

#wild_interaction_fig
makePNG(fig = wild_interaction_fig,file_name = "wild_interaction_fig", width = 8, height = 6,units = "in")


#vis.gam(mod, view=c('postFireSAP', 'minPostFirePrecip'), n.grid=50, theta=-45, phi=20, zlab="regen", too.far=0.16,plot.type = "persp",ticktype="detailed")+title("SAP by Post-fire Precip")

#vis.gam(mod, view=c('postFireMaxT', 'postFirePrecip'), n.grid=50, theta=-90, phi=10, zlab="", too.far=0.1)+title("SAP by Post-fire Precip")

