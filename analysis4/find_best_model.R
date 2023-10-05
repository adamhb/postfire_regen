#find the best model

topoVars <- c("hli","tpi","adjustedNorthness","adj_fold_northness","slope","elevation")
histClimateVars <- c("AEThist","CWDhist","TmaxHist","PPThist")
fireHistory <- c("burnSev","mediumReburns","lowReburns","postFireSAP")
postFireWeatherVars <- c("postFirePrecip","ppt_Yr1_3_mean_annual_anom", "pptYr1_3_annual_mean_t_adjusted",
                         "minPostFirePrecip", "min_annual_ppt_Yr1_3_anom","min_annual_ppt_t_adjusted_Yr1_3")

var_categories <- c("topography", "fire_history", "historical_climate", "post_fire_weather")


allVars <- c(topoVars,histClimateVars,fireHistory,postFireWeatherVars)