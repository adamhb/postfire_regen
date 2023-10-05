#find the best model

var_categories <- c("topography", "fire_history", "historical_climate", "post_fire_weather","recovery_time")

topoVars <- tibble(category = var_categories[1], var = c("hli","tpi","adjustedNorthness","adj_fold_northness","elevation"))
fireHistory <- tibble(category = var_categories[2], var = c("burnSev","mediumReburns","lowReburns","postFireSAP"))
histClimateVars <-  tibble(category = var_categories[3], var = c("AEThist","CWDhist","TmaxHist","PPThist"))
postFireWeatherVars <-  tibble(category = var_categories[4], var = c("postFirePrecip","ppt_Yr1_3_mean_annual_anom", "pptYr1_3_annual_mean_t_adjusted","minPostFirePrecip", "min_annual_ppt_Yr1_3_anom","min_annual_ppt_t_adjusted_Yr1_3","postFireMaxT"))

timeVar <- tibble(category = var_categories[5], var = "recoveryTime")
predictor_vars <- rbind(topoVars,fireHistory,histClimateVars,postFireWeatherVars,timeVar)
print(predictor_vars,n = 100)



#make histogram of predictor variables across domains
varsForHist <- df6_filter %>%
  dplyr::select(predictor_vars$var) %>%
  select_if(function(x) any(is.na(x))) 

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


#which variables are highly correlated



