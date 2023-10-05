#find the best model
redo_hist = F
var_categories <- c("topography", "fire_history", "historical_climate", "post_fire_weather","recovery_time")

topoVars <- tibble(category = var_categories[1], var = c("hli","tpi","adjustedNorthness","adj_fold_northness","elevation"))
fireHistory <- tibble(category = var_categories[2], var = c("burnSev","mediumReburns","lowReburns","postFireSAP"))
histClimateVars <-  tibble(category = var_categories[3], var = c("AEThist","CWDhist","TmaxHist","PPThist"))
postFireWeatherVars <-  tibble(category = var_categories[4], var = c("postFirePrecip","ppt_Yr1_3_mean_annual_anom", "pptYr1_3_annual_mean_t_adjusted","minPostFirePrecip", "min_annual_ppt_Yr1_3_anom","min_annual_ppt_t_adjusted_Yr1_3","postFireMaxT"))

timeVar <- tibble(category = var_categories[5], var = "recoveryTime")
predictor_vars <- rbind(topoVars,fireHistory,histClimateVars,postFireWeatherVars,timeVar)
print(predictor_vars,n = 100)


#make histogram of predictor variables across domains

domain_name <- "wild_megram_reduced_1000m"

#df6_filter <- df6 %>%
#  filter(pixelID %in% wild_megram_reduced$pixelID)

df6_filter <- df6 %>%
  filter(pixelID %in% nf_megram_reduced_with_warm_dry)

pred_var_df <- df6_filter %>%
  dplyr::select(predictor_vars$var)

varsForHist <- predictor_vars %>% filter(category == "post_fire_weather") %>% pull(var)

if(redo_hist == T){
  predictorHist <- pred_var_df %>%
    select_if(is.numeric) %>%
    gather(key = "var", value = "value", varsForHist) %>%
    ggplot(aes(value)) +
    geom_histogram(aes(y = ..density..)) +
    geom_density() +
    facet_wrap(.~var, scales = "free",nrow = 5) +
    theme_minimal()
  makePDF(fig = predictorHist, file_name = paste0('predictorVarsHist_',domain_name),height = 20, width = 30)
}


#make one histogram at a time
predictorHist <- pred_var_df %>%
  #dplyr::select(postFirePrecip) %>%
  ggplot(aes(postFirePrecip)) +
  geom_histogram(aes(y = ..density..)) +
  geom_density() +
  scale_x_continuous(limits = c(0,3000),breaks = seq(from = 00, to = 3000, by = 500)) +
  theme_minimal()
makePDF(fig = predictorHist, file_name = paste0('predictorVarsHist_',domain_name),height = 20, width = 30)




# Adjust the predictor var names
pred_var_df_new_names <- pred_var_df %>%
  rename(
    postFirePrecipAnom = ppt_Yr1_3_mean_annual_anom
  ) %>% dplyr::select(-pptYr1_3_annual_mean_t_adjusted,
         -min_annual_ppt_Yr1_3_anom,
         -min_annual_ppt_t_adjusted_Yr1_3) 

corr_mat_for_pdf <- getCorrelationMatrix(d = pred_var_df_new_names,varsForCorr = names(pred_var_df_new_names))
makePDF(fig = corr_mat_for_pdf,file_name = paste0("pred_corr_",domain_name),height = 10,width = 30)
#which variables are highly correlated



