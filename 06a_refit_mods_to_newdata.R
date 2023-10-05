
####Domain######################################
domain_name <- "nf_megram_reduced_1000m"
df6_filter <- df6 %>%
  filter(pixelID %in% nf_megram_reduced_with_warm_dry)
print(paste("sample size:",nrow(df6_filter)))
modData <-  df6_filter %>%
  mutate(across(where(is.numeric),scale_this))
print(paste("sample size:",nrow(modData)))

#path to previously fit models
paths <- paste0("analysis4/mods/",list.files("analysis4/mods",pattern = "_nf_"))
modNum <- 100
j <- 1
for(m in paths){
  j <- j + 1
  modNum <- modNum + 1
  savedForm <- get_formula(m)
  mod <- gam(formula = savedForm,
             data = modData, select = F, method = "REML")
  model_run_time_stamp <- Sys.time() %>% 
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = ":", replacement = "-") %>%
    sub(pattern = " ", replacement = "-")
  save(mod,file = paste0("analysis4/mods2/mod",modNum,"_",domain_name,"_","_notes_",model_run_time_stamp,".rda"))
  print(paste("done with:",j,"of",length(paths)))
}
