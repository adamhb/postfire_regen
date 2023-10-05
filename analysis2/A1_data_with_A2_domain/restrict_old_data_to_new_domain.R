#df4 %>% filter(pixelID %in% modData$pixelID) %>% select(pixelID, x, y) %>% write_csv(file = "old_mod_points.csv")

source('~/cloud/gdrive/fire_project/postfire_regen/analysis1/A1_results.Rmd')

A1_points <- df4 %>%
  filter(pixelID %in% modData$pixelID)

A1_points %>% dplyr::select(pixelID, x, y) %>% write_csv(file = "~/cloud/gdrive/fire_project/postfire_regen/analysis2/A1_data_with_A2_domain/A1_points.csv")
