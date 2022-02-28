library(tidyverse)
library(sf)
d <- read_csv("~/cloud/gdrive/fire_project/local_data/classificationData/pct_cover_training_feb_2022/pct_cover_data.csv")

#calculate pct cover of each plot
pfts <- names(table(d$pft))

sample_sizes <- d %>%
  group_by(fid) %>%
  summarise(n = length(pft)) %>%
  pull(n) %>% summary()

print(paste("sample size ranged from",sample_sizes[1],"to",sample_sizes[6], "with a median of",sample_sizes[3]))

getPctCover <- function(df,pft = "c"){
  x <- table(df$pft)
  n_total <- sum(x)
  pct_cover <- x/n_total
  pct_cov_all_pfts <- pct_cover[pft]
  names(pct_cov_all_pfts) <- pft
  pct_cov_all_pfts[is.na(pct_cov_all_pfts)] <- 0
  output <- tibble(pft = pft,
                   pct_cover = as.numeric(pct_cov_all_pfts))
  return(output)
}

getPctCover(df = d,pft = pfts)

hiRez_pct_cover_data <- tibble()
for(p in unique(d$fid)){
  tmp <- getPctCover(df = d %>% filter(fid == p), pft = pfts)
  tmp <- tmp %>% add_column(fid = p) 
  hiRez_pct_cover_data <- rbind(hiRez_pct_cover_data,tmp)
}

hiRez_pct_cover_data2 <- hiRez_pct_cover_data %>%
  select(fid,pft,pct_cover) %>%
  rename(pctCover = pct_cover) %>%
  filter(pft == "c")

hist(hiRez_pct_cover_data2$pctCover)

trainingAreaDates <- read_csv('~/cloud/gdrive/fire_project/local_data/classificationData/pct_cover_training_feb_2022/trainingAreaDate_firstBatch.csv') %>% select(fid,year)

pctCovConHiRez <- hiRez_pct_cover_data2 %>%
  left_join(trainingAreaDates) %>%
  mutate(fid2 = paste0("F",fid)) %>%
  select(-fid, -pft) %>%
  rename(fid = fid2) %>%
  select(fid, year, pctCover) %>%
  add_column(new = 1)

nrow(pctCovConHiRez)
write_csv(pctCovConHiRez,"data/pctCovConHiRez.csv")
#join these data to the polygons
polys <- st_read('~/cloud/gdrive/fire_project/local_data/classificationData/pct_cover_polygons_2_28_2022.shp') %>% st_zm(polys, drop = TRUE, what = "ZM")


pctCovData <- polys %>%
  left_join(pctCovConHiRez) %>%
  drop_na(pctCover)
st_write(pctCovData,dsn = "data/pctCoverData_2_28_2022.shp",driver = "ESRI Shapefile")

