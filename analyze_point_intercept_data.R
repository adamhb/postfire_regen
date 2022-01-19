library(tidyverse)
library(exifr)
library(sf)
newgeodata <- F

options(max.print = 1000)
options(dplyr.print_max = 1e3)

raw <- read_csv('~/cloud/gdrive/fire_project/local_data/field_validation/PointInterceptData/pointInterceptData.csv') 

#Clean the point intercept data
df <- raw %>%
  mutate_at(.vars = "pft",.funs = tolower) %>% 
  mutate_at(
    vars(starts_with("pft")),
    funs(case_when(
      . == "br" ~ "r",
      . == "p" ~ "b",
      TRUE ~ .
    ))
  ) %>% rename(pointID = photoID)


#Check that there are no points attributed to the wrong plot
#bring in the coordinates of each photo

getCoordsFromExifFile <- function(file){
  dat <- read_exif(path = file)
  x <- strsplit(x = dat$GPSPosition,split = " ")[[1]][1]
  y <- strsplit(x = dat$GPSPosition,split = " ")[[1]][2]
  return(tibble(pointID = dat$FileName, x = x, y = y))
}

getCoordsFromPoint <- function(plotID = "B0"){
  #browser()
  photo_path <- paste0("/media/adam/EMTEC B250/SortedPoints/",plotID)
  files <- paste0(photo_path,"/",list.files(path = photo_path,pattern = "JPG"))
  output <- tibble()
  j <- 0
  for(i in files){
    tmp <- getCoordsFromExifFile(file = i)
    tmp <- tmp %>% add_column(plotID = plotID)
    output <- rbind(output,tmp)
    j <- j + 1
    print(paste(j,"of",length(files),"photos"))
  }
  return(output)
}


if(newgeodata == T){
  points <- basename(list.dirs(photo_path))[-c(1,28)]
  geo_data <- tibble()
  j <- 0
  for(p in points){
    j <- j + 1
    tmp <- getCoordsFromPoint(plotID = p)
    geo_data <- rbind(geo_data,tmp)
    print(paste(j,"of",length(points),"points"))
  }
  write_csv(x = geo_data,"data/geo_data_point_intercept.csv")
}else{
  geo_data <- read_csv(file = "data/geo_data_point_intercept.csv")
}

#check that all photo points are in the same plot
#create spatial object
geo_data2 <- st_as_sf(geo_data, coords = c("x","y"))
geo_data3 <-st_set_crs(geo_data2,st_crs(4326))

#map points from each validation plot to check that all points are in
#a systematic grid.
geo_data3 %>%
  filter(plotID == unique(geo_data3$plotID)[26]) %>% plot()

 
#calculate pct cover of each plot
pfts <- names(table(df$pft))

sample_sizes <- df %>%
  group_by(plotID) %>%
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

fieldValidationData <- tibble()
for(p in unique(df$plotID)){
  tmp <- getPctCover(df = df %>% filter(plotID == "B0"), pft = pfts)
  tmp <- tmp %>% add_column(plotID = p)
  fieldValidationData <- rbind(fieldValidationData,tmp)
}

#join point intercept data with line intercept data









