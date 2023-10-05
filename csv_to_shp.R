library(sf)
library(tidyverse)

fireYearConCov <- read_csv("~/cloud/gdrive/fire_project/data_091722/fireYearConCov.csv")

fireYearConCov_sp <- st_as_sf(fireYearConCov, coords = c("x", "y"), crs = "EPSG:3310")


st_write(obj = fireYearConCov_sp, dsn = "~/cloud/gdrive/fire_project/data_091722/fire_year_con_cov_100522.shp")


