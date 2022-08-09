
RRI_mean <- df4 %>% pull(RRI_end) %>% mean()
RRI_sd <- df4 %>% pull(RRI_end) %>% sd()



mod <- readRDS("data/old_mods/mod_2-2022-04-02 16:37:11.RDS")

summary(mod)



library(sf)

studyareas <- st_read('~/cloud/gdrive/fire_project/local_data/focalAreas')
studyareas %>% filter(id %in% df4 %>% pull(pixelID))

studyAreasIncluded <- studyareas[studyareas$id %in% focalAreasIn,]
st_write(studyAreasIncluded,"~/cloud/gdrive/fire_project/local_data/focalAreas/includedInDissertation.shp")


modData <- read_csv('data/mod_3-2022-04-02 17:13:55data.csv')
nrow(modData)


modData <- read_csv("data/mod"



modDataPixels <- modData %>% pull(pixelID)

df4 %>% filter(pixelID %in% modDataPixels) %>%
  filter(TmaxHist > 21.1)

fv <- predict(mod,type="terms") ## get term estimates
## compute partial residuals for first smooth...          
prsd1 <- residuals(mod,type="working") + fv[,8]
plot(mod,select=8) ## plot first smooth
ind <- sample(1:length(prsd1),100)
points(dat$x0[ind],prsd1[ind],pch=19,col="pink") 


rarefiedData <- read_csv("data/modDataSub_StewartModel.csv")
snowPack <- df4 %>% select(pixelID,snowPack)

rarefiedData <- rarefiedData %>% left_join(snowPack, by = c("pixelID")) 
write_csv(rarefiedData,"data/rarefiedData.csv")

df2 %>% filter(focalAreaID == "1") %>% dplyr::select(pixelID,x,y) %>% distinct() %>% filter(pixelID == "FA-1-100")
  
  
  left_join(snowtest, by = c("x","y"))

snowtest <- read_csv('~/cloud/gdrive/fire_project/local_data/analysis/snowPack.csv')
snowtest %>% filter(pixelID == "FA-1-100")

str(modData)
temp <- modData %>% select(pixelID, focalAreaID, x, y) %>%
  filter(focalAreaID == 5)
coordinates(temp) <- ~ x + y
temp <- st_as_sf(temp)
st_crs(temp) <- CRS(SRS_string = "EPSG:3310")

plot(st_sample(x = temp, size = 1))

plot(temp)


modDataSub <- tibble()
sampleSize <- 45
for(fa in unique(modData$focalAreaID)){
  d <- modData %>% filter(focalAreaID == fa)
  if(nrow(d) < sampleSize){
    tmp <- d
  }else{
    tmp <- sample_n(d,size = sampleSize)
  }
  modDataSub <- rbind(modDataSub,tmp)
}

coordinates(modDataSub) <- ~ x + y 


library(sp)


data(meuse)
coordinates(meuse) <- ~x+y 
meuse

sf::st_sample(meuse, size = 3)

meuse@data
dmat <- spDists(meuse)

min.dist <- 500 
dmat[dmat <= min.dist] <- NA

samples <- data.frame(ID=rownames(meuse@data), kNN=NA)
for(i in 1:nrow(dmat) ) {
  x <- as.vector( dmat[,i] )
  names(x) <- samples$ID
  x <- x[!is.na(x)]
  if(!length(x) == 0) {
    samples[i,][2] <- names(x)[sample(1:length(x), 1)]
  } else {
    samples[i,][2] <- NA
  }   
}




ozone <- read.table("https://stats.idre.ucla.edu/stat/r/faq/ozone.csv", sep=",", header=T)
head(ozone, n=10)

ozone.dists <- as.matrix(dist(cbind(ozone$Lon, ozone$Lat)))
ozone.dists.inv <- 1/ozone.dists
diag(ozone.dists.inv) <- 0
as.vector(ozone.dists[1:5, 1:5])
Moran.I(ozone$Av8top,ozone.dists.inv)

coordinates(modData) <- ~ x+y
modData$residuals <- residuals(mod)
bubble(modData, zcol='residuals', fill=TRUE, do.sqrt=FALSE, maxsize=3)

TheVariogram=variogram(residuals~1, data=)
plot(TheVariogram)



pixel.dists <- as.matrix(dist(cbind(modData$x[1:5], modData$y[1:5])))

TheVariogram <- Variogram(modData$residuals, as.vector(pixel.dists))
plot(TheVariogram)

plot(TheData$x,TheData$y) 












tail(df$`system:index`,100)
write_csv(tibble(names(df)),"tmp/new_names.csv")

head(df$`2_LT05_041034_19920620_ConProb`,300)


#import data from GEE
path <- ("~/cloud/gdrive/fire_project/local_data/fromGEE/")
files <- list.files(path,pattern = "tif")
old2new_names <- read_csv(file = "tmp/names.csv")


rast <- stack("~/cloud/gdrive/fire_project/local_data/fromGEE/FocalArea3_4_25_2021.tif")

as.numeric(system("awk '/MemFree/ {print $2}' /proc/meminfo",intern=TRUE))/1e6

rast <- raster::stack("~/test/FocalArea3_4_25_2021.tif")
#clean data
n_bands <- length(names(rast))
n_pixels <- dim(rast[[1]])[1] * dim(rast[[1]])[2]

years <- as.character(1984:2020)

df_tmp <- tibble(pixelID = 1:n_pixels)
for(i in 1:n_bands){
  t <- tibble(raster::values(rast[[i]])) #make faster? ignore empty cells?
  names(t) <- names(rast[[i]])
  df <- cbind(df_tmp,t)
  print(paste("done with band", i, "of", n_bands))
}




test <- as.data.frame(rast, na.rm = TRUE)



str(df)
#drop pixels that are not in the study

#ID Vars
IDvars <- c("pixelID","PatchID","focalAreaID")



#creates a new DF that shows how a time-varying variable (user-defined) changes over time for each pixel
CreateTimeTrajDF <- function(inputDF, timeVar){
  
  assign(x = timeVar,value = eval(as.name(timeVar)))
  
  df %>% select(IDvars, fireYear) %>%
    gather() %>%
    mutate(year2 = case_when(
      
    ))
  
  
  
}








names(rast[[164]])

summary(values(rast[[165]]))

plot(rast[[46]])








names(rast)

#explore and visualize data
df %>%
  drop_na(PatchID) %>%
  mutate_at(.vars = "PatchID", .funs = as.character) %>%
  group_by(PatchID) %>%
  summarise(n = length(PatchID)) %>% pull(n) %>% hist()

#create recovery trajectories
Recovery <- df %>%
  dplyr:: select(pixelID,FireYear,89:125) %>% 
  drop_na(ConProb2013) %>% 
  gather(ConProb2013:ConProb1998, key = "year",value = "ConProb") %>%
  mutate_at(.vars = "year",.funs = function(x){as.integer(substr(x,start = 8,stop = 12))} ) %>%
  arrange(pixelID,year)

#figure of recovery trajectory
Recovery %>% 
  filter(FireYear == 1992) %>%
  group_by(year) %>%
  summarise(ConProb = mean(ConProb),
            SD = sd(ConProb)) %>%
  ggplot(aes(year,ConProb)) +
  geom_line() +
  adams_theme


#pixel planted






















write_csv(tibble(grep(names(df),pattern = "SAP",value = T)),path = "tmp/codeNameSAP.csv")

















old_names <- names(df) %>% tibble() 
names(old_names) <- "old_name"

old_names %>%
  mutate(col_names = case_when(
    str_detect(string = old_name, pattern = "SAP") == TRUE ~ na.omit(unlist(str_extract_all(string = old_name,years))),
    TRUE ~ old_name
  ))


write_csv(tibble(names(rast)),path = "tmp/names.csv")

old_name <- ("X1_1_LC08_041034_20130630_SAP")



str_extract_all(string = "X1_1_LC08_041034_20130630_SAP",years) %>%
  unlist() %>%
  na.omit()


str_subset(string = "X1_1_LC08_041034_20130630_SAP", pattern = years)
as.character(na.omit(str_extract(string = "X1_1_LC08_041034_20130630_SAP", pattern = years)))
str_extract_all(string = "asdf", pattern = "SAP") 18:21

\_\d{4}
str_extract("X1_1_LC08_041034_20130630_SAP","[:digit:]")


na.omit(str_extract(string = "X1_1_LC08_041034_20120630_SAP", pattern = c("2012","2013")))


df <- data.frame(id = 1:10, a = 1:10, b = 11:20, c = 21:30)

lst_result <- apply(df, 1, function(x){
  var1 <- (x[['a']] + x[['b']])
  var2 <- x[['c']]/2
  return(data.frame(var1 = var1, var2 = var2))
})


df %>%
mutate(d = pmap_dbl(.l = list(b, c), 
                   function(b, c) {b + c})
)




a <- c(1,2,3)
b <- c(4,5,6)
c <- c(7,8,9)

al <- list(a,b,c)

d <- tibble(a = a, b = b, c = c)

d %>%
  rowwise() %>%
  mutate(d = map_dbl(list(a,b,c),.f = sum))

map_dbl(d, function(x) {sum(x)} )










