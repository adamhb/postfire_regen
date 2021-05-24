#import libraries
library(tidyverse)
library(parallel)
#library(raster)
#library(sf)
#library(rgdal)
#library(ggcorrplot)
source('utils/system_settings.R') # source internal functions and settings

#set path to data
#path <- "/home/rstudio/data/"
path <- ("~/cloud/gdrive/fire_project/local_data/fromGEE/")

files <- list.files(path,pattern = "csv")
#outPath <- "/home/rstudio/figures/"

#set parameters
minPixPerPatch <- 100

#join data from different focal areas
df <- tibble()

for(f in files){
              df_tmp <- read_csv(paste0(path,f))
              focalAreaID <- df_tmp$focalAreaID[1]
              pixelID <- paste(focalAreaID,df_tmp$pixelID, sep = "_")
              df_tmp$pixelID <- pixelID
              df <- rbind(df,df_tmp)
}


#record focal areas
focal_areas <- unique(df$focalAreaID)

#cleaning NOT DONE
df <- df %>% rename(patchID = PatchID, fireYear = FireYear) %>%
  mutate_at(.vars = c("patchID","focalAreaID"),.funs = as.character) ##make IDs characters
  
pixelsPerPatch <- df %>% #pixels per patch
  group_by(patchID) %>%
  summarise(nPerPatch = length(pixelID))

#filter to patches with more than minPixPerPatch number of pixels
PatchesIN <- pixelsPerPatch %>% filter(nPerPatch >= minPixPerPatch) %>% 
  pull(patchID) #could sample patches randomly here

df <- df %>% filter(patchID %in% PatchesIN)


pixelIDs <- unique(timeVaryingDF$pixelID)

########################################
summary_stats <- function(data){
  
  n_focal_areas <- length(unique(data$focalAreaID))
  n_patches <- length(unique(data$patchID))
  n_pixels <- length(unique(data$pixelID))
  area <- n_pixels * (60*60) / 1e4 #hectares
  output <- tibble(stat = c("n Focal Areas","n Patches","n Pixels","Area (ha)"),
                   value = c(n_focal_areas,n_patches,n_pixels,area))
  return(output)
}
summary_stats(df)

#function to get the time trajectories of time-varying variables
GetTimeTraj <- function(d, varOfInterest, pattern = "[:digit:]{4}(?=0)"){
  
  timeTraj <- d %>% 
    dplyr::select(pixelID, patchID, focalAreaID, fireYear, contains(varOfInterest)) %>% 
    gather(contains(varOfInterest),key = "year",value = varOfInterest) %>%
    mutate(year = flatten_chr(str_extract_all(year,pattern))) %>%
    arrange(pixelID,year) %>% 
    rename(!!sym(x = eval(varOfInterest)) := varOfInterest)
  
  return(timeTraj)
}

conProbDF <- GetTimeTraj(d = df,varOfInterest = "ConProb")
seedDF <- GetTimeTraj(d = df,varOfInterest = "SAP") %>% dplyr::select(pixelID,year,SAP)
managementDF <- GetTimeTraj(d = df,varOfInterest = "management",pattern = "[:digit:]{4}") %>% dplyr::select(pixelID,year,management)

timeVaryingDF <- conProbDF %>%
  left_join(seedDF,by = c("pixelID","year")) %>%
  left_join(managementDF, by = c("pixelID","year")) %>%
  mutate_at(.vars = "year",.funs = as.numeric) %>%
  mutate(timeSinceFire = year-fireYear)


GetTimeVarStat <- function(pixel, d, varOfInterest,relYrs,stat = "mean") {
  
  if(stat == "mean"){f = function(x){mean(x)}}
  if(stat == "sum"){f = function(x){sum(x)}}
  if(stat == "max"){f == function(x){max(x)}}
  
  fireYear <- d[d$pixelID == pixel,]$fireYear #get fire year of pixel
  output <- d %>%
    filter(timeSinceFire %in% relYrs,
           pixelID == pixel) %>%
    pull(varOfInterest) %>% f()
  
  return(output)
}


#calculating pixel-level metrics from time-varying variables
numCores <- detectCores()
start_time <- Sys.time()
preFireConProb <- mclapply(X = pixelIDs,
       FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "ConProb",relYrs = c(-1,-2)) %>% flatten_dbl()
postFireConProb <- mclapply(X = pixelIDs,
       FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "ConProb",relYrs = c(1,2)) %>% flatten_dbl()
postFireSAP <- mclapply(X = pixelIDs,
       FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "SAP",relYrs = c(1,2,3)) %>% flatten_dbl()
postFirePlanting <- mclapply(X = pixelIDs,
                        FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "management",relYrs = 1:6) %>% flatten_dbl()
end_time <- Sys.time()
print(end_time-start_time)

#calculated time metrics (time stable)


timeVaryingDF2 <- tibble(pixelID = pixelIDs, 
       preFireConProb = preFireConProb,
       postFireConProb = postFireConProb,
       postFireSAP = postFireSAP,
       postFirePlanting = postFirePlanting) %>% 
  mutate_at(.vars = 'postFirePlanting', .funs = function(x){x > 0}) %>%
  right_join(timeVaryingDF, by = "pixelID") %>%
  mutate(disturbanceSize = postFireConProb - preFireConProb,
         ARI = ConProb - postFireConProb,
         RRI = ARI / deltaConProb) 
  
stableVars <- df %>% dplyr::select(pixelID,patchID,focalAreaID,fireYear,
                                   CWDhist,PPThist,PPThistSD,T_meanHist,T_meanSD,
                                   SolarLoad,aspect,eastness,northness,slope,elevation,
                                   burnSev,wilderness)

#PICK UP HERE









#joinging all vars back to recovery DF
df2 <- timeVaryingDF2 %>% 
  left_join(stableVars, by = "pixelID") %>% str()



flatten_dbl(lapply(X = unique(timeVaringDF$year),FUN = function(x){x+1}))


timeVaryingDF %>%
  mutate(preFire = pmap(list(pixelID,ConProb,c(-2,-1)),
                        d = .,
                        .f = GetTimeVarStat))

GetTimeVarStat(pixel = df$pixelID[1],d = timeVaringDF,varOfInterest = "ConProb",relYrs = c(-2,-1),stat = function(x){mean(x)})



#function to calculate pre-fire ConProb
getPreDisturbance <- function(d, pixel){
  
  
  preDisturbance <- d %>%
    filter(pixelID == pixel,
           year %in% c(fireYear-1,fireYear-2)) %>%
    pull(ConProb) %>% mean()
  return(preDisturbance)
}

str(df2)

#function to calculate delta ConProb
d <- head(recovery,100)
pixel <- recovery$pixelID[1]



disturbanceSize <- function(d, pixel){
  #get fire year
  fireYear <- d[d$pixelID == pixel,]$FireYear

  preFireConProb <- preFireConProbFunc(recov_data = recov_data,pixelID.x = pixelID.x)
  
  postFireConProb <- recov_data %>%
    filter(pixelID == pixelID.x,
           year %in% c(FireYear+1, FireYear+2)) %>%
    pull(ConProb) %>% mean()
  
  return(preFireConProb - postFireConProb)
}


pixelIDvec <- unique(df$pixelID) #[1:numPix] 
start_time <- Sys.time()
deltaConProbDF <- tibble()

for(i in 1:length(pixelIDvec)){
 
  p <- pixelIDvec[i]
  tmp <- tibble(pixelID = p,
                deltaConProb = deltaConProbFunc(recov_data = recovery, pixelID.x = p),
                preFireConProb = preFireConProbFunc(recov_data = recovery, pixelID.x = p))
  
  deltaConProbDF <- rbind(deltaConProbDF,tmp)
  print(i/length(pixelIDvec)*100)
}
end_time <- Sys.time()
print(paste(end_time - start_time, "test1"))


recovery1 <- recovery %>%
  left_join(deltaConProbDF, by = "pixelID") %>% 
  filter(deltaConProb > 20) %>%
  drop_na(deltaConProb) %>%
  mutate(postFireConProb = preFireConProb - deltaConProb) %>%
  mutate(ARI = ConProb - postFireConProb) %>%
  mutate(RRI = ARI / deltaConProb) %>% arrange(pixelID,year)




RRIoverTime <- recovery1 %>%
  group_by(PatchID,year) %>%
  summarise(RRI = mean(RRI)) %>%
  ggplot(aes(year,RRI,color=PatchID)) +
  geom_line() +
  adams_theme #+
  #theme(legend.position = "none")

#view histogram of patch-level RRI at year 20
recovery1 %>%
  group_by(PatchID,year) %>%
  filter(timeSinceFire == 20) %>%
  summarise(RRI = mean(RRI)) %>%
  pull(RRI) %>% hist()
  
RRIyr20 <- recovery1 %>%
  filter(timeSinceFire == 20) %>% dplyr::select(pixelID,RRI) %>%
  rename(RRI_yr20 = RRI)

recovery2 <- recovery1 %>%
  left_join(RRIyr20,by = "pixelID")
  

#function to create summary variables of time trajectory data


SAP <- timeTrajFunc(data = df, codeNameFile = "tmp/codeNameSAP.csv",varOFinterest = "SAP")
SAP <- SAP %>%
  rename(SAP = varOFinterest)

SAPmeanF1_3 <- function(data, pixelID.x){
  FireYear <-  data %>%
    filter(pixelID == pixelID.x) %>% pull(FireYear) %>% unique() %>% head(1)
  
  SAPF1_3 <- data %>%
    filter(pixelID == pixelID.x,
           year %in% c(FireYear+1, FireYear+2, FireYear+3)) %>%
    pull(SAP) %>% mean()
  
  return(SAPF1_3)
}

start_time <- Sys.time()
SAP_Yr1_3_DF <- tibble()
for(i in 1:length(pixelIDvec)){
  
  p <- pixelIDvec[i]
  tmp <- tibble(pixelID = p,
                SAPYr1_3 = SAPmeanF1_3(data = SAP,pixelID.x = p))
  
  SAP_Yr1_3_DF <- rbind(SAP_Yr1_3_DF,tmp)
  print(i/length(pixelIDvec)*100)
}
end_time <- Sys.time()
print(paste(end_time - start_time, "test2"))


#write_csv(tibble(grep(names(df),pattern = "ppt",value = T)),path = "tmp/ppt_code.csv")

PPT <- timeTrajFunc(data = df, codeNameFile = "tmp/ppt_code_new.csv",varOFinterest = "ppt")
PPT <- PPT %>%
  rename(PPT = varOFinterest) %>%
  mutate()


GrabVarByFireYear <- function(data, pixelID.x,varOfInterest,YrsRel2Fire,stat = function(x){max(x)}){
 
  FireYear <-  data %>%
    filter(pixelID == pixelID.x) %>% pull(FireYear) %>% unique() %>% head(1)
  
  output <- data %>%
    filter(timeSinceFire %in% YrsRel2Fire,
           pixelID == pixelID.x) %>%
    pull(varOfInterest) %>% stat()
  return(output)
}

# GrabVarByFireYear(data = PPT %>% filter(period == "JanSept"),
#                   pixelID.x = "3_2_3",
#                   varOfInterest = "PPT",
#                   YrsRel2Fire = c(1,2,3),stat = function(x){mean(x)})

  start_time <- Sys.time()
  PPT_DF <- tibble()
  for(i in 1:length(pixelIDvec)){
    
    p <- pixelIDvec[i]
    tmp <- tibble(pixelID = p,
                  JanSeptSum = GrabVarByFireYear(data = PPT %>% filter(period == "JanSept"),
                                                    pixelID.x = p,
                                                    varOfInterest = "PPT",
                                                    YrsRel2Fire = c(1,2,3),stat = function(x){sum(x)}),
                  OctNovDecSum = GrabVarByFireYear(data = PPT %>% filter(period == "OctDec"),
                                                   pixelID.x = p,
                                                   varOfInterest = "PPT",
                                                   YrsRel2Fire = c(0,1,2),stat = function(x){sum(x)})) %>%
      mutate(PPT_Yr0_3_mean = (JanSeptSum + OctNovDecSum) /3)
    
    PPT_DF <- rbind(PPT_DF,tmp)
    
    print(i/length(pixelIDvec)*100)
  }
  
  end_time <- Sys.time()
  print(paste(end_time - start_time, "test3"))


  PPT_DF <- PPT_DF %>% dplyr::select(pixelID,PPT_Yr0_3_mean)
#planted in first 6 years
  #write_csv(tibble(grep(names(df),pattern = "management",value = T)),path = "tmp/management_code.csv")
  
planted <- timeTrajFunc(data = df,codeNameFile = "tmp/management_code_new.csv",varOFinterest = "management")

planted <- planted %>% rename(planted = varOFinterest)

table(planted$planted)

plantedDF <- tibble() 

start_time <- Sys.time()
for(i in 1:length(pixelIDvec)){
  p <- pixelIDvec[i]
  tmp <- tibble(pixelID = p,
                tmp = GrabVarByFireYear(data = planted,pixelID.x = p,
                                        varOfInterest = "planted",
                                        YrsRel2Fire = c(1,2,3,4,5,6),
                                        stat = function(x){max(x)}),
                plantedYr0_6 = tmp > 0) %>%
    dplyr::select(-tmp)
  
  plantedDF <- rbind(plantedDF,tmp)
  print(i/length(pixelIDvec)*100)
}
  
end_time <- Sys.time()
print(paste(end_time - start_time, "test4"))
  


#non-time varying variables 
stableVars <- df %>% dplyr::select(pixelID,PatchID,focalAreaID,FireYear,
                     CWDhist,PPThist,PPThistSD,T_meanHist,T_meanSD,
                     SolarLoad,aspect,eastness,northness,slope,elevation,
                     burnSev,wilderness)

#joinging all vars back to recovery DF
df2 <- recovery2 %>% 
  left_join(SAP_Yr1_3_DF,by = "pixelID") %>%
  left_join(plantedDF, by = "pixelID") %>%
  left_join(PPT_DF, by = "pixelID") %>% 
  left_join(stableVars)

print(summary_stats(df2))


crossPatchMean <- df2 %>%
  group_by(timeSinceFire) %>%
  summarise(RRI = mean(RRI))

#patch level stats
df3 <- df2 %>%
  group_by(PatchID,year) %>%
  mutate_at(.vars = "plantedYr0_6",.funs = as.numeric) %>%
  summarise_if(.predicate = is.numeric, .funs = mean)

RRI_Plot1 <- df3 %>% ggplot(aes(timeSinceFire,RRI,color = PatchID)) +
  geom_line() +
  geom_line(data = crossPatchMean, mapping = aes(timeSinceFire,RRI), color = "black", size = 3) +
  adams_theme +
  theme(legend.position = "none")

makePNG(fig = RRI_Plot1,path_to_output.x = outPath,file_name = "NoStratification")

SAP_quantiles <- quantile(df3$SAPYr1_3, probs = c(0.2,0.8))
PPT_quantiles <- quantile(df3$PPT_Yr0_3_mean, probs = c(0.2,0.8))

df4 <- df3 %>%
mutate(SeedAvail = case_when(
  SAPYr1_3 <= SAP_quantiles[1] ~ "low seed avail.",
  SAPYr1_3 >= SAP_quantiles[2] ~ "high seed avail.",
  (SAPYr1_3 > SAP_quantiles[1]) & (SAPYr1_3 < SAP_quantiles[2]) ~ "medium seed avail."
)) %>%
  mutate(PostFirePPT = case_when(
    PPT_Yr0_3_mean <= PPT_quantiles[1] ~ "low ppt Yrs 0-3",
    PPT_Yr0_3_mean >= PPT_quantiles[2] ~ "high ppt Yrs 0-3",
    (PPT_Yr0_3_mean > PPT_quantiles[1]) & (SAPYr1_3 < PPT_quantiles[2]) ~ "medium ppt Yrs 0-3"
  )) %>% mutate(SeedAvail2 = factor(SeedAvail,levels = c("high seed avail.","medium seed avail.","low seed avail."))) %>%
  mutate(PostFirePPT2 = factor(PostFirePPT,levels = c("high ppt Yrs 0-3","medium ppt Yrs 0-3","low ppt Yrs 0-3"))) %>%
  mutate(planted = case_when(
    plantedYr0_6 >= 0.5 ~ TRUE,
    plantedYr0_6 < 0.5 ~ FALSE
  ))


RRI_plot2 <- df4 %>% ggplot(aes(timeSinceFire,RRI,color = PatchID,linetype = planted)) +
  geom_line() +
  facet_grid(rows = vars(SeedAvail2), cols = vars(PostFirePPT2),drop = FALSE) +
  scale_colour_discrete(guide = FALSE) 
  #theme(legend.position = "none")
  
makePNG(fig = RRI_plot2,path_to_output.x = outPath,file_name = "WithStratification",res = 600)

write_csv(tibble(unlist(summary_stats(df2))),path = paste0("tmp/metaData",Sys.time() %>% 
                                                             sub(pattern = ":", replacement = "-") %>%
                                                             sub(pattern = ":", replacement = "-") %>%
                                                             sub(pattern = " ", replacement = "-"),".csv"))

write_csv(df4,path = paste0("tmp/Data",Sys.time() %>% 
                              sub(pattern = ":", replacement = "-") %>%
                              sub(pattern = ":", replacement = "-") %>%
                              sub(pattern = " ", replacement = "-"),".csv"))



#exploring variables: correlations
names(df4)[11] <- "RRIYR20"

corr_data <- df4 %>% 
  summarise_if(.predicate = is.numeric, .funs = mean) %>%
  ungroup() %>%
  dplyr::select(-FireYear,-year,-ConProb,-ARI,-RRI,-timeSinceFire) %>%
  select_if(.predicate = is.numeric) %>%
  dplyr::select(RRIYR20,SAPYr1_3,slope,plantedYr0_6,burnSev,elevation,PPThist,PPT_Yr0_3_mean) 

corr <- corr_data %>% cor() %>% round(1)
Pmat <-  corr_data %>% cor_pmat()

ggcorrplot(corr, hc.order = TRUE, type = "lower",
           lab = TRUE, p.mat = Pmat) +
  adams_theme +
  theme(legend.position = "none",
        axis.text.x.bottom  = element_text(angle = 90, vjust = 0.5, hjust=1))


#exploring variables: discrete variables
df4 %>%
  summarise_if(.predicate = is.numeric, .funs = mean) %>%
  ungroup() %>%
  ggplot(aes(plantedYr0_6 * 100,RRIYR20)) +
  ylab(label = "RRI at Year 20") +
  xlab(label = "% of Patch Planted After Fire") +
  geom_point() +
  adams_theme









