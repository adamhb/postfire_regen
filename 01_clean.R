#import libraries
library(tidyverse)
library(raster)
library(sf)
library(rgdal)
source('utils/system_settings.R')

#set path to data
path <- ("~/cloud/gdrive/fire_project/local_data/fromGEE/")
files <- list.files(path,pattern = "csv")
outPath <- "~/cloud/gdrive/fire_project/figures/fromR/"


numPix <- 9000
#join data from different focal areas
df <- tibble()
for(f in files){
              df_tmp <- read_csv(paste0(path,f))
              focalAreaID <- df_tmp$focalAreaID[1]
              pixelID <- paste(focalAreaID,df_tmp$`system:index`, sep = "_")
              df_tmp$pixelID <- pixelID
              df <- rbind(df,df_tmp)
}

#record focal areas
focal_areas <- unique(df$focal_area_code)

#check that focalArea pixels match file name
read_csv("~/cloud/gdrive/fire_project/local_data/fromGEE/FocalArea30.csv") %>%
  pull(focalAreaID) %>% head()

#cleaning NOT DONE
df <- df %>% mutate_at(.vars = c("PatchID","focalAreaID"),.funs = as.character) ##make IDs characters

#descriptive statistics
pixelsPerPatch <- df %>% #pixels per patch
  group_by(PatchID) %>%
  summarise(nPerPatch = length(pixelID))

PatchesIN <- pixelsPerPatch %>% filter(nPerPatch > 100) %>% 
  #could sample patches randomly here
  pull(PatchID)

df <- df %>% filter(PatchID %in% PatchesIN)


########################################
summary_stats <- function(data){
  n_focal_areas <- length(unique(data$focalAreaID))
  n_patches <- length(unique(data$PatchID))
  n_pixels <- length(unique(data$pixelID))
  area <- n_pixels * (60*60) / 1e4 #hectares
  return(list(n_focal_areas,n_patches,n_pixels,area))
}

summary_stats(df2)




########################################
#create recovery trajectories per pixel#
########################################
conPcode <- read_csv("tmp/code_conP.csv") #import names code

recovery <- df %>% dplyr::select(pixelID, PatchID, focalAreaID, FireYear, contains("ConProb")) %>% 
  gather(contains("ConProb"),key = "year",value = "ConProb") 
recovery <- recovery %>%
  left_join(conPcode, by = "year") %>%
  dplyr::select(-year) %>%
  rename(year = newyear) %>%
  mutate(timeSinceFire = year-FireYear)

#function to calculate pre-fire ConProb
preFireConProbFunc <- function(recov_data, pixelID.x){
  FireYear <-  recov_data %>%
    filter(pixelID == pixelID.x) %>% pull(FireYear) %>% unique() %>% head(1)
  
  ConProb2yrsPrior <- recov_data %>%
    filter(pixelID == pixelID.x,
           year %in% c(FireYear-1,FireYear-2)) %>%
    pull(ConProb) %>% mean()
  return(ConProb2yrsPrior)
}


#function to calculate delta ConProb
deltaConProbFunc <- function(recov_data, pixelID.x){
  FireYear <-  recov_data %>%
    filter(pixelID == pixelID.x) %>% pull(FireYear) %>% unique() %>% head(1)
  
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
timeTrajFunc <-  function(data, codeNameFile, varOFinterest){
    
  
  Code <- read_csv(codeNameFile) #import names code
  
  timeTraj <- data %>% 
    dplyr::select(pixelID, PatchID, focalAreaID, FireYear, contains(varOFinterest)) %>% 
    gather(contains(varOFinterest),key = "year",value = varOFinterest) 
  timeTraj <- timeTraj %>%
    left_join(Code, by = "year") %>%
    dplyr::select(-year) %>%
    rename(year = newyear) %>%
    mutate(timeSinceFire = year-FireYear)
  
  return(timeTraj)

  }

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




