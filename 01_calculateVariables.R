source('00_setup.R')

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

if(light_run == T){
  PatchesIN <- sample(x = PatchesIN,size = 10)
}

df <- df %>% filter(patchID %in% PatchesIN)

pixelIDs <- unique(df$pixelID)


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
focalAreasList <- unique(df$focalAreaID)
print('focalAreas:')
print(focalAreasList)









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


#make time trajectories of time-varying variables
conProbDF <- GetTimeTraj(d = df,varOfInterest = "ConProb")

seedDF <- GetTimeTraj(d = df,varOfInterest = "SAP") %>% dplyr::select(pixelID,year,SAP)

managementDF <- GetTimeTraj(d = df,varOfInterest = "management",pattern = "[:digit:]{4}") %>% dplyr::select(pixelID,year,management)

pptOctNovDecDF <- GetTimeTraj(d = df,varOfInterest = "10_ppt",pattern = "[:digit:]{4}")
names(pptOctNovDecDF)[6] <- "pptOctNovDec"
pptOctNovDecDF <- pptOctNovDecDF %>%
  dplyr::select(pixelID,year,pptOctNovDec)

pptJanSeptDF <- GetTimeTraj(d = df,varOfInterest = "01_ppt",pattern = "[:digit:]{4}")
names(pptJanSeptDF)[6] <- "pptJanSept"
pptJanSeptDF <- pptJanSeptDF %>%
  dplyr::select(pixelID,year,pptJanSept)

tmaxJanSeptDF <- GetTimeTraj(d = df,varOfInterest = "01_tmax",pattern = "[:digit:]{4}")
names(tmaxJanSeptDF)[6] <- "tmaxJanSept"
tmaxJanSeptDF <- tmaxJanSeptDF %>%
  dplyr::select(pixelID,year,tmaxJanSept)

tmaxOctNovDecDF <- GetTimeTraj(d = df,varOfInterest = "10_tmax",pattern = "[:digit:]{4}")
names(tmaxOctNovDecDF)[6] <- "tmaxOctNovDec"
tmaxOctNovDecDF <- tmaxOctNovDecDF %>%
  dplyr::select(pixelID,year,tmaxOctNovDec)




#adding all time varying variables into the same DF
timeVaryingDF <- conProbDF %>%
  left_join(seedDF,by = c("pixelID","year")) %>%
  left_join(managementDF, by = c("pixelID","year")) %>%
  left_join(pptOctNovDecDF,by = c("pixelID","year")) %>%
  left_join(pptJanSeptDF, by = c("pixelID","year")) %>%
  left_join(tmaxJanSeptDF,by = c("pixelID","year")) %>%
  left_join(tmaxOctNovDecDF, by = c("pixelID","year")) %>%
  mutate_at(.vars = "year",.funs = as.numeric) %>%
  mutate(timeSinceFire = year-fireYear)





#function to calculate new time-invariant variables from the time-varying variables
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


###################################################################
#calculating time-invariant variables from time-varying variables##
###################################################################

numCores <- detectCores()
start_time <- Sys.time()

preFireConProb <- mclapply(X = pixelIDs,
       FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "ConProb",relYrs = c(-1,-2)) %>% flatten_dbl()

#hist(preFireConProb)

postFireConProb <- mclapply(X = pixelIDs,
       FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "ConProb",relYrs = c(1,2)) %>% flatten_dbl()
#hist(postFireConProb)

postFireSAP <- mclapply(X = pixelIDs,
       FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "SAP",relYrs = c(1,2,3)) %>% flatten_dbl()
#hist(postFireSAP)


postFirePlanting <- mclapply(X = pixelIDs,
                        FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "management",relYrs = 1:6) %>% flatten_dbl()

pptYr0_2_OctNovDec <- mclapply(X = pixelIDs,
                             FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, 
                             varOfInterest = "pptOctNovDec",relYrs = c(0,1,2), stat = "sum") %>% flatten_dbl()
#hist(PPTYr0_2_OctNovDec)


pptYr1_3_JanSept <- mclapply(X = pixelIDs,
                               FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "pptJanSept",relYrs = c(1,2,3)) %>% flatten_dbl()

tmaxYr1_3_JanSept <- mclapply(X = pixelIDs,
                             FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "tmaxJanSept",relYrs = c(1,2,3)) %>% flatten_dbl()

tmaxYr0_2_OctNovDec <- mclapply(X = pixelIDs,
                            FUN = GetTimeVarStat,mc.cores = numCores,d = timeVaryingDF, varOfInterest = "tmaxOctNovDec",relYrs = c(0,1,2)) %>% flatten_dbl()

#hist(tmax1_3_JanSept)
end_time <- Sys.time()
print(end_time-start_time)


#############################################################
###creating a DF of the calculated time-invariant variables##
#############################################################
calculatedTimeInvariant <- tibble(pixelID = pixelIDs, 
       preFireConProb = preFireConProb,
       postFireConProb = postFireConProb,
       postFireSAP = postFireSAP,
       postFirePlanting = postFirePlanting,
       pptYr0_2_OctNovDec = pptYr0_2_OctNovDec,
       pptYr1_3_JanSept = pptYr1_3_JanSept,
       tmaxYr1_3_JanSept = tmaxYr1_3_JanSept,
       tmaxYr0_2_OctNovDec = tmaxYr0_2_OctNovDec
       ) %>% 
  mutate_at(.vars = 'postFirePlanting', .funs = function(x){x > 0}) %>%
  rowwise() %>%
  mutate(pptYr0_3_sum = pptYr0_2_OctNovDec + pptYr1_3_JanSept,
    tmaxYr0_3 = (tmaxYr1_3_JanSept * 0.75 + tmaxYr0_2_OctNovDec * 0.25)) %>% #take the weighted average of tmax between fall and the rest of the year
  dplyr::select(-pptYr0_2_OctNovDec, -pptYr1_3_JanSept, -tmaxYr1_3_JanSept, -tmaxYr0_2_OctNovDec) #remove the intermediary climate variables




##################################################################
####join back with time varying DF and calculate recovery metrics#
##################################################################
timeVaryingDF2 <- timeVaryingDF %>%
  left_join(calculatedTimeInvariant, by = "pixelID") %>% 
  #select(pixelID,year,timeSinceFire,ConProb,postFireConProb,preFireConProb) %>%
  mutate(disturbanceSize = preFireConProb - postFireConProb,
         ARI = ConProb - postFireConProb,
         RRI = ARI / disturbanceSize) 

#######################################
#create relative recovery at year 20###
#######################################

RRIyr20 <- timeVaryingDF2 %>%
  filter(timeSinceFire == 20) %>% dplyr::select(pixelID,RRI) %>%
  rename(RRI_yr20 = RRI)

######################################################################
#adding the time-invariant variables from GEE to the time-varying DF##
######################################################################

#selecting the time-invariant variables from GEE
stableVars <- df %>% dplyr::select(pixelID,
                                   CWDhist,PPThist,PPThistSD,T_meanHist,T_meanSD,
                                   SolarLoad,aspect,eastness,northness,slope,elevation,
                                   burnSev,wilderness) 

#adding the time-invariant variables from GEE to the time-varying DF
#also adding RRI yr 20
#also filtering out pixels that did not experience a significant disturbance
timeVaryingDF3 <- timeVaryingDF2 %>% 
  left_join(stableVars, by = "pixelID") %>%
  left_join(RRIyr20, by = "pixelID") %>%
  mutate_at(.vars = c("postFirePlanting","focalAreaID"),.funs = as.numeric) %>%
  filter(disturbanceSize > 20) #filting pixels that did not experience a significant disturbance



############################################
#saving the Pixel-level Time Varying Data###
############################################
str(timeVaryingDF3)
timeStamp <- gsub(x = gsub(pattern = " ", replacement = "_", x = Sys.time()),pattern = ":",replacement = "-")

if(write_csvs == T){
write_csv(x = timeVaryingDF3, path = paste0(outpath,"fromR_pixelLevelTimeVarying_",timeStamp,".csv"))
}

############################################
#creating patch-level time varying data#####
############################################
patchLevelTimeVarying <- timeVaryingDF3 %>%
  group_by(patchID,year) %>%
  summarise_if(.predicate = is.numeric, .funs = mean)

############################################
#saving patch-level time varying data#####
############################################
str(patchLevelTimeVarying)
timeStamp <- gsub(x = gsub(pattern = " ", replacement = "_", x = Sys.time()),pattern = ":",replacement = "-")
if(write_csvs == T){
  write_csv(x = timeVaryingDF3, path = paste0(outpath,"fromR_patchLevelTimeVarying_",timeStamp,".csv"))
}



