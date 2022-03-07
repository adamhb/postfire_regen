rm(list = ls())
gc()
source('00_setup.R')

#set parameters

minPixPerPatch <- 1
nViablePixPerYear <- 1
light_run <- F
write_csvs <- T
patches_sub_sample <- 50
path <- "~/cloud/gdrive/fire_project/local_data/fromGEE/checked/"
path <- "~/cloud/gdrive/fire_project/local_data/fromGEE/"
tmpFolder <- "~/cloud/gdrive/fire_project/local_data/fromGEE/tmp/"
outpath <- "~/cloud/gdrive/fire_project/local_data/CleanedDataForAnalysis/"
path_to_clean_data <- "~/cloud/gdrive/fire_project/local_data/CleanedDataForAnalysis/"

files <- list.files(path,pattern = "csv")

write_csv_to_temp <- function(obj,file_name,perPatch = F){
  write_csv(obj,file = paste0(tmpFolder,file_name,".csv"))
}

writePatchLevel <- function(df,var){
  my_sym <- sym(var)
  tmp <- df %>%
    mutate_at(.vars = "patchID", .funs = as.character) %>%
    select(-pixelID) %>%
    group_by(patchID,focalAreaID,year) %>%
    summarise(mean = mean(!!my_sym, na.rm = T),
              fireYear = mean(fireYear)) %>%
    rename(!!my_sym := mean) %>%
    ungroup()
  
  write_csv_to_temp(tmp, file_name = paste0(var,"PerPatch"))
}


readPatchLevel <- function(var){
  read_csv(paste0(tmpFolder,var,"PerPatch.csv"),show_col_types = FALSE)
}

readPixelLevel <- function(var){
  read_csv(paste0(tmpFolder,var,".csv"),show_col_types = FALSE)
}


#outPath <- "/home/rstudio/figures/"

#join data from different focal areas (csvs)
df <- tibble()

j <- 0
for(f in files){
              j <- j + 1
              df_tmp <- read_csv(paste0(path,f),show_col_types = FALSE) %>%
                drop_na(PatchID) %>% mutate(newPatchID = paste0(PatchID,"-FA-",focalAreaID)) %>%
                rename(patchID = newPatchID) %>% select(-PatchID)
              focalAreaID <- df_tmp$focalAreaID[1]
              pixelID <- paste(focalAreaID,df_tmp$pixelID, sep = "_")
              df_tmp$pixelID <- pixelID
              df <- rbind(df,df_tmp)
              print(paste("done with focal area",focalAreaID,":",j,"of",length(files),"focal areas"))
}


#record focal areas
focal_areas <- unique(df$focalAreaID)

#cleaning
df <- df %>% rename(fireYear = FireYear) %>%
  mutate_at(.vars = c("patchID","focalAreaID"),.funs = as.character) ##make IDs characters
  
pixelsPerPatch <- df %>% #pixels per patch
  group_by(patchID) %>%
  summarise(nPerPatch = length(pixelID))

print(paste("unique patches before filtering:",length(unique(pixelsPerPatch$patchID))))

#filter to patches with more than minPixPerPatch number of pixels
PatchesIN <- pixelsPerPatch %>% filter(nPerPatch >= minPixPerPatch) %>% 
  pull(patchID) #could sample patches randomly here

print(paste("nPatches after patch size filter =",length(PatchesIN)))

if(light_run == T){
  PatchesIN <- sample(x = PatchesIN,size = patches_sub_sample)
}

print(paste("nPatches after light run filter =",length(PatchesIN)))

df <- df %>% filter(patchID %in% PatchesIN)

fa_pa_px <- df %>% select(focalAreaID,patchID,pixelID)

lu <- function(x){length(unique(x))}

pixelIDs <- unique(df$pixelID) 

# patchSizeDistribution <- fa_pa_px %>%
#   group_by(patchID) %>%
#   summarise(patchSize = length(pixelID)*900/1e4) %>% 
#   ggplot(aes(patchSize)) +
#   geom_histogram(binwidth = 5, color="black", fill="white") +
#   scale_x_continuous("Patch size (ha)", breaks = seq(0,70,by = 5)) +
#   adams_theme


#makePNG(fig = patchSizeDistribution, path_to_output.x = "figures/",file_name = "PatchSizedist")
#save the time-invariant variables (per pixel and per patch versions)
stableVars <- df %>% dplyr::select(pixelID, patchID,
                                   CWDhist,PPThist,PPThistSD,T_meanHist,T_meanSD,
                                   SolarLoad,aspect,eastness,northness,slope,elevation,
                                   tpi, AEThist,
                                   burnSev,wilderness)

write_csv(x = stableVars, file = paste0(tmpFolder,"stableVars.csv"))

# stableVarsPerPatch <- stableVars %>%
#   select(-pixelID) %>%
#   group_by(patchID) %>%
#   summarise_if(.predicate = is.numeric, .funs = mean)
# write_csv(x = stableVarsPerPatch , file = paste0(tmpFolder,"stableVarsPerPatch.csv"))

rm(stableVars)
gc()
########################################

#function to get the time trajectories of time-varying variables
GetTimeTraj <- function(d, varOfInterest, pattern = "[:digit:]{4}(?=0)", write_to_csv = F){
  
  timeTraj <- d %>% 
    dplyr::select(pixelID, patchID, focalAreaID, fireYear, contains(varOfInterest)) %>% 
    gather(contains(varOfInterest),key = "year",value = varOfInterest) %>%
    mutate(year = flatten_chr(str_extract_all(year,pattern))) %>%
    arrange(pixelID,year) %>% 
    rename(!!sym(x = eval(varOfInterest)) := varOfInterest)
  
  return(timeTraj)
  
}

#filter to make sure all patches have at least 30 pixels with a 
#ConProb reading in every year

#make time trajectories of time-varying variables
ConProb <- GetTimeTraj(d = df,varOfInterest = "ConProb") %>% 
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear,ConProb)

nYearsPerStudy <- max(as.numeric(ConProb$year)) - min(as.numeric(ConProb$year)) + 1

# viablePatches <- ConProb %>% 
#   group_by(patchID,year) %>%
#   summarise(ConProb_n = length(ConProb),
#             NA_n = sum(is.na(ConProb))) %>%
#   mutate(ViablePixelsPerPatch = ConProb_n - NA_n) %>%
#   filter(ViablePixelsPerPatch > nViablePixPerYear) %>%
#   group_by(patchID) %>%
#   summarise(NViableYears = length(year)) %>%
#   filter(NViableYears == nYearsPerStudy) %>%
#   pull(patchID) %>% unique()

#PatchesIN <- PatchesIN[PatchesIN %in% viablePatches] 

#update the df
#df <- df %>% filter(patchID %in% PatchesIN)
pixelIDs <- unique(df$pixelID)
patchIDs <- unique(df$patchID)

#make time trajectories of time-varying variables
ConProb <- GetTimeTraj(d = df,varOfInterest = "ConProb") %>% 
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear,ConProb)

write_csv_to_temp(ConProb ,"ConProb")
#writePatchLevel(df = ConProb , "ConProb")

seedDF <- GetTimeTraj(d = df,varOfInterest = "SAP") %>% 
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear, SAP)
write_csv_to_temp(seedDF,"seedDF")
#writePatchLevel(df = seedDF, "SAP")
rm(seedDF)


managementDF <- GetTimeTraj(d = df,varOfInterest = "management",pattern = "[:digit:]{4}") %>% 
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear,management) %>%
  mutate_at(.vars = 'management', .funs = function(x){x > 0}) %>%
  mutate_at(.vars = "management", .funs = as.numeric)
write_csv_to_temp(managementDF,"managementDF")
#writePatchLevel(df = managementDF, "management")
rm(managementDF)

pptOctNovDecDF <- GetTimeTraj(d = df,varOfInterest = "10_ppt",pattern = "[:digit:]{4}")
names(pptOctNovDecDF)[6] <- "pptOctNovDec"
pptOctNovDecDF <- pptOctNovDecDF %>%
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear,pptOctNovDec)
write_csv_to_temp(pptOctNovDecDF,"pptOctNovDecDF")
#writePatchLevel(df = pptOctNovDecDF, "pptOctNovDec")
rm(pptOctNovDecDF)


pptJanSeptDF <- GetTimeTraj(d = df,varOfInterest = "01_ppt",pattern = "[:digit:]{4}")
names(pptJanSeptDF)[6] <- "pptJanSept"
pptJanSeptDF <- pptJanSeptDF %>%
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear,pptJanSept)
write_csv_to_temp(pptJanSeptDF,"pptJanSeptDF")
#writePatchLevel(df = pptJanSeptDF, "pptJanSept")
rm(pptJanSeptDF)


tmaxJanSeptDF <- GetTimeTraj(d = df,varOfInterest = "01_tmax",pattern = "[:digit:]{4}")
names(tmaxJanSeptDF)[6] <- "tmaxJanSept"
tmaxJanSeptDF <- tmaxJanSeptDF %>%
  dplyr::select(pixelID,year,patchID,focalAreaID,fireYear,tmaxJanSept)
write_csv_to_temp(tmaxJanSeptDF,"tmaxJanSeptDF")
#writePatchLevel(df = tmaxJanSeptDF, "tmaxJanSept")
rm(tmaxJanSeptDF)


tmaxOctNovDecDF <- GetTimeTraj(d = df,varOfInterest = "10_tmax",pattern = "[:digit:]{4}")
names(tmaxOctNovDecDF)[6] <- "tmaxOctNovDec"
tmaxOctNovDecDF <- tmaxOctNovDecDF %>%
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear,tmaxOctNovDec)
write_csv_to_temp(tmaxOctNovDecDF,"tmaxOctNovDecDF")
#writePatchLevel(df = tmaxOctNovDecDF, "tmaxOctNovDec")
rm(tmaxOctNovDecDF)



#adding all time varying variables into the same DF
# timeVaryingDF <- conProbDF %>%
#   left_join(seedDF,by = c("pixelID","year")) %>%
#   left_join(managementDF, by = c("pixelID","year")) %>%
#   left_join(pptOctNovDecDF,by = c("pixelID","year")) %>%
#   left_join(pptJanSeptDF, by = c("pixelID","year")) %>%
#   left_join(tmaxJanSeptDF,by = c("pixelID","year")) %>%
#   left_join(tmaxOctNovDecDF, by = c("pixelID","year")) %>%
#   mutate_at(.vars = "year",.funs = as.numeric) %>%
#   mutate(timeSinceFire = year-fireYear)

#function to calculate new time-invariant variables from the time-varying variables
GetTimeVarStat <- function(pixel, d, varOfInterest,relYrs,stat = "mean") {
   d <- d %>%
    mutate_at(.vars = "year",.funs = as.numeric) %>%
    mutate(timeSinceFire = year-fireYear)

  if(stat == "mean"){f = function(x){mean(x,na.rm = T)}}
  if(stat == "sum"){f = function(x){sum(x,na.rm = T)}}
  if(stat == "max"){f = function(x){max(x,na.rm = T)}}
  
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

# ConProb %>%
#   filter(pixelID %in% pixelIDs[11]) %>%
#   ggplot(aes(year,ConProb,color = pixelID)) +
#   geom_smooth() +
#   scale_x_continuous(minor_breaks = 1990:2020) +
#   geom_point() +
#   adams_theme

#GetTimeVarStat(pixel = pixelIDs[1],d = ConProb, varOfInterest = "ConProb", relYrs = c(-1,-2))


GetTimeVarStat2 <- function(var = "ConProb", fileName = "ConProb", relYrs.x = c(-1,-2), stat.x = "mean"){
  pixelLevelDF <- readPixelLevel(fileName)
  output <- mclapply(X = pixelIDs,
                             FUN = GetTimeVarStat,
                             mc.cores = numCores,
                             d = pixelLevelDF, 
                             varOfInterest = var,
                             relYrs = relYrs.x,
                             stat = stat.x) %>% flatten_dbl()
  rm(pixelLevelDF)
  return(output)
}

start_time <- Sys.time()
preFireConProb <- GetTimeVarStat2(var = "ConProb", fileName = "ConProb",relYrs.x = c(-1,-2))
end_time <- Sys.time()
print(paste(as.numeric(end_time - start_time)/length(pixelIDs) * 1000, "seconds per 1000 pixels"))


postFireConProb <- GetTimeVarStat2(var = "ConProb", fileName = "ConProb", relYrs.x = c(1,2))
postFireSAP <- GetTimeVarStat2(var = "SAP", fileName = "seedDF",relYrs.x = c(1,2,3))
postFirePlanting <- GetTimeVarStat2(var = "management", fileName = "managementDF",relYrs.x = 1:6)
pptYr0_2_OctNovDec <- GetTimeVarStat2(var = "pptOctNovDec", fileName = "pptOctNovDecDF",relYrs.x = c(0,1,2), stat.x = "sum")
pptYr1_3_JanSept <- GetTimeVarStat2(var = "pptJanSept", fileName = "pptJanSeptDF",relYrs.x = c(1,2,3), stat.x = "sum")
tmaxYr1_3_JanSept <- GetTimeVarStat2(var = "tmaxJanSept", fileName = "tmaxJanSeptDF",relYrs.x = c(1,2,3), stat.x = "mean")
tmaxYr0_2_OctNovDec <- GetTimeVarStat2(var = "tmaxOctNovDec", fileName = "tmaxOctNovDecDF",relYrs.x = c(0,1,2), stat.x = "mean")


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
  #mutate_at(.vars = 'postFirePlanting', .funs = function(x){x > 0}) %>%
  #mutate_at(.vars = "postFirePlanting", .funs = as.numeric) %>%
  rowwise() %>%
  mutate(pptYr0_3_sum = pptYr0_2_OctNovDec + pptYr1_3_JanSept, 
         tmaxYr0_3 = (tmaxYr1_3_JanSept * 0.75 + tmaxYr0_2_OctNovDec * 0.25)) %>% #take the weighted average of tmax between fall and the rest of the year
  dplyr::select(-pptYr0_2_OctNovDec, -pptYr1_3_JanSept, -tmaxYr1_3_JanSept, -tmaxYr0_2_OctNovDec) %>% #remove the intermediary climate variables
  ungroup()
  #group_by(patchID) %>%
  #summarise_if(.predicate = is.numeric, .funs = mean)

#write_csv(calculatedTimeInvariant, file = paste0(tmpFolder,"calculatedTimeInvariantPerPatch.csv"))



##################################################################
####join back with time varying DF and calculate recovery metrics#
#####commenting this out for now to focus on the patch-level######
#####can calculate these metrics at the end at the patch-level####
##################################################################
# timeVaryingDF2 <- timeVaryingDF %>%
#   left_join(calculatedTimeInvariant, by = "pixelID") %>% 
#   #select(pixelID,year,timeSinceFire,ConProb,postFireConProb,preFireConProb) %>%
#   mutate(disturbanceSize = preFireConProb - postFireConProb,
#          ARI = ConProb - postFireConProb,
#          RRI = ARI / disturbanceSize) 

#######################################
#create relative recovery at year 20###
#######################################
# RRIyr20 <- timeVaryingDF2 %>%
#   filter(timeSinceFire == 20) %>% dplyr::select(pixelID,RRI) %>%
#   rename(RRI_yr20 = RRI)

######################################################################
#adding the time-invariant variables from GEE to the time-varying DF##
######################################################################

#selecting the time-invariant variables from GEE
# stableVars <- df %>% dplyr::select(pixelID,
#                                    CWDhist,PPThist,PPThistSD,T_meanHist,T_meanSD,
#                                    SolarLoad,aspect,eastness,northness,slope,elevation,
#                                    burnSev,wilderness) 

#adding the time-invariant variables from GEE to the time-varying DF
#also adding RRI yr 20
#also filtering out pixels that did not experience a significant disturbance
# timeVaryingDF3 <- timeVaryingDF2 %>% 
#   left_join(stableVars, by = "pixelID") %>%
#   left_join(RRIyr20, by = "pixelID") %>%
#   mutate_at(.vars = c("postFirePlanting","focalAreaID"),.funs = as.numeric) %>%
#   filter(disturbanceSize > 20) #filting pixels that did not experience a significant disturbance



############################################
#saving the Pixel-level Time Varying Data###
############################################
# str(timeVaryingDF3)
# timeStamp <- gsub(x = gsub(pattern = " ", replacement = "_", x = Sys.time()),pattern = ":",replacement = "-")
# 
# if(write_csvs == T){
# write_csv(x = timeVaryingDF3, path = paste0(outpath,"fromR_pixelLevelTimeVarying_FA13",timeStamp,".csv"))
# }

############################################
#creating patch-level time varying data#####
############################################
# patchLevelTimeVarying <- timeVaryingDF3 %>%
#   group_by(patchID,year) %>%
#   summarise_if(.predicate = is.numeric, .funs = mean)



######################################################
#joining and saving patch-level time varying data#####
######################################################

# readFromTemp <- function(x){
#   read_csv(paste0(tmpFolder,x,"PerPatch.csv"),show_col_types = F) %>%
#     mutate_at(.vars = c("patchID","focalAreaID"), .funs = as.character)
# }


#aggregate and export
stableVarsPerPixel <- readPixelLevel("stableVars")
ConProb <- readPixelLevel("ConProb")

allVarsPixelLevel <- ConProb %>% select(-patchID) %>%
  left_join(stableVarsPerPixel, by = "pixelID") %>%
  left_join(calculatedTimeInvariant, by = "pixelID") %>%
  mutate_at(.vars = "fireYear",.funs = round) %>%
  mutate(timeSinceFire = year-fireYear) 
  
#add recovery trajectory length here?

write_csv_to_temp(allVarsPixelLevel,file_name = "allVarsPixelLevel_3_6_2022")
#write_csv(allVarsPatchLevel, file = paste0(path_to_clean_data,"allVarsPatchLevel.csv"))

NAs_per_var <- summarise_all(allVarsPixelLevel, .funs = is.na) %>%
  summarise_all(.tbl = ., .funs = sum) %>% as.numeric()

final_domain <- fa_pa_px %>% filter(patchID %in% PatchesIN)

print(summary_stats(data = fa_pa_px %>% filter(patchID %in% PatchesIN)))
print(paste("FocalAreas:",unique(final_domain$focalAreaID)))
print(paste("NAs per var:",NAs_per_var))



