source('00_setup.R')
#source('00_clean.R')

#This script calculates the variables used to predict conifer recovery
#Input: tibble of vars from gee
#output: analysis-ready tibble

#set parameters###
light_run <- T
write_csvs <- T
#path <- "~/cloud/gdrive/fire_project/local_data/fromGEE/checked/"
tmpFolder <- "~/cloud/gdrive/fire_project/local_data/fromGEE/tmp/"
figuresPath <- '~/cloud/gdrive/fire_project/figures/'
outpath <- "~/cloud/gdrive/fire_project/local_data/CleanedDataForAnalysis/"
sample_n <- 5

##functions###
lu <- function(x){length(unique(x))}


#function to get the time trajectories of time-varying variables
GetTimeTraj <- function(d, varOfInterest, pattern = "[:digit:]{4}(?=0)"){
  
  timeTraj <- d %>% 
    dplyr::select(pixelID, focalAreaID, fireYear, contains(varOfInterest)) %>% 
    gather(contains(varOfInterest),key = "year",value = varOfInterest) %>%
    mutate(year = flatten_chr(str_extract_all(year,pattern))) %>%
    arrange(pixelID,year) %>% 
    rename(!!sym(x = eval(varOfInterest)) := varOfInterest)
  
  return(timeTraj)
  
}

GetTimeTraj2 <- function(d = df, varOfInterest.x, pattern.x = "[:digit:]{4}", newName = 'test'){
  
  output <- GetTimeTraj(d = df,varOfInterest = varOfInterest.x,pattern = pattern.x) %>% 
    dplyr::select(pixelID,focalAreaID,year,fireYear,sym(eval(varOfInterest.x))) %>%
    mutate_at(.vars = "year", .funs = as.numeric) %>%
    rename(!!sym(eval(newName)) := sym(eval(varOfInterest.x)))
  
  return(output)
}

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

GetTimeVarStat2 <- function(df.x, var, relYrs.x = c(-1,-2), stat.x = "mean"){
  output <- mclapply(X = pixelIDs,
                     FUN = GetTimeVarStat,
                     mc.cores = numCores,
                     d = df.x, 
                     varOfInterest = var,
                     relYrs = relYrs.x,
                     stat = stat.x) %>% flatten_dbl()
  return(output)
}


###read in data###
df <- fa2 %>% rename(focalAreaID = focalArea)

head(df)

#decide how many pixels to calculate vars for
if(light_run == T){
  pixelsIN <- sample(x = df$pixelID,size = sample_n)
}

#filter dataframe by pixels that you want to calculate vars for
df <- df %>% filter(pixelID %in% pixelsIN)

#the pixels included in the analysis
pixelIDs <- unique(df$pixelID) 


#make time trajectories of time-varying variables
###alternative pattern in case need it: #"[:digit:]{4}(?=0)"
managementDF <- GetTimeTraj2(df,"management",newName = "management")
pptOctNovDecDF <- GetTimeTraj2(df,"10_ppt",newName = "pptOctNovDec")
pptJanSeptDF <- GetTimeTraj2(df,"01_ppt", newName = "pptJanSept")
tmaxOctNovDecDF <- GetTimeTraj2(df,"10_tmax",newName = "tmaxOctNovDec")
tmaxJanSeptDF <- GetTimeTraj2(df,"01_tmax",newName = "tmaxJanSept")


ConProb <- GetTimeTraj(d = df,varOfInterest = "ConProb") %>% 
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear,ConProb)
nYearsPerStudy <- max(as.numeric(ConProb$year)) - min(as.numeric(ConProb$year)) + 1
seedDF <- GetTimeTraj(d = df,varOfInterest = "SAP") %>% 
  dplyr::select(pixelID,patchID,focalAreaID,year,fireYear, SAP)


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



###################################################################
#calculating time-invariant variables from time-varying variables##
###################################################################
numCores <- detectCores()
preFireConProb <- GetTimeVarStat2(df.x = df, var = "ConProb", relYrs.x = c(-1,-2))
postFireConProb <- GetTimeVarStat2(df.x = df, var = "ConProb", relYrs.x = c(1,2))
postFireSAP <- GetTimeVarStat2(df.x = df, var = "SAP", relYrs.x = c(1,2,3))
postFirePlanting <- GetTimeVarStat2(df.x = managementDF, var = "management", relYrs.x = 1:6, stat = "max")
pptYr0_2_OctNovDec <- GetTimeVarStat2(df.x = df, var = "pptOctNovDec", relYrs.x = c(0,1,2), stat.x = "sum")
pptYr1_3_JanSept <- GetTimeVarStat2(df.x = df, var = "pptJanSept", relYrs.x = c(1,2,3), stat.x = "sum")
tmaxYr1_3_JanSept <- GetTimeVarStat2(df.x = df, var = "tmaxJanSept", relYrs.x = c(1,2,3))
tmaxYr0_2_OctNovDec <- GetTimeVarStat2(df.x = df, var = "tmaxOctNovDec", relYrs.x = c(0,1,2))

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
  rowwise() %>%
  mutate(pptYr0_3_sum = pptYr0_2_OctNovDec + pptYr1_3_JanSept, 
         tmaxYr0_3 = (tmaxYr1_3_JanSept * 0.75 + tmaxYr0_2_OctNovDec * 0.25)) %>% #take the weighted average of tmax between fall and the rest of the year
  dplyr::select(-pptYr0_2_OctNovDec, -pptYr1_3_JanSept, -tmaxYr1_3_JanSept, -tmaxYr0_2_OctNovDec) %>% #remove the intermediary climate variables
  ungroup()
 
##################################################################
####join back with time varying DF and calculate recovery metrics#
#####commenting this out for now to focus on the patch-level######
#####can calculate these metrics at the end at the patch-level####
##################################################################



#save the stable variables
stableVars <- df %>% dplyr::select(pixelID,focalAreaID,
                                   CWDhist,AEThist,PPThist,PPThistSD,TmaxHist,TmaxHistSD,
                                   hli,tpi,aspect,eastness,northness,adjustedNorthness,
                                   adj_fold_northness,slope,elevation,
                                   tpi, AEThist, 
                                   burnSev,wilderness,x,y)


df %>% select(conCov1984conCov.FA.2, conCov1984)


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



