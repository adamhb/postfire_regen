#This script calculates the variables used to predict conifer recovery
#Input: tibble of vars from gee
#output: analysis-ready tibble


#to do: 
#1) add reburn vars (did I do low and medium?) to stable vars
#2) add focal areas 1-3
#3) add precipitation anomaly terms (min and mean in 0-3yrs)
#4) add Temp-adjusted precip. anomaly terms
#5) may need to add the fire so that I can do spatial-cross validation like Rodman and Young studies
#6) a way of deviding this up into primarily moisture-limited vs. energy limited forests? Or are all mixed conifer forests primarily moisture-limited?
#7) need to create post-fire precip anomalies?




source('00_setup.R')
#source('00_clean.R')

#load data
df <- read_csv(paste0(data_path,'/allFAs.csv'))

#params of script
light_run <- F
sample_n <- 10
options(max.print = 1000)
options(dplyr.print_max = 1e4)

#set number of pixels to calculate variables for

if(light_run == T){
  pixelsIN <- sample(x = df$pixelID,size = sample_n)
  df <- df %>% filter(pixelID %in% pixelsIN)
}

#the pixels included in the analysis
pixelIDs <- unique(df$pixelID)

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

GetTimeTraj2 <- function(d = df, varOfInterest.x, pattern.x = "[:digit:]{4}", 
                         newName = 'test'){
  
  output <- GetTimeTraj(d = df,varOfInterest = varOfInterest.x,pattern = pattern.x) %>% 
    dplyr::select(pixelID,focalAreaID,year,fireYear,sym(eval(varOfInterest.x))) %>%
    mutate_at(.vars = "year", .funs = as.numeric) %>%
    rename(!!sym(eval(newName)) := sym(eval(varOfInterest.x)))
  
  return(output)
}

#function to calculate new time-invariant variables from the time-varying variables
GetTimeVarStat <- function(pixel, d, varOfInterest,relYrs,stat = "mean") {
  
  fireYearInt <- d[d$pixelID == pixel,]$fireYear %% 1 == 0
  if(fireYearInt == F){print("caution fire year not an integer")}
  
  d <- d %>%
    mutate_at(.vars = "year",.funs = as.numeric) %>%
    mutate_at(.vars = "fireYear",.funs = round) %>%
    mutate(timeSinceFire = year-fireYear)

  if(stat == "mean"){f = function(x){mean(x,na.rm = T)}}
  if(stat == "sum"){f = function(x){sum(x,na.rm = T)}}
  if(stat == "max"){f = function(x){max(x,na.rm = T)}}
   
  
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


#rename the SAP vars
SAP.name.indices <- grep(pattern = "SAP", x = names(df))
SAP.names.old <- grep(pattern = "SAP", x = names(df),value = T)
SAP.names.new <- paste0("SAP",str_extract(string = SAP.names.old, pattern = "[:digit:]{4}"))
names(df)[SAP.name.indices] <- SAP.names.new





#save the stable variables
stableVars <- df %>% dplyr::select(pixelID,
                                   CWDhist,AEThist,PPThist,PPThistSD,TmaxHist,TmaxHistSD,
                                   hli,tpi,aspect,eastness,northness,adjustedNorthness,
                                   adj_fold_northness,slope,elevation,
                                   fireYear, burnSev, mediumReburns, lowReburns,
                                   wilderness,x,y)


#wide to long conversion of time-varying variables
###alternative pattern in case need it: #"[:digit:]{4}(?=0)"
conCovDF <- GetTimeTraj2(df,"conCov",newName = "conCov")
sapDF <- GetTimeTraj2(df,"SAP",newName = "SAP")
managementDF <- GetTimeTraj2(df,"management",newName = "management")
pptOctNovDecDF <- GetTimeTraj2(df,"10_ppt",newName = "pptOctNovDec")
pptJanSeptDF <- GetTimeTraj2(df,"01_ppt", newName = "pptJanSept")
tmaxOctNovDecDF <- GetTimeTraj2(df,"10_tmax",newName = "tmaxOctNovDec")
tmaxJanSeptDF <- GetTimeTraj2(df,"01_tmax",newName = "tmaxJanSept")

###################################################################
#calculating time-invariant variables from time-varying variables##
###################################################################
numCores <- detectCores()
preFireConCov <- GetTimeVarStat2(df.x = conCovDF, var = "conCov", relYrs.x = c(-1,-2))
postFireConCov <- GetTimeVarStat2(df.x = conCovDF, var = "conCov", relYrs.x = c(1,2))
postFireSAP <- GetTimeVarStat2(df.x = sapDF, var = "SAP", relYrs.x = c(1,2,3))
postFirePlanting <- GetTimeVarStat2(df.x = managementDF, var = "management", relYrs.x = 1:6, stat = "max")
pptYr0_2_OctNovDec <- GetTimeVarStat2(df.x = pptOctNovDecDF, var = "pptOctNovDec", relYrs.x = c(0,1,2), stat.x = "sum")
pptYr1_3_JanSept <- GetTimeVarStat2(df.x = pptJanSeptDF, var = "pptJanSept", relYrs.x = c(1,2,3), stat.x = "sum")
tmaxYr0_2_OctNovDec <- GetTimeVarStat2(df.x = tmaxOctNovDecDF, var = "tmaxOctNovDec", relYrs.x = c(0,1,2))
tmaxYr1_3_JanSept <- GetTimeVarStat2(df.x = tmaxJanSeptDF, var = "tmaxJanSept", relYrs.x = c(1,2,3))

#nYearsPerStudy <- max(as.numeric(conCovDF$year)) - min(as.numeric(conCovDF$year)) + 1

#############################################################
###creating a DF of the calculated time-invariant variables##
#############################################################
calculatedTimeInvariant <- tibble(pixelID = pixelIDs,
       preFireConCov = preFireConCov,
       postFireConCov = postFireConCov,
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
  dplyr::select(-pptYr0_2_OctNovDec, -pptYr1_3_JanSept, -tmaxYr1_3_JanSept, -tmaxYr0_2_OctNovDec)
 
##############################
####join all vars together####
##############################

df2 <- conCovDF %>%
  mutate_at(.vars = "year",.funs = as.numeric) %>%
  mutate(timeSinceFire = year-fireYear) %>%
  left_join(calculatedTimeInvariant,by = "pixelID") %>%
  left_join(stableVars, by = 'pixelID') %>%
  mutate(disturbanceSize = preFireConCov - postFireConCov,
         ARI = conCov - postFireConCov,
         RRI = ARI / disturbanceSize)


#check and write cleaned dataset
#str(df2)

NAs_per_var <- summarise_all(df2, .funs = is.na) %>%
  summarise_all(.tbl = ., .funs = sum) %>% as.numeric()
print(paste('NAs:',NAs_per_var))

model_run_time_stamp <- Sys.time() %>% 
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = ":", replacement = "-") %>%
  sub(pattern = " ", replacement = "-")

write_csv(df2,paste0(data_path,'analysisReadyDF.csv'))







