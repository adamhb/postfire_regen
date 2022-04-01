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

#Sys.time()

end_recovery_period <- 2019
rollingMeanLength  <- 3 #years

source('00_setup.R')
#source('00_clean.R')

#load data
df <- read_csv(paste0(data_path,'FA_3_30_2022.csv')) 

#params of script
light_run <- F
sample_n <- 20
options(max.print = 1000)
options(dplyr.print_max = 1e4)

#set number of pixels to calculate variables for

if(light_run == T){
  pixelsIN <- sample(x = df$pixelID,size = sample_n)
  df <- df %>% filter(pixelID %in% pixelsIN)
}

#the pixels included in the analysis
pixelIDs <- unique(df$pixelID)


#function to get exponential moving average from vector
updateEMA <- function(currentAverage, newValue, window, alpha = 1, i){
  
  if ((i-window) < 0) {
    newValue <- currentAverage
  } else {
    wgtNew <- alpha / window
    wgtCurrent <- 1 - wgtNew
    newValue <- (currentAverage * wgtCurrent) + (newValue * wgtNew)
  }
  
  return(newValue)
}


getEMA <- function(df, var, ema_window, alpha = 1){
  
  ema <- c()
  ema[1] <- as.numeric(df[1,var])
  for(i in 2:nrow(df)){
    ema[i] <- updateEMA(currentAverage = ema[i-1],
                        newValue = as.numeric(df[i,var]),
                        window = ema_window,
                        alpha = alpha,
                        i = i)
  }
  
  return(ema)
  
}




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
  
  fireYearInt <- d[d$pixelID == pixel,]$fireYear[1] %% 1 == 0
  if(fireYearInt == F){print("caution fire year not an integer")}
  
  d <- d %>%
    mutate_at(.vars = "year",.funs = as.numeric) %>%
    mutate_at(.vars = "fireYear",.funs = round) %>%
    mutate(timeSinceFire = year-fireYear)

  if(stat == "mean"){f = function(x){mean(x,na.rm = T)}}
  if(stat == "sum"){f = function(x){sum(x,na.rm = T)}}
  if(stat == "max"){f = function(x){max(x,na.rm = T)}}
  if(stat == "min"){f = function(x){min(x,na.rm = T)}}
   
  
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

#check nas
checkNAs <- function(d){
  NAs_per_var_d <- summarise_all(d, .funs = is.na) %>%
    summarise_all(.tbl = ., .funs = sum) %>% as.numeric()
  
  print(tibble(var = names(d), NAs = NAs_per_var_d))
}



rollFunc <- function(df, var, pixelIDs = pixelIDs, width = rollingMeanLength){
  output <- tibble()
  for(p in pixelIDs){
    years <- df %>% filter(pixelID == p) %>% pull(year)
    rawVector <- df %>% filter(pixelID == p) %>% pull(var)
    centerRoll <- rollapply(data = rawVector, 
                            width = width, 
                            FUN = function(x){mean(x,na.rm = T)}, 
                            fill = c(NA,NA,NA))
    leftPad <- mean(rawVector[1:width])
    rightPad <- mean(rawVector[(length(rawVector)-width):length(rawVector)])
    centerRoll[1] <- leftPad
    centerRoll[length(centerRoll)] <- rightPad
    tmp <- tibble(pixelID = p, year = years, conCovRoll = centerRoll)
    output <- rbind(output, tmp)
  }
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
                                   burnSev, mediumReburns, lowReburns,
                                   forestType, wilderness,x,y) %>%
                     mutate_at(.vars = c("mediumReburns","lowReburns"), .funs = function(x){ifelse(is.na(x), 0, x)})

checkNAs(d = stableVars)
#wide to long conversion of time-varying variables
###alternative pattern in case need it: #"[:digit:]{4}(?=0)"
conCovDF <- GetTimeTraj2(df,"conCov",newName = "conCov")

#converting con cov to a rolling mean
adamRoll <- rollFunc(df = conCovDF, var = 'conCov', pixelIDs = pixelIDs)
conCovDF2 <- conCovDF %>% left_join(adamRoll, by = c("pixelID","year"))
conCovDF$conCov <- conCovDF2$conCovRoll

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
preFireConCov <- GetTimeVarStat2(df.x = conCovDF, var = "conCov", relYrs.x = c(-1,-2,-3), stat.x = 'mean')
postFireConCov <- GetTimeVarStat2(df.x = conCovDF, var = "conCov", relYrs.x = c(1,2,3), stat.x = "min")
#GetTimeVarStat(pixel = pixelIDs[1], d = conCovDF, varOfInterest = "conCov", relYrs = c(1,2,3), stat = "min")
postFireSAP <- GetTimeVarStat2(df.x = sapDF, var = "SAP", relYrs.x = c(1,2,3), stat.x = "mean")
postFirePlanting <- GetTimeVarStat2(df.x = managementDF, var = "management", relYrs.x = 1:6, stat.x = "max")

pptYr0_2_OctNovDec <- GetTimeVarStat2(df.x = pptOctNovDecDF, var = "pptOctNovDec", relYrs.x = c(0,1,2), stat.x = "sum")
pptYr1_3_JanSept <- GetTimeVarStat2(df.x = pptJanSeptDF, var = "pptJanSept", relYrs.x = c(1,2,3), stat.x = "sum")
pptYr1_3_annual_mean <- (pptYr0_2_OctNovDec + pptYr1_3_JanSept) / 3 #use this

tmaxYr0_2_OctNovDec <- GetTimeVarStat2(df.x = tmaxOctNovDecDF, var = "tmaxOctNovDec", relYrs.x = c(0,1,2))
tmaxYr1_3_JanSept <- GetTimeVarStat2(df.x = tmaxJanSeptDF, var = "tmaxJanSept", relYrs.x = c(1,2,3))
tmaxYr1_3_mean = (tmaxYr1_3_JanSept * 0.75) + (tmaxYr0_2_OctNovDec * 0.25) #use this

#To get the min precip. anomaly in the first 3 water years after fire
#calendar year labels
pptYr0_OctNovDec <- GetTimeVarStat2(df.x = pptOctNovDecDF, var = "pptOctNovDec", relYrs.x = c(0), stat.x = "sum")
pptYr1_OctNovDec <- GetTimeVarStat2(df.x = pptOctNovDecDF, var = "pptOctNovDec", relYrs.x = c(1), stat.x = "sum")
pptYr2_OctNovDec <- GetTimeVarStat2(df.x = pptOctNovDecDF, var = "pptOctNovDec", relYrs.x = c(2), stat.x = "sum")
pptYr1_JanSept <- GetTimeVarStat2(df.x = pptJanSeptDF, var = "pptJanSept", relYrs.x = c(1), stat.x = "sum")
pptYr2_JanSept <- GetTimeVarStat2(df.x = pptJanSeptDF, var = "pptJanSept", relYrs.x = c(2), stat.x = "sum")
pptYr3_JanSept <- GetTimeVarStat2(df.x = pptJanSeptDF, var = "pptJanSept", relYrs.x = c(3), stat.x = "sum")

#water year labels
pptYr1 <- pptYr0_OctNovDec + pptYr1_JanSept
pptYr2 <- pptYr1_OctNovDec + pptYr2_JanSept
pptYr3 <- pptYr2_OctNovDec + pptYr3_JanSept

tmaxYr0_OctNovDec <- GetTimeVarStat2(df.x = tmaxOctNovDecDF, var = "tmaxOctNovDec", relYrs.x = c(0))
tmaxYr1_OctNovDec <- GetTimeVarStat2(df.x = tmaxOctNovDecDF, var = "tmaxOctNovDec", relYrs.x = c(1))
tmaxYr2_OctNovDec <- GetTimeVarStat2(df.x = tmaxOctNovDecDF, var = "tmaxOctNovDec", relYrs.x = c(2))
tmaxYr1_JanSept <- GetTimeVarStat2(df.x = tmaxJanSeptDF, var = "tmaxJanSept", relYrs.x = c(1))
tmaxYr2_JanSept <- GetTimeVarStat2(df.x = tmaxJanSeptDF, var = "tmaxJanSept", relYrs.x = c(2))
tmaxYr3_JanSept <- GetTimeVarStat2(df.x = tmaxJanSeptDF, var = "tmaxJanSept", relYrs.x = c(3))

tmaxYr1 <- (tmaxYr0_OctNovDec * 0.25) + (tmaxYr1_JanSept * 0.75)
tmaxYr2 <- (tmaxYr1_OctNovDec * 0.25) + (tmaxYr2_JanSept * 0.75)
tmaxYr3 <- (tmaxYr2_OctNovDec * 0.25) + (tmaxYr3_JanSept * 0.75)

t_adjusted_precip_Yr1 <- pptYr1 / tmaxYr1
t_adjusted_precip_Yr2 <- pptYr2 / tmaxYr2
t_adjusted_precip_Yr3 <- pptYr3 / tmaxYr3

min_annual_ppt_Yr1_3 <- tibble(pptYr1 = pptYr1, pptYr2 = pptYr2, pptYr3 = pptYr3) %>% #use this
  rowwise() %>% mutate(minPpt = min(pptYr1,pptYr2,pptYr3)) %>% pull(minPpt)

min_annual_ppt_t_adjusted_Yr1_3 <- tibble(t_adjusted_precip_Yr1 = t_adjusted_precip_Yr1, #use this
                                t_adjusted_precip_Yr2 = t_adjusted_precip_Yr2, 
                                t_adjusted_precip_Yr3 = t_adjusted_precip_Yr3) %>% rowwise() %>% 
                         mutate(min_t_adjusted_precip = min(t_adjusted_precip_Yr1,
                                                            t_adjusted_precip_Yr2,
                                                            t_adjusted_precip_Yr3)) %>% 
                         pull(min_t_adjusted_precip)


#############################################################
###creating a DF of the calculated time-invariant variables##
#############################################################
calculatedTimeInvariant <- tibble(pixelID = pixelIDs,
       preFireConCov = preFireConCov,
       postFireConCov = postFireConCov,
       postFireSAP = postFireSAP,
       postFirePlanting = postFirePlanting,
       pptYr1_3_annual_mean = pptYr1_3_annual_mean,
       tmaxYr1_3_mean = tmaxYr1_3_mean,
       min_annual_ppt_Yr1_3 = min_annual_ppt_Yr1_3,
       min_annual_ppt_t_adjusted_Yr1_3 = min_annual_ppt_t_adjusted_Yr1_3) %>%
  mutate(pptYr1_3_annual_mean_t_adjusted = pptYr1_3_annual_mean / tmaxYr1_3_mean)
  
 

##############################
####join all vars together####
##############################
timeStable <- stableVars %>%
  left_join(calculatedTimeInvariant, by = "pixelID") 
#check NAs in all time-stable vars
NAs_per_var_timeStable <- summarise_all(timeStable, .funs = is.na) %>%
  summarise_all(.tbl = ., .funs = sum) %>% as.numeric()
#print(tibble(var = names(timeStable), NAs = NAs_per_var_timeStable))


df2 <- conCovDF %>%
  mutate_at(.vars = "year",.funs = as.numeric) %>%
  mutate(timeSinceFire = year-fireYear) %>%
  left_join(timeStable,by = "pixelID") %>%
  mutate(disturbanceSize = preFireConCov - postFireConCov,
         ARI = conCov - postFireConCov,
         RRI = ARI / disturbanceSize,
         recoveryTime = end_recovery_period - fireYear,
         ppt_Yr1_3_mean_annual_anom = (pptYr1_3_annual_mean - PPThist) / PPThistSD,
         min_annual_ppt_Yr1_3_anom = (min_annual_ppt_Yr1_3 - PPThist) / PPThistSD)


#check nas in the time varying data frame
NAs_per_var_df2 <- summarise_all(df2, .funs = is.na) %>%
  summarise_all(.tbl = ., .funs = sum) %>% as.numeric()
print(tibble(var = names(df2), NAs = NAs_per_var_df2))



#export for analysis
export <- df2 %>% drop_na() 
print('focal areas:')
print(unique(export$focalAreaID))

print('pixels:')
print(unique(df2$pixelID))

write_csv(export,paste0(data_path,'analysisReadyDF_3_30_2022.csv'))



















#scratch
#df2 %>% select(pixelID, year, fireYear, conCov, RRI, preFireConCov, AEThist) %>% filter(is.na(conCov))

# model_run_time_stamp <- Sys.time() %>% 
#   sub(pattern = ":", replacement = "-") %>%
#   sub(pattern = ":", replacement = "-") %>%
#   sub(pattern = " ", replacement = "-")


# conCovDF2 %>%
#   filter(pixelID == pixelIDs[5]) %>%
#   ggplot(aes(year,conCov)) +
#   geom_line() +
#   geom_line(aes(year,conCovRoll), color = "red") +
#   theme_minimal()

# #converting con cov to an exponential moving average
# conCovEMA <- getEMA(df = conCovDF %>% filter(pixelID == pixelIDs[1]), var = 'conCov', ema_window = rollingMeanLength)
# 
# conCovRollingMean <- rollapply(data = conCovDF %>% filter(pixelID == pixelIDs[1]) %>% pull(conCov), width = rollingMeanLength, FUN = function(x){mean(x,na.rm = T)}, fill = c(NA,NA,NA))
# 
# conCovDF %>% filter(pixelID == pixelIDs[1]) %>%
#   add_column(ema = conCovEMA, rollMean = conCovRollingMean, adamRoll = adamRoll$conCov) %>%
#   ggplot(aes(year,conCov)) +
#   geom_line() +
#   geom_line(aes(year,conCovEMA), color ="blue") +
#   geom_line(aes(year,rollMean), color ="red") +
#   geom_line(aes(year,adamRoll), color ="green", linetype = "dashed") +
#   #geom_line(data = conCovDF2, aes(year,conCovRoll2), color = "red") +
#   theme_minimal()
# 
# 
# conCovRoll <- rollapply(data = conCovDF$conCov, width = rollingMeanLength, FUN = function(x){mean(x,na.rm = T)}, fill = c(NA,NA,NA))
# conCovDF2 <- conCovDF %>%
#   mutate(conCovRoll = conCovRoll,
#          conCovRoll2 = case_when(
#     is.na(conCovRoll) ~ conCov,
#     TRUE ~ conCovRoll
#   )) %>% filter(pixelID == pixelIDs[1])
# 
# conCovDF$conCov <- conCovDF2$conCovRoll2


