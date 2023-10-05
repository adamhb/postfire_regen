#See trial 100 as a good model.
#Trial 100 could be good model to go with. It is qualitatively similar to the one in the manuscript already, so it might be ok to just use the one in the MS.
#The model and model data of trail 100 has been saved.

trialN <- 101
sub <- T
k.default = 10
sampleSize <- 30

source('00_setup.R')

 modData <- df4 #%>%
   #mutate(across(where(is.numeric),scale_this))

 modDataSub <- tibble()

 for(fa in unique(modData$focalAreaID)){
   d <- modData %>% filter(focalAreaID == fa)
   if(nrow(d) < sampleSize){
     tmp <- d
   }else{
     tmp <- sample_n(d,size = sampleSize)
   }
   modDataSub <- rbind(modDataSub,tmp)
 }

 if(sub == T){
   modData <- modDataSub
 }

 
#modData <- read_csv('data/mod_3-2022-04-02 17:13:55data.csv')
#modData <- modData %>% mutate_at(.vars = "RRI_end", .funs = function(x){ (x * 0.3856) + 0.5110378 } )

mod <- gam(formula = RRI_end ~ 
             recoveryTime + 
             s(x, y, bs = "gp", k = 75, m = 2) +
             #s(fire, bs="re") +
             mediumReburns +
             #s(lowReburns, k = k.default) +
             #postFirePlanting +
             burnSev +
             #tpi +
             #s(PPThist) +
             s(TmaxHist, k = 15) +
             #s(tmaxYr1_3_mean, k = 15) +
             #adj_fold_northness +
             s(adj_fold_northness, k = 15) +
             #s(hli, k = k.default) + 
             #s(slope, k = k.default) +
             #s(CWDhist, k = k.default) +
             #s(AEThist, k = 15) +
             s(postFireSAP, k = 15) +
             s(pptYr1_3_annual_mean, k = 15),
           #s(ppt_Yr1_3_mean_annual_anom, k = 15),
           data = modData)

#for correlation matrix
otherVars.x <- c("recoveryTime","mediumReburns","lowReburns")
topoVar.x  <- c("slope","hli")
histClimateVars.x <- c("AEThist","CWDhist")
seedVars.x <- c("burnSev","postFireSAP","postFirePlanting")
postFireWeather.x <- c("pptYr1_3_annual_mean")
predictors.x <- c(otherVars.x,topoVar.x,histClimateVars.x,seedVars.x,postFireWeather.x)
getCorrelationMatrix(d = modData, varsForCorr = predictors.x)



#check spatial autocorrelation
pixel.dists <- as.matrix(dist(cbind(modData$x, modData$y)))
pixel.dists.inv <- 1/pixel.dists
diag(pixel.dists.inv) <- 0

#store model for making table and comparing AICs later
modName <- paste0("mod_",trialN,"-",Sys.time(),'.RDS')
dataName <- paste0("mod_",trialN,"-",Sys.time(),"data.csv")
#saveRDS(object = mod, file = paste0('data/',modName))
#write_csv(modData,file = paste0('data/',dataName))


#mod <- readRDS("data/mod_3-2022-04-02 17:13:55.RDS")
#view results
summary(mod)
gam.check(mod)
plot(mod, rug = T, all.terms = T, residuals = T, pch = 1, cex = 1, shift = coef(mod)[1], scheme = 2)

print(Moran.I(residuals(mod),pixel.dists.inv))
print(concurvity(mod, full = F))
paste0("AIC:",AIC(mod))
#write_csv(modDataSub,file = "data/modDataSub_StewartModel.csv")