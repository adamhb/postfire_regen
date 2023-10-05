library(tidyverse)
library(sf)
source('utils/system_settings.R')
makeFigs <- T

options(max.print = 1000)
options(dplyr.print_max = 1e4)

#load field validation data
fieldValidationData_LI <- read_csv('data/fieldValidationData_LI.csv') 
fieldValidationData_PI <- read_csv("data/fieldValidationData_PI.csv") 
fieldValidationData <- rbind(fieldValidationData_LI, fieldValidationData_PI)



#add field validation plot spatial data
#Loading the centroids of the line intercept data
lineInterceptValidationPoints <- st_read(dsn = "data/samplePoints_11_1_2021/samplePoints_11_1_2021.shp") %>%
  drop_na(name) %>%
  mutate(plotID = case_when(
    name == "A11" ~ "A011",
    name == "A10" ~ "A010",
    TRUE ~ as.character(name)
  )) %>% select(-time,-name) %>%
  st_zm(drop = TRUE, what = "ZM")

#getting the circles (i.e. validation plots for the line intercept data)
lineInterceptValidationCircles <- st_buffer(lineInterceptValidationPoints,90)

#st_write(obj = lineInterceptValidationPoints,dsn = "data/lineInterceptValidationPoints.shp", driver = "ESRI Shapefile")
#st_write(obj = lineInterceptValidationCircles,dsn = "data/lineInterceptValidationCircles.shp", driver = "ESRI Shapefile")

#centroids of all validation plots
pointInterceptCentroids <- st_read(dsn = 'data/pointInterceptCentroids.shp')

st_crs(pointInterceptCentroids)
st_crs(lineInterceptValidationPoints)

validationPlotCentroids <- rbind(lineInterceptValidationPoints,pointInterceptCentroids)

#exporting validation plot centroids for google earth engine
#validationPlotCentroids %>% st_transform(4326) %>% 
#  st_write(dsn = "data/validationPlotCentroids.shp",driver = "ESRI Shapefile")


#compare predictions of conifer probability to percent conifer cover

#load RS predictions
validationPlotRSPredictions <- read_csv('data/validationPlotPredictions_3_22_22.csv') %>%
  select(plotID,pctConCov2021_mean) %>%
  drop_na(plotID) %>%
  rename(pctCoverRS = pctConCov2021_mean)
  #mutate_at(.vars = "ConProb", .funs = function(x){x/100})


#join with field data
validationDF <- fieldValidationData %>%
  filter(pft == "c") %>%
  left_join(validationPlotRSPredictions, by = "plotID") 

# categoricalDF <- fieldValidationData %>%
#   filter(pft == "c") %>%
#   left_join(validationPlotRSPredictions, by = "plotID") %>%
#   mutate(coniferDomPred = pctCoverRS > 0.5) %>%
#   mutate(coniferDomRef = pctCover > 0.5) %>%
#   select(coniferDomPred, coniferDomRef) %>%
#   table() 
  

#############################
#fitting a logistic model####
#############################
#to predict pct cover as a function of conifer prob

linearMod <- lm(validationDF, formula = pctCover ~ pctCoverRS)
summary(linearMod)


#shrub cov data

#point shrub cov
shrubCovPoint <- read_csv("data/shrubCov.csv") %>% select(plotID,shrubCov) 

#line shrub cov
AllshrubCov <- fieldValidationData_LI %>% filter(pft == "s") %>% rename(shrubCov = pctCover) %>%
  select(-pft) %>% rbind(shrubCovPoint)

validationFig <- validationDF %>%
  left_join(AllshrubCov, by = "plotID") %>%
  mutate(LotsOfShrub = shrubCov > 0.5) %>%
  ggplot(aes(pctCover,pctCoverRS, shape = LotsOfShrub)) +
  geom_point(size = 3) +
  geom_abline(slope=1, intercept=0, linetype = "dashed") +
  xlab("Observed conifer foliage projective cover") +
  ylab("Predicted conifer foliage projective cover") +
  #xlab(label = expression(atop("Predicted conifer canopy leaf area index [m2 m-2]") +
  #ylab('Conifer cover observed in field \n [% of surface area]') +
  scale_x_continuous(limits = c(0,0.8),breaks = seq(0,0.8,0.1)) +
  scale_y_continuous(limits = c(0,0.8),breaks = seq(0,0.8,0.1)) +
  scale_shape_manual(values = c(1,2)) +
  adams_theme +
  theme(legend.position = "none")

#validMod <- lm(data = validationDF,formula = pctCover ~ pctCoverRS)
#summary(validMod)

makePNG(fig = validationFig, path_to_output.x = fig_path, file_name = "validationFig", res = 400, width = 6, height = 6)




##########plotting predictions against training data#############

trainingPlotRSPredictions <- read_csv('data/trainingAreaPredictions/trainingPlotPredictions_4_26_2022.csv') %>%
  select(fid,mean,pctCover,year) 

linearMod <- lm(trainingPlotRSPredictions, formula = pctCover ~ mean)
summary(linearMod)

trainingPlotRSPredictionsFig <- trainingPlotRSPredictions %>%
  ggplot(aes(pctCover,mean)) +
  geom_point(size = 3, shape = 1) +
  geom_abline(slope=1, intercept=0, linetype = "dashed") +
  xlab("Observed conifer foliage projective cover") +
  ylab("Predicted conifer foliage projective cover") +
  #xlab(label = expression(atop("Predicted conifer canopy leaf area index [m2 m-2]") +
  #ylab('Conifer cover observed in field \n [% of surface area]') +
  scale_x_continuous(limits = c(0,0.8),breaks = seq(0,0.8,0.1)) +
  scale_y_continuous(limits = c(0,0.8),breaks = seq(0,0.8,0.1)) +
  adams_theme +
  theme(legend.position = "none")

makePNG(fig = trainingPlotRSPredictionsFig, path_to_output.x = fig_path, file_name = "trainingPlotRSPredictionsFig", res = 400, width = 6, height = 6)







# logitMod <- glm(data = forLogit, formula = conProbNorm ~ pctCover, family = "binomial")
# predictions <- predict(object = logitMod,newdata = tibble(pctCover = forLogit$pctCover),type = "response")
# forLogit$predictions <- predictions

# logitModInverse <- glm(data = forLogit, formula = pctCover/0.7228916 ~ conProbNorm, family = "binomial",)
# predictionsInverse <- predict(object = logitModInverse,newdata = tibble(conProbNorm = forLogit$conProbNorm),type = "response")
# forLogit$predictionsInverse <- predictionsInverse

##################
#validation figs###
##################
# logMod <- lm(data = validationDF, formula = pctCover ~ log(ConProb))
# validationDF$pctCovPreds <- predict(object = logMod,newdata = validationDF)
# summary(logMod)
# 
# validationFigInverse <- validationDF %>%
#   ggplot(aes(ConProb, pctCover) ) +
#   geom_point() +
#   geom_vline(xintercept = 0.5, linetype = "dotted") +
#   geom_line(mapping = aes(ConProb,pctCovPreds), linetype = "dashed") +
#   xlab('Predicted probability of conifer dominance \n (> 50% conifer canopy cover)') +
#   ylab('Fractional conifer canopy cover \n observed in field') +
#   #scale_x_continuous(limits = c(0,0.8),breaks = seq(0,8,0.1)) +
#   adams_theme
# makePNG(fig = validationFigInverse, path_to_output.x = figuresPath, file_name = "validationFig")
# 
# validationFigInverse_lowcon <- validationDF %>%
#   ggplot(aes(ConProb, pctCover) ) +
#   geom_point() +
#   geom_vline(xintercept = 0.5, linetype = "dotted") +
#   geom_line(mapping = aes(ConProb,pctCovPreds), linetype = "dashed") +
#   scale_y_continuous(limits = c(0,0.25)) +
#   scale_x_continuous(limits = c(0,0.25)) +
#   xlab('Predicted probability of conifer dominance \n (> 50% conifer canopy cover)') +
#   ylab('Fractional conifer canopy cover \n observed in field') +
#   #scale_x_continuous(limits = c(0,0.8),breaks = seq(0,8,0.1)) +
#   adams_theme
# makePNG(fig = validationFigInverse_lowcon, path_to_output.x = figuresPath, file_name = "validationLowConiferAreas")
# 
# 
# makePNG(validationFigInverse, "figures/","validationFigBag900")
# 
# validationFig <- validationDF %>%
#   ggplot(aes(pctCover,ConProb)) +
#   geom_point() +
#   geom_line(data = forLogit, mapping = aes(pctCover,predictions), linetype = "dotted") +
#   geom_vline(xintercept = 0.5, linetype = "dashed") +
#   ylab('Predicted probability of conifer dominance \n (> 50% conifer canopy cover)') +
#   xlab('Conifer canopy cover (% of total ground area) \n observed in field') +
#   scale_x_continuous(limits = c(0,0.8),breaks = seq(0,8,0.1)) +
#   adams_theme
# 
# #makePNG(fig = validationFig,path_to_output.x = "figures/", file_name = "validationFig.png")
# 
# 
# #Interpretation of conifer probability
# 
# #When the predicted conifer probability is > 50% there are always conifers present in the field
# #Conifers could occupy as little as 20% of hte canopy, but more likely 50-60% of the canopy
# ggplot(validationDF %>% filter(ConProb > 0.5), 
#        aes(x=pctCover)) + 
#   geom_histogram(binwidth=0.01) +
#   xlab("Observed Pct cover") +
#   labs(title = "When predicted ConProb > 50%") +
#   adams_theme
# # Change colors
# 
# 
# # 5-50 percent probability means anywhere from 20-40 of canopy (significant recovery, but not domianting the canopy)
# ggplot(validationDF %>% filter(ConProb < 0.5 & ConProb > 0.20), 
#        aes(x=pctCover)) + 
#   geom_histogram(binwidth=0.01) +
#   xlab("Observed Pct cover") +
#   scale_x_continuous(limits = c(0,0.7)) +
#   labs(title = "When predicted ConProb between 5 and 20%") +
#   adams_theme
# 
# # < 1 percent probability of recovery means that conifers are either not there or comprise
# # (most likely) < 2.5%, potentially up to 17%
# ggplot(validationDF %>% filter(ConProb < 0.01), 
#        aes(x=pctCover)) + 
#   geom_histogram(binwidth=0.001) +
#   xlab("Observed Pct cover") +
#   labs(title = "When predicted ConProb < 1%") +
#   adams_theme

#The next classification goal would be to develop an algorithm that can better distinguish
#between plots that have conifers as a small fraction of the pixel window
#versus plots that do not have any conifers












#scratch below
# 
# sigmoid = function(params, x) {
#   params[1] / (1 + exp(-params[2] * (x - params[3])))
# }
# 
# #the followsing is taken from: https://stats.stackexchange.com/questions/242594/automatically-finding-starting-values-for-a-sigmoid-curve
# 
# A.x = 0
# B.x = max(validationDF$pctCover)
# 
# forLogit <- validationDF %>% filter(pctCover > 0 & pctCover < B) %>%
#   mutate(pctCoverNorm = (pctCover - A) / B)
# 
# logitModel <- glm(data = forLogit, formula = pctCoverNorm ~ ConProb, family = binomial(link = "logit"))
# 
# a.x <- as.numeric(coefficients(logitModel)[2])
# b.x <- as.numeric(coefficients(logitModel)[1])
# 
# sigmoid_ahb <- function(A = A.x,B = B.x,a = a.x,b = b.x,x = validationDF$ConProb){
#   return( A + B * ( 1 / ( 1 + exp( -(a*x + b) ) ) ) ) 
# }
# 
# preds <- tibble(x = validationDF$ConProb, y = sigmoid_ahb())
# 
# validationPlot +
#   geom_point(data = preds, mapping = aes(x,y), color = "red")
# 
# fitmodel <- nls(y ~ A + B * ( 1 / ( 1 + exp( -(a*x + b) ) ) ), start=list(a=a.x,b=b.x,A=A.x, B=B.x) )
# 
# fitmodel <- nls(y ~ a/(1 + exp(-b * (x-c))), start=list(a=1,b=1,c=0.00001))
# 
# #z <- z[-10]
# #z <- z[z!=0]
# 
# lm(formula = z ~ x, family = "logit")
# 
# # fitting code
# fitmodel <- nls(y~a/(1 + exp(-b * (x-c))), start=list(a=1,b=1,c=0.00001))
# # visualization code
# # get the coefficients using the coef function
# params=coef(fitmodel)
# 
# y2 <- sigmoid(params,x)
# plot(y2,type="l")
# points(y)
# 












