source('00_setup.R')
source('02_explore.R')

modDataPixels <- read_csv('data/mod_3-2022-04-02 17:13:55data.csv') %>% pull(pixelID) %>% unique()

modData <- df3

# modData <- df3 %>%
#  filter(pixelID %in% modDataPixels)

recoveryTrajsDF <- modData %>%
  mutate(postFirePrecipGroup = case_when(
    pptYr1_3_annual_mean <= 600 ~ "PPT_Yr1-3 <= 800 mm",
    pptYr1_3_annual_mean > 600 ~ "PPT_Yr1-3 > 800 mm"
  )) %>%
  mutate(reburnGroup = case_when(
    mediumReburns <= 0.5 ~ "Low re-burn fraction \n  [< 50% of pixel re-burned]",
    mediumReburns > 0.5 ~ "High re-burn fraction \n  [> 50% of pixel re-burned]"#,
    #TRUE ~ "mediumReburn"
  )) %>% 
  mutate(plantedGroup = case_when(
    postFirePlanting > 0.5 ~ "Planted",
    postFirePlanting < 0.1 ~ "Not planted",
    TRUE ~ "mediumPlanted"
  )) %>%
  filter(plantedGroup != "mediumPlanted") 


recoveryTrajsDF_mean <- recoveryTrajsDF %>%
  group_by(reburnGroup,plantedGroup,timeSinceFire) %>%
  summarise(RRI_mean = mean(RRI, na.rm = T))

axis_size <- 15
recoveryTrajFig <- recoveryTrajsDF_mean %>%
  ggplot(aes(timeSinceFire,RRI_mean)) +
  geom_line(data = recoveryTrajsDF, mapping = aes(timeSinceFire,RRI, group = pixelID, color = burnSev), alpha = 1) +
  facet_grid(rows = vars(plantedGroup), cols = vars(reburnGroup), drop = FALSE) +
  #scale_y_continuous(limits = c(-0.4,1.2)) +
  scale_x_continuous(limits = c(-2,35)) +
  geom_line(size = 2) +
  scale_colour_gradient2(low = "blue", mid = "yellow", high = "firebrick1", midpoint = 3.5,  name = "Burn severity") +
  adams_theme +
  ylab(label = "Relative recovery index") +
  xlab(lab = "Time since fire [yr]") +
  theme(strip.text.y = element_text(size = axis_size),
        strip.text.x = element_text(size = axis_size),
        legend.title = element_text())


makePNG(fig = recoveryTrajFig, path_to_output.x = fig_path, file_name = "RecoveryTrajFig", res = 300)


#statistical test for conclusions about the trajectories

#1. high re-burn pixel show less recovery at 2019 than low re-burn
str(recoveryTrajsDF)

lowReburnRRI <- recoveryTrajsDF %>%
  filter(timeSinceFire == 25, reburnGroup == "Low re-burn fraction \n  [< 50% of pixel re-burned]") %>%
  pull(RRI)

highReburnRRI <- recoveryTrajsDF %>%
  filter(timeSinceFire == 25, reburnGroup == "High re-burn fraction \n  [> 50% of pixel re-burned]") %>%
  pull(RRI)

t.test(x = lowReburnRRI, y = highReburnRRI)


#2. medium severity showed more recovery in 2019 than high severity

lowBurnSevRRI <- recoveryTrajsDF %>%
  filter(timeSinceFire == 25, burnSev < 3.25) %>%
  pull(RRI)

highBurnSevRRI <- recoveryTrajsDF %>%
  filter(timeSinceFire == 25, burnSev > 3.75) %>%
  pull(RRI)

t.test(x = lowBurnSevRRI, y = highBurnSevRRI)

#3. planting fraction
lowPlantingRRI <- recoveryTrajsDF %>%
  filter(timeSinceFire == 10, postFirePlanting < 0.1) %>%
  pull(RRI)

highPlantingRRI <- recoveryTrajsDF %>%
  filter(timeSinceFire == 10, postFirePlanting > 0.9) %>%
  pull(RRI)

t.test(x = lowPlantingRRI, y = highPlantingRRI)

npix <- length(unique(recoveryTrajsDF$pixelID))
pix <- unique(recoveryTrajsDF$pixelID)

recoveryTrajsDF %>% filter(time)

getMaxRRI <- function(pixID){
  maxRRI <- recoveryTrajsDF %>% 
    filter(pixelID == pixID,
           timeSinceFire > 2) %>%
    pull(RRI) %>% max()
  return(maxRRI)
}

maxRRIs <- c()
j <- 0
for(i in pix){
  j <- j + 1
  maxRRIs <- append(maxRRIs,getMaxRRI(i))
  print(j/length(pix))
}

sum(maxRRIs > 0.9) / npix




#HOW DOES BURN SEVERITY INFLUENCE TIME TO RECOVERY.
###################################################3#
#mean time to recover 50% RRI for low burn##########
####################################################
lowBurnSevTimeTo50pct <- recoveryTrajsDF %>%
  filter(burnSev < 3.25, RRI > 0.5, timeSinceFire > 2) %>%
  pull(timeSinceFire) 

highBurnSevTimeTo50pct <- recoveryTrajsDF %>%
  filter(burnSev > 3.75, RRI > 0.5, timeSinceFire > 2) %>%
  pull(timeSinceFire) 

t.test(lowBurnSevTimeTo50pct, highBurnSevTimeTo50pct)




lowBurnSevTimeTo50pct <- recoveryTrajsDF %>%
  filter(burnSev < 3.25) %>%
  group_by(timeSinceFire) %>%
  summarise(meanRRI = mean(RRI)) %>% 
  add_column(burntype = "mediumSeverityBurn")

highBurnSevTimeTo50pct <- recoveryTrajsDF %>%
  filter(burnSev > 3.75) %>%
  group_by(timeSinceFire) %>%
  summarise(meanRRI = mean(RRI)) %>% 
  add_column(burntype = "highSeverityBurn")


rbind(lowBurnSevTimeTo50pct,highBurnSevTimeTo50pct) %>%
  ggplot(aes(timeSinceFire,meanRRI, color = burntype)) +
  scale_x_continuous(limits = c(-2,35)) +
  geom_line(size = 2) +
  #scale_colour_gradient2(low = "blue", mid = "yellow", high = "firebrick1", midpoint = 3.5,  name = "Burn severity") +
  adams_theme +
  ylab(label = "Relative recovery index") +
  xlab(lab = "Time since fire [yr]") +
  theme(strip.text.y = element_text(size = axis_size),
        strip.text.x = element_text(size = axis_size),
        legend.title = element_text())


