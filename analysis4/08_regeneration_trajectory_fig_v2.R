source('~/cloud/gdrive/fire_project/postfire_regen/analysis4/00_setup_A4.R')


#png options
PNGheight = 5
PNGwidth = 8 #usually 8
PNGunits = "in"
PNGres = 100


##Load data and filters
#This dataset is created in 01_clean_A4
df6 <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/modelFittingData_030623.csv')
df5 <- read_csv("~/cloud/gdrive/fire_project/postfire_regen/analysis4/timeSeriesData_030623.csv")[-1]

smooth_trajectories <- read_csv("analysis4/08_smooth_trajectories_start_at_fire_year.csv")

recovery_rate_ARI_end_smooth <- read_csv("~/cloud/gdrive/fire_project/postfire_regen/analysis4/recovery_rate_ARI_end_smooth.csv")

notPlanted <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/not_planted_040323.csv') 

#load domain filters
nf <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf.csv')
nf_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf_megram_reduced.csv')
wild <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild.csv')
wild_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild_megram_reduced.csv')


df6_filter <- df6 %>%
  filter(pixelID %in% notPlanted$pixelID)
print(paste("sample size:",nrow(df6_filter)))
print(paste("Mean ARI on full NF domain no spatail rarification",mean(df6$ARI_end_smooth)))

#Calculate some statistics for the results section

#How many recovered?
n_recovered <- df6_filter %>%
  mutate(RRI_smooth = ARI_end_smooth / disturbanceSize) %>%
  filter(RRI_smooth > 0.9) %>%
  nrow()

pct_recovered <- n_recovered / nrow(df6_filter)
print(paste("percent recovered:",pct_recovered * 100))


#Mean recovery of reburned vs. non-reburned pixels 25 years after fire

#get df of burned
burnSevDF <- df6_filter %>% dplyr::select(pixelID,burnSev,fireYear,mediumReburns,wilderness)





#filter the time series data
domain_name <- "nf"
df5_filter <- df5 %>%
  filter(pixelID %in% notPlanted$pixelID) %>%
  left_join(burnSevDF,by = "pixelID") %>%
  left_join(recovery_rate_ARI_end_smooth, by = "pixelID") %>%
  mutate(recoveryTime = year - fireYear) %>%
  filter(recoveryTime >= -3)
print(paste("sample size:",nrow(df5_filter)))





burn_quants <- quantile(probs = c(0.33,0.66),df5_filter$burnSev)

#create dataframe to make the trajectory figure 
traj_fig_df <- df5_filter %>% 
  dplyr::select(pixelID,recoveryTime,year,burnSev,ARI,RRI,
                postFirePrecip,postFireMaxT,recovery_rate,
                disturbanceSize,
                mediumReburns,wilderness) %>%
  mutate(reburn_g = case_when(
  mediumReburns < 0.05 ~ "No reburn",
  mediumReburns >= 0.05 ~ "Reburn"
)) %>% mutate(burnSev_g = case_when(
  burnSev < 3.33 ~ "3.00-3.32",
  (burnSev >= 3.33 & burnSev <= 3.66) ~ "3.33-3.66",
  burnSev > 3.66 ~ "3.67-4.00"
)) %>% mutate(recovered_g = case_when(
  recovery_rate == 0 ~ FALSE,
  recovery_rate > 0 ~ TRUE
))


#calculate mean recovery of reburned vs. non-reburned
RRI_df <- traj_fig_df %>%
  dplyr::select(pixelID,reburn_g) %>%
  left_join(df6_filter, by = "pixelID") %>%
  distinct() %>% 
  mutate(RRI_smooth = ARI_end_smooth / disturbanceSize) 


RRI_df %>%
  group_by(reburn_g) %>%
  summarise(RRI_smooth_g = mean(RRI_smooth))


RRI_reburn <- RRI_df %>% filter(reburn_g == "Reburn") %>% pull(RRI_smooth)
RRI_noreburn <- RRI_df %>% filter(reburn_g == "No reburn") %>% pull(RRI_smooth)

t.test(RRI_reburn,RRI_noreburn)


sample_size <- traj_fig_df %>% group_by(recoveryTime,burnSev_g,reburn_g) %>%
  summarise(n = length(postFireMaxT)) %>%
  ungroup()

traj_fig_df <- traj_fig_df %>%
  left_join(sample_size, by = c("recoveryTime","reburn_g", "burnSev_g")) %>%
  mutate_at(.vars = "burnSev_g",.funs = function(x){factor(x,levels = c("3.00-3.32","3.33-3.66","3.67-4.00"))}) #%>%
  #mutate_at(.vars = "reburn_g",.funs = function(x){factor(x,levels = c("No reburn","Reburn"))} )


#pixels <- sample(traj_fig_df$pixelID,5)
#sample(pixels,size = 5)
#table(traj_fig_df$reburn_g)

#traj_fig_df %>%
#  filter(recoveryTime > 0) %>% 
#  #filter(pixelID %in% pixels) %>%
#  filter(n > 30) %>%

recovery_time_jitter <- c()
for(i in 1:length(traj_fig_df$recoveryTime)){
  recovery_time_jitter[i] <- runif(1,traj_fig_df$recoveryTime[i],traj_fig_df$recoveryTime[i]+1)
}
traj_fig_df$recoveryTime_jitter = recovery_time_jitter

smooth_trajectories <- smooth_trajectories %>% dplyr::select(pixelID,year,ARI_smooth)

traj_fig_df_with_smooth <- traj_fig_df %>%
  left_join(smooth_trajectories, by = c("pixelID","year")) %>%
  mutate(RRI_smooth = ARI_smooth / disturbanceSize)

smoothpix = unique(smooth_trajectories$pixelID)
pix = unique(traj_fig_df$pixelID)
sum(smoothpix %in% pix)


traj_fig_nf <- traj_fig_df_with_smooth %>%
  filter(recoveryTime > 0) %>% 
  #filter(pixelID %in% pixels) %>%
  filter(n > 30) %>%
  ggplot(aes(recoveryTime, RRI)) +
  geom_line(aes(group = pixelID),alpha = 0, color = "black") +
  geom_line(aes(recoveryTime, RRI_smooth, group = pixelID),alpha = 0.01, color = "black") +
  #geom_smooth(aes(color = burnSev_g), method = "loess", span = 1, ) +
  geom_smooth(aes(color = burnSev_g),
              method = "gam", formula = y ~ s(x, k = 5),
              alpha = 1, level = 0.95, size = 1.5, fill = "black") + 
  
  scale_color_manual(aesthetics = "color", values = c("skyblue","gold","red")) +
  facet_grid(cols = vars(reburn_g),drop = FALSE) +
  labs(color = "Burn severity", title = "NF domain") +
  ylab(label = "Relative recovery index") +
  xlab(label = "Time since fire [yrs]") +
  scale_y_continuous(limits = c(-0.2,1.2),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  adams_theme + 
  theme(legend.position = c(0.6,0.7),
        legend.spacing.y = unit(0.1, "cm"),
        legend.text = element_text(size = 10),
        #legend.background = "white",
        legend.box.background = element_rect(color = "white"),
        plot.title = element_text(size = 20),
        legend.key.size = unit(1.5,"lines")) +
  guides(
    color = guide_legend(override.aes = list(fill = NA)) # Remove the confidence interval fill in the legend
  )
  

#traj_fig_nf
#makePNG(fig = traj_fig_nf,path_to_output.x = "analysis4/figs/", file_name = "traj_fig_NF")



wild_pixel_n <- traj_fig_df_with_smooth %>%
  filter(wilderness == 1) %>% pull(pixelID) %>% unique() %>% length()
print(paste("Wilderness pixel n:",wild_pixel_n))


traj_fig_wild <- traj_fig_df_with_smooth %>%
  filter(recoveryTime > 0 & wilderness == 1) %>% 
  #filter(pixelID %in% pixels) %>%
  filter(n > 30) %>%
  ggplot(aes(recoveryTime, RRI)) +
  geom_line(aes(group = pixelID),alpha = 0, color = "black") +
  geom_line(aes(recoveryTime, RRI_smooth, group = pixelID),alpha = 0.01, color = "black") +
  #geom_smooth(aes(color = burnSev_g), method = "loess", span = 1, ) +
  geom_smooth(aes(color = burnSev_g), method = "gam", formula = y ~ s(x, k = 5), 
              fill = "black", alpha = 0.8, level = 0.95, size = 1.5) + 
  
  scale_color_manual(aesthetics = "color", values = c("skyblue","yellow","red")) +
  facet_grid(cols = vars(reburn_g),drop = FALSE) +
  labs(color = "Burn severity",title = "Wilderness domain") +
  ylab(label = "Relative recovery index") +
  xlab(label = "Time since fire [yrs]") +
  scale_y_continuous(limits = c(-0.2,1.2),breaks = c(0,0.2,0.4,0.6,0.8,1)) +
  adams_theme +
  theme(legend.position = "none",
        legend.spacing.y = unit(0.05, "cm"),
        legend.text = element_text(size = 10),
        #legend.background = "white",
        legend.box.background = element_rect(color = "white"),
        plot.title = element_text(size = 20))


#traj_fig_wild
#makePNG(fig = traj_fig_nf,path_to_output.x = "analysis4/figs/", file_name = "traj_fig_wild")

#library(cowplot)

traj_figs <- plot_grid(traj_fig_nf,traj_fig_wild,align = "h",nrow = 2)
makePNG(fig = traj_figs,path_to_output.x = "analysis4/figs/", file_name = "traj_figs",height = 8,width = 10)

# traj_fig_df %>% group_by(precip_g,temp_g) %>%
#   summarise(n_recovering = sum(recovered_g),
#             n = length(recovered_g)) %>%
#   mutate(frac_recovering = n_recovering / n)



# burnSev < burn_quants[1] ~ "3.00 - 3.25",
# burnSev >= burn_quants[1] & burnSev <= burn_quants[2] ~ "3.25 - 3.75",
# burnSev > burn_quants[2] ~ "3.75 - 4.00"
