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

recovery_rate_ARI_end_smooth <- read_csv("~/cloud/gdrive/fire_project/postfire_regen/analysis4/recovery_rate_ARI_end_smooth.csv")

#load domain filters
nf <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf.csv')
nf_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/nf_megram_reduced.csv')
wild <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild.csv')
wild_megram_reduced <- read_csv('~/cloud/gdrive/fire_project/postfire_regen/analysis4/wild_megram_reduced.csv')


df6_filter <- df6 %>%
  filter(pixelID %in% nf_megram_reduced$pixelID)
print(paste("sample size:",nrow(df6_filter)))

#get df of burnev
burnSevDF <- df6_filter %>% dplyr::select(pixelID,burnSev,fireYear,mediumReburns)

#filter the time series data
domain_name <- "nf_megram_reduced_1000m"
df5_filter <- df5 %>%
  filter(pixelID %in% nf_megram_reduced$pixelID) %>%
  left_join(burnSevDF,by = "pixelID") %>%
  left_join(recovery_rate_ARI_end_smooth, by = "pixelID") %>%
  mutate(recoveryTime = year - fireYear) %>%
  filter(recoveryTime >= -3)
print(paste("sample size:",nrow(df5_filter)))





burn_quants <- quantile(probs = c(0.33,0.66),df5_filter$burnSev)

#create dataframe to make the trajectory figure 
traj_fig_df <- df5_filter %>% 
  filter(mediumReburns < 0.1) %>%
  dplyr::select(pixelID,recoveryTime,burnSev,ARI,postFirePrecip,postFireMaxT,recovery_rate,RRI) %>%
  mutate(precip_g = case_when(
  postFirePrecip < 1000 ~ "low",
  postFirePrecip >= 1000 ~ "high"
)) %>% mutate(temp_g = case_when(
  postFireMaxT < 20 ~ "cool",
  postFireMaxT >= 20 ~ "warm"
)) %>% mutate(burnSev_g = case_when(
  burnSev < 3.5 ~ "medium",
  burnSev >= 3.5 ~ "high"
)) %>% mutate(recovered_g = case_when(
  recovery_rate == 0 ~ FALSE,
  recovery_rate > 0 ~ TRUE
))
  

sample_size <- traj_fig_df %>% group_by(recoveryTime, precip_g,temp_g,burnSev_g) %>%
  summarise(n = length(postFireMaxT)) %>%
  ungroup()

traj_fig_df <- traj_fig_df %>%
  left_join(sample_size, by = c("recoveryTime","precip_g","temp_g","burnSev_g"))


traj_fig <- traj_fig_df %>%
  filter(recoveryTime > 0) %>%
  filter(n > 5) %>%
  ggplot(aes(recoveryTime, RRI)) +
  geom_line(aes(group = pixelID, color = recovered_g),alpha = 0.1) +
  scale_color_manual(aesthetics = "color", values = c("red","blue","red","black")) +
  geom_smooth(aes(color = burnSev_g), method = "loess", span = 1) +
  facet_grid(rows = vars(precip_g), cols = vars(temp_g),drop = FALSE) +
  adams_theme 

traj_fig

makePNG(fig = traj_fig, file_name = "traj_fig_NF")


traj_fig_df %>% group_by(precip_g,temp_g) %>%
  summarise(n_recovering = sum(recovered_g),
            n = length(recovered_g)) %>%
  mutate(frac_recovering = n_recovering / n)



# burnSev < burn_quants[1] ~ "3.00 - 3.25",
# burnSev >= burn_quants[1] & burnSev <= burn_quants[2] ~ "3.25 - 3.75",
# burnSev > burn_quants[2] ~ "3.75 - 4.00"
