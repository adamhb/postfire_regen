mod <- readRDS("data/mod_13-2022-04-02 18:18:12.RDS")
#df <- plot(mod)
#write_rds(df, "data/Fig_bestMod_pptYr1-3_Tmax.Rds")
df <- read_rds("data/Fig_bestMod_pptYr1-3_Tmax.Rds")


preds2 <- predict(mod, type='terms')[,'s(pptYr1_3_annual_mean)']

#prep data for ppt1-3 figure
m <- mean(df4$pptYr1_3_annual_mean)
sdx <- sd(df4$pptYr1_3_annual_mean)
x <- ((as.numeric(df[[5]]$x)) * sdx) + m
x.obs <- as.numeric(df[[5]]$raw) * sdx + m
fit <- as.numeric(df[[5]]$fit)
down95 <- fit - df[[5]]$se
up95 <- fit + df[[5]]$se
figData <- tibble(x = x, fit = fit, down95 = down95, up95 = up95) 
obs <- tibble(x = x.obs, y = -15)


figa <- figData %>%
  ggplot(aes(x,fit)) +
  geom_line() +
  geom_line(aes(x,down95), linetype = "dotted") +
  geom_line(aes(x,up95), linetype = "dotted") +
  geom_point(data = obs, mapping = aes(x,y), shape = 4) +
  xlab("Mean annual PPT in \n first 3 years after fire [mm]") +
  ylab("s(PPT_Yr1-3)") +
  scale_x_continuous(limits = c(500,2100), breaks = c(500,1000,1500,2000,2500,3000,3500,4000)) +
  scale_y_continuous(limits = c(-15,5)) +
  adams_theme

#prep data for temp figure
m <- mean(df4$TmaxHist)
sdx <- sd(df4$TmaxHist)
x <- ((as.numeric(df[[2]]$x)) * sdx) + m
x.obs <- as.numeric(df[[2]]$raw) * sdx + m
fit <- as.numeric(df[[2]]$fit)
down95 <- fit - df[[2]]$se
up95 <- fit + df[[2]]$se
figData <- tibble(x = x, fit = fit, down95 = down95, up95 = up95) 
obs <- tibble(x = x.obs, y = -4)

figb <- figData %>%
  ggplot(aes(x,fit)) +
  geom_line() +
  geom_line(aes(x,down95), linetype = "dotted") +
  geom_line(aes(x,up95), linetype = "dotted") +
  geom_point(data = obs, mapping = aes(x,y), shape = 4) +
  xlab(expression(atop("Historical mean daily",paste("maximum temperature ["*degree*C,"]")))) +
  ylab("s(Tmax)") +
  scale_y_continuous(limits = c(-4,2), breaks = seq(-4,2,2)) +
  adams_theme




#prep data for northness figure
m <- mean(df4$adj_fold_northness)
sdx <- sd(df4$adj_fold_northness)
x <- ((as.numeric(df[[3]]$x)) * sdx) + m
x.obs <- as.numeric(df[[3]]$raw) * sdx + m
fit <- as.numeric(df[[3]]$fit)
down95 <- fit - df[[3]]$se
up95 <- fit + df[[3]]$se
figData <- tibble(x = x, fit = fit, down95 = down95, up95 = up95) 
obs <- tibble(x = x.obs, y = -0.75)

figc <- figData %>%
  ggplot(aes(x,fit)) +
  geom_line() +
  geom_line(aes(x,down95), linetype = "dotted") +
  geom_line(aes(x,up95), linetype = "dotted") +
  geom_point(data = obs, mapping = aes(x,y), shape = 4) +
  xlab("Adj-fold-northness") +
  ylab("s(Adj-fold-northness)") +
  #scale_x_continuous(limits = c(500,2100), breaks = c(500,1000,1500,2000,2500,3000,3500,4000)) +
  #scale_y_continuous(limits = c(-15,5)) +
  adams_theme

#prep data for SAP figure
m <- mean(df4$postFireSAP)
sdx <- sd(df4$postFireSAP)
x <- ((as.numeric(df[[4]]$x)) * sdx) + m
x.obs <- as.numeric(df[[4]]$raw) * sdx + m
fit <- as.numeric(df[[4]]$fit)
down95 <- fit - df[[4]]$se
up95 <- fit + df[[4]]$se
figData <- tibble(x = x, fit = fit, down95 = down95, up95 = up95) 
obs <- tibble(x = x.obs, y = -2)


figd <- figData %>%
  ggplot(aes(x,fit)) +
  geom_line() +
  geom_line(aes(x,down95), linetype = "dotted") +
  geom_line(aes(x,up95), linetype = "dotted") +
  geom_point(data = obs, mapping = aes(x,y), shape = 4) +
  xlab("Post-fire seed availability \n proxy (SAP)") +
  ylab("s(SAP)") +
  #scale_x_continuous(limits = c(500,2100), breaks = c(500,1000,1500,2000,2500,3000,3500,4000)) +
  #scale_y_continuous(limits = c(-15,5)) +
  adams_theme

#fig <- plot_grid(figa, figb, figc, figd, labels = c("(a)","(b)","(c)","(d)"), label_x = 0.1, label_y = 1.05, align = "hv")
fig <- plot_grid(figa, figb, figc, figd, align = "hv")
makePNG(fig = fig, path_to_output.x = fig_path, file_name = "BestModNonLinearTerms", height = 8, width = 10)












length(predict(mod))
modData <- read_csv('data/mod_3-2022-04-02 17:13:55data.csv')
nrow(modData)
plot(modData$pptYr1_3_annual_mean, predict(mod, type = "response"))


df <- plot(mod)
