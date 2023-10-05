mod <- readRDS("data/mod_13-2022-04-02 18:18:12.RDS")

#prep data for ppt1-3 figure
m <- mean(df4$pptYr1_3_annual_mean)
sdx <- sd(df4$pptYr1_3_annual_mean)
df <- plot(mod)
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
  geom_point(data = obs, mapping = aes(x,y)) +
  xlab("Mean annual PPT in first 3 years after fire [mm]") +
  ylab("s(pptYr1-3)") +
  scale_x_continuous(limits = c(500,2100), breaks = c(500,1000,1500,2000,2500,3000,3500,4000)) +
  scale_y_continuous(limits = c(-16,5)) +
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
obs <- tibble(x = x.obs, y = -5)

figb <- figData %>%
  ggplot(aes(x,fit)) +
  geom_line() +
  geom_line(aes(x,down95), linetype = "dotted") +
  geom_line(aes(x,up95), linetype = "dotted") +
  geom_point(data = obs, mapping = aes(x,y)) +
  xlab("Mean daily maximum temperature [C]") +
  ylab("s(Tmax)") +
  scale_y_continuous(limits = c(-5,2)) +
  adams_theme



#makePNG(fig = figa, path_to_output.x = fig_path, file_name = "BestModPPT1-3")
