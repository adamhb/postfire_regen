source('00_setup.R')

mod <- readRDS("data/mod_2-2022-04-02 16:52:03.RDS")
mod <- readRDS("data/mod_3-2022-04-02 17:13:55.RDS")
#mod <- readRDS("data/mod_4-2022-04-02 17:33:56.RDS"); same as mod3, but with increased x,y
mod <- readRDS("data/mod_5-2022-04-02 17:40:13.RDS")
mod <- readRDS("data/mod_7-2022-04-02 17:47:29.RDS")
mod <- readRDS("data/mod_9-2022-04-02 17:56:44.RDS")
mod <- readRDS("data/mod_10-2022-04-02 18:04:49.RDS")
mod <- readRDS("data/mod_11-2022-04-02 18:09:09.RDS")
mod <- readRDS("data/mod_12-2022-04-02 18:13:02.RDS")
mod <- readRDS("data/mod_13-2022-04-02 18:18:12.RDS")
mod <- readRDS("data/mod_16-2022-04-03 19:00:20.RDS")

AIC(mod)
summary(mod)

preds <- predict(mod, type = "response")

predict(mod, type = "terms")
