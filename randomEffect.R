library(mgcv)
## simple examples using gamm as alternative to gam
set.seed(0) 
dat <- gamSim(1,n=200,scale=2)
b <- gamm(y~s(x0)+s(x1)+s(x2)+s(x3),data=dat)
plot(b$gam,pages=1)
summary(b$lme) # details of underlying lme fit
summary(b$gam) # gam style summary of fitted model
anova(b$gam) 
gam.check(b$gam) # simple checking plots

b <- gamm(y~te(x0,x1)+s(x2)+s(x3),data=dat) 
op <- par(mfrow=c(2,2))
plot(b$gam)
par(op)
rm(dat)

list(fac=~1)
## Add a factor to the linear predictor, to be modelled as random
dat <- gamSim(6,n=200,scale=.2,dist="poisson")
b2 <- gamm(y~s(x0)+s(x1)+s(x2),family=poisson,
           data=dat,random=list(fac=~1))
plot(b2$gam,pages=1)
fac <- dat$fac
rm(dat)
vis.gam(b2$gam)




for(x in unique(df4$fire)){
  fa <- as.numeric(str_extract(x,'[:digit:]{2}'))
  year <- as.numeric(str_extract(x,'[:digit:]{4}'))
  expression <- paste0("var years = [",year+1,",",year+2,",",year+3,"]","\n",
                       'years.map(exportSAP_',fa,");")
  write(x = expression, file = "file6.txt", append = T)
}






