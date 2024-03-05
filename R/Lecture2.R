## -------------------------------------------------- ##
## EDSD 2023-2024: Demographic Forecasting
## Lecture 2
## Direct extrapolation by time-series methods
## Date: 05/03/2024
## Instructor: Ugofilippo Basellini
## -------------------------------------------------- ##

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(forecast)

## loading data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("data/TimeSeries.Rdata")

## start by inspecting our data
head(sp500.data)

## subset the data
my.df <- sp500.data %>% 
  filter(Date>="1990-01-01") %>% slice(1:250)

## extract the data for plotting
y.all <- my.df$SP500
t.all <- my.df$Date
y <- y.all[1:200]
t <- t.all[1:200]
n <- length(t)

## plotting the data
plot(t.all,y.all)
points(t,y,pch=16)

## plot the acf
acf(y)

## first differences of the series
y.diff <- diff(y)
plot(t[-1],y.diff,t="l")
acf(y.diff)

## fitting two RW models
mod1 <- Arima(y,order = c(0,1,0),include.drift = F)
mod2 <- Arima(y,order = c(0,1,0),include.drift = T)
summary(mod1)
summary(mod2)

## forecasting the time series
tF <- t.all[!t.all%in%t]
nF <- length(tF)
y.fore1 <- forecast(mod1,h=nF)
plot(y.fore1)
y.fore2 <- forecast(mod2,h=nF)
plot(y.fore2)

plot(t.all,y.all,ylim=range(y.all,y.fore2$mean))
points(t,y,pch=16)
lines(tF,y.fore1$mean,lwd=2,col=2)
lines(tF,y.fore2$mean,lwd=2,col=4)


## ---- EXERCISE 2 ----

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(forecast)

## loading data
load("data/FertSWE.Rdata")

## subset of my data
my.df <- FERT.SWE %>% filter(Age==20, Year >= 1950)

## extract the data of interest
lfx <- my.df$logRates
t <- my.df$Year
n <- length(t)
plot(t,lfx)

## extraplotaing to the future - way 1
t.all <- t[1]:2050
tF <- t.all[!t.all%in%t]
nF <- length(tF)

## moving now to a GLM
mod.glm <- glm(Births~Year,data = my.df,
               family = poisson(),offset=log(Exposures))
summary(mod.glm)

## forecast of the GLM
eta.fore.glm <- coef(mod.glm)[1] + coef(mod.glm)[2]*tF

## plotting the forecast (log scale)
plot(t,my.df$logRates,ylim=range(my.df$logRates,eta.fore.glm),
     xlim=range(t.all))
lines(tF,eta.fore.glm,col=4,lwd=2)

## moving to time-series models
mod1 <- Arima(y=lfx,order = c(0,1,0),include.drift = T)
mod2 <- auto.arima(y=lfx)
summary(mod1)
summary(mod2)

## forecast of time-series models
y.fore1 <- forecast(mod1,h=nF)
plot(y.fore1)
y.fore2 <- forecast(mod2,h=nF)
plot(y.fore2)


## plotting the forecast (log scale)
plot(t,my.df$logRates,
     ylim=range(my.df$logRates,eta.fore.glm,y.fore2$lower),
     xlim=range(t.all))
lines(tF,eta.fore.glm,col=4,lwd=2)
lines(tF,y.fore1$mean,col=2,lwd=2)
lines(tF,y.fore2$mean,col=5,lwd=2)
## plot the PI for ts models
lines(tF,y.fore1$lower[,1],col=2,lwd=2,lty=2)
lines(tF,y.fore1$upper[,1],col=2,lwd=2,lty=2)
lines(tF,y.fore2$lower[,1],col=5,lwd=2,lty=2)
lines(tF,y.fore2$upper[,1],col=5,lwd=2,lty=2)

## --- simulations
lfx.sim <- simulate(object = mod2,nsim=nF,
                    future = T,bootstrap = T)

plot(t,my.df$logRates,
     ylim=range(my.df$logRates,eta.fore.glm,y.fore2$lower),
     xlim=range(t.all))
lines(tF,lfx.sim)

## number of simulations
nS <- 100
LFXsim <- matrix(NA,nF,nS)
for (i in 1:nS){
  ## genereate 1 simulation
  lfx.sim <- simulate(object = mod2,nsim=nF,
                      future = T,bootstrap = T)
  ## save the simulation
  LFXsim[,i] <- lfx.sim
}
plot(t,my.df$logRates,
     ylim=range(my.df$logRates,eta.fore.glm,y.fore2$lower),
     xlim=range(t.all))
matlines(tF,LFXsim,col="grey30",lty=1)

## comparing 80% PI
lev <- 80
lev.p <- lev/100
lfx.sim.median <- apply(LFXsim,1,median)
lfx.sim.low <- apply(LFXsim,1,quantile,prob=(1-lev.p)/2)
lfx.sim.up <- apply(LFXsim,1,quantile,prob=1-(1-lev.p)/2)

plot(t,my.df$logRates,
     ylim=range(my.df$logRates,eta.fore.glm,y.fore2$lower),
     xlim=range(t.all))
matlines(tF,LFXsim,col="grey70",lty=1)
lines(tF,lfx.sim.median,col="darkred",lwd=2)
lines(tF,lfx.sim.up,col="darkred",lwd=2,lty=2)
lines(tF,lfx.sim.low,col="darkred",lwd=2,lty=2)
## theoretical
lines(tF,y.fore2$mean,col=5,lwd=2)
lines(tF,y.fore2$lower[,1],col=5,lwd=2,lty=2)
lines(tF,y.fore2$upper[,1],col=5,lwd=2,lty=2)
