## -------------------------------------------------- ##
## EDSD 2023-2024: Demographic Forecasting
## Lecture 3
## Forecasting by paranetric approaches
## Date: 06/03/2024
## Instructor: Ugofilippo Basellini
## -------------------------------------------------- ##

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)
library(forecast)
library(viridis)

## loading data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("data/FertSWE.Rdata")

## subset of my data
my.df <- FERT.SWE %>% filter(Year >= 1950)

## extracting the data of interest
y <- my.df %>% filter(Year==2000) %>% 
  select(Births) %>% pull()
e <- my.df %>% filter(Year==2000) %>% 
  select(Exposures) %>% pull()
x <- unique(my.df$Age)
m <- length(x)
lfx <- log(y/e)
plot(x,lfx)

## age squared
x.sq <- x^2 

## moving now to a GLM
mod.glm <- glm(y~x+x.sq,family = poisson(),
               offset=log(e))
summary(mod.glm)
coef(mod.glm)

## alternative way
mod.glm2 <- glm(Births~Age+I(Age^2),
                data = my.df %>% filter(Year==2000),
                family = poisson(),
                offset=log(Exposures))
coef(mod.glm2)

## fitted values of the GLM
eta.hat.glm <- coef(mod.glm)[1] + coef(mod.glm)[2]*x +
  coef(mod.glm)[3]*x.sq 
eta.hat.glm2 <- log(predict(mod.glm,type="response")/e)
all.equal(eta.hat.glm,unname(eta.hat.glm2))  

## plotting the forecast (log scale)
plot(x,lfx,ylim=range(lfx,eta.hat.glm,finite=T))
lines(x,eta.hat.glm,col=4,lwd=2)

## doing this all for all the years
t <- unique(my.df$Year)
n <- length(t)

## object to store our results
COEFS <- matrix(NA,n,3)

i <- 1
for (i in 1:n){
  ## extracting the data of interest
  y <- my.df %>% filter(Year==t[i]) %>% 
    select(Births) %>% pull()
  e <- my.df %>% filter(Year==t[i]) %>% 
    select(Exposures) %>% pull()
  ## fitting the GLM
  mod.glm <- glm(y~x+x.sq,family = poisson(),
                 offset=log(e))
  ## save the coefficients
  COEFS[i,] <- coef(mod.glm) 
  
}

matplot(t,COEFS,t="p",pch=1:3)

## create a dataframe for plotting purposes
df.plot <- tibble(Year=t,Intercept=COEFS[,1],
                  Linear=COEFS[,2],Quadratic=COEFS[,3])

df.plot.long <- df.plot %>% 
  pivot_longer(-Year)

## plotting
df.plot.long %>% 
  ggplot(aes(x=Year,y=value))+
  geom_line()+
  facet_wrap(.~name,scales = "free_y")

## forecasting the time-series of the parameters
tF <- (t[n]+1):2050
nF <- length(tF)

## extract the time-series of interest
y1 <- COEFS[,1]
y2 <- COEFS[,2]
y3 <- COEFS[,3]
plot(t,y3)
acf(y3)

## fit the best ARIMA model using auto.arima
mod1 <- auto.arima(y1)
mod2 <- auto.arima(y2)
mod3 <- auto.arima(y3)
summary(mod1)

## forecasting the time series
y1.fore <- forecast(mod1,h=nF)
y2.fore <- forecast(mod2,h=nF)
y3.fore <- forecast(mod3,h=nF)
plot(y1.fore)
plot(y2.fore)
plot(y3.fore)


## create a matrix where to store our forecast
ETA.fore <- matrix(NA,m,nF)

i <- 1
for (i in 1:nF){
  eta.fore <- y1.fore$mean[i] + y2.fore$mean[i]*x +
    y3.fore$mean[i]*x.sq 
  ETA.fore[,i] <- eta.fore
}



LRATES <- matrix(my.df$logRates,m,n)

## plotting
my.cols <- viridis(n+nF)
matplot(x,LRATES,col = my.cols[1:n],lty=1,t="l",
        ylim=range(LRATES,ETA.fore,finite=T))
matlines(x,ETA.fore,col = my.cols[1:nF+n],lty=1)

## computing TFR
tfr.obs <- apply(exp(LRATES),2,sum)
tfr.fore <- apply(exp(ETA.fore),2,sum)
plot(t,tfr.obs,ylim=range(tfr.obs,tfr.fore,0),
     xlim=range(t,tF))
lines(tF,tfr.fore,col=2,lwd=2)
abline(h=0)

##---- EXERCISE 4

## fitting a single GLM
mod.glm2 <- glm(Births~Age+I(Age^2)+Year,
                data = my.df,
                family = poisson(),
                offset=log(Exposures))
coef(mod.glm2)

## extract the age and time patterns
age.pattern <- coef(mod.glm2)[2]*x +
  coef(mod.glm2)[3]*x.sq
plot(x,age.pattern)

time.pattern <- coef(mod.glm2)[4]*t
plot(t,time.pattern)


## forecasting
time.patternF <- coef(mod.glm2)[4]*tF
plot(t,time.pattern,ylim=range(time.pattern,time.patternF),
     xlim=range(t,tF))
points(tF,time.patternF,pch=16)

## create a matrix where to store our forecast
ETA.fore2 <- matrix(NA,m,nF)
i <- 1
for (i in 1:nF){
  eta.fore <- coef(mod.glm2)[1] + age.pattern +
    time.patternF[i] 
  ETA.fore2[,i] <- eta.fore
}


## plotting
par(mfrow=c(1,2))
matplot(x,LRATES,col = my.cols[1:n],lty=1,t="l",
        ylim=range(LRATES,ETA.fore,finite=T))
matlines(x,ETA.fore,col = my.cols[1:nF+n],lty=1)

matplot(x,LRATES,col = my.cols[1:n],lty=1,t="l",
        ylim=range(LRATES,ETA.fore,finite=T))
matlines(x,ETA.fore2,col = my.cols[1:nF+n],lty=1)
par(mfrow=c(1,1))

tfr.fore2 <- apply(exp(ETA.fore2),2,sum)
plot(t,tfr.obs,ylim=range(tfr.obs,tfr.fore,0),
     xlim=range(t,tF))
lines(tF,tfr.fore,col=2,lwd=2)
lines(tF,tfr.fore2,col=4,lwd=2)
abline(h=0)










