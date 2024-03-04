## -------------------------------------------------- ##
## EDSD 2023-2024: Demographic Forecasting
## Lecture 1
## Direct extrapolation by (generalized) linear models
## Date: 02/05/2023
## Instructor: Ugofilippo Basellini
## -------------------------------------------------- ##

## cleaning the workspace
rm(list=ls(all=TRUE))

## loading useful packages
library(tidyverse)

## loading data
load("data/FertSWE.Rdata")

## start by inspecting our data
head(FERT.SWE)
range(FERT.SWE$Year)
range(FERT.SWE$Age)

## subset of my data
my.df <- FERT.SWE %>% filter(Age==20, Year >= 1950)
range(my.df$Year)
range(my.df$Age)

## plotting the data
my.df %>% ggplot(aes(x=Year,y=Births))+
  geom_point()

## fitting a linear model
mod1 <- lm(Births~Year,data = my.df)
summary(mod1)

## extract the data of interest
y <- my.df$Births
t <- my.df$Year
plot(t,y)
mod2 <- lm(y~t)
summary(mod2)

## extraplotaing to the future - way 1
tF <- t[1]:2050
y.fore <- coef(mod1)[1] + coef(mod1)[2]*tF

## extraplotaing to the future - way 1
df.fore <- tibble(Year=tF)
y.fore2 <- predict(mod1,newdata = df.fore)
all.equal(y.fore,unname(y.fore2))

## plotting the forecast
plot(t,y,ylim=range(y,y.fore),xlim=range(tF))
lines(tF,y.fore,col=2,lwd=2)
lines(tF,y.fore2,col=4,lwd=2,lty=2)
abline(h=0)

## ---- EXERCISE 2 ---

## fitting a linear model to the rates
mod3 <- lm(Rates~Year,data = my.df)
summary(mod3)

## forecast of the linear model
fx.fore.lm <- coef(mod3)[1] + coef(mod3)[2]*tF

## plotting the forecast
plot(t,my.df$Rates,ylim=range(my.df$Rates,fx.fore.lm),
     xlim=range(tF))
lines(tF,fx.fore.lm,col=2,lwd=2)
# lines(tF,y.fore2,col=4,lwd=2,lty=2)
abline(h=0)

## moving now to a GLM
mod.glm <- glm(Births~Year,data = my.df,
               family = poisson(),offset=log(Exposures))
summary(mod.glm)

## forecast of the GLM
eta.fore.glm <- coef(mod.glm)[1] + coef(mod.glm)[2]*tF

## plotting the forecast (log scale)
plot(t,my.df$logRates,ylim=range(my.df$logRates,eta.fore.glm),
     xlim=range(tF))
lines(tF,log(fx.fore.lm),col=2,lwd=2)
lines(tF,eta.fore.glm,col=4,lwd=2)


plot(t,my.df$Rates,
     ylim=range(my.df$Rates,fx.fore.lm,exp(eta.fore.glm)),
     xlim=range(tF))
lines(tF,fx.fore.lm,col=2,lwd=2)
lines(tF,exp(eta.fore.glm),col=4,lwd=2)
abline(h=0)


## linear combination of betas
X <- cbind(1,tF)
eta.hat <- c(X%*%coef(mod.glm))
all.equal(eta.hat,eta.fore.glm)
## var-covar matrix
V <- vcov(mod.glm)
## computing standard errors
se.eta <- sqrt(diag(X%*%V%*%t(X)))
## upper and lower 95% CI
eta.hat.low <- eta.hat - 2*se.eta
eta.hat.up <- eta.hat + 2*se.eta
## plotting
plot(my.df$Year,my.df$logRates,ylim=range(my.df$logRates,
                                          eta.hat.low,eta.hat.up),xlim=range(tF))
lines(tF,eta.hat,col=2,lwd=2)
lines(tF,eta.hat.low,col=4,lwd=2,lty=2)
lines(tF,eta.hat.up,col=4,lwd=2,lty=2)














