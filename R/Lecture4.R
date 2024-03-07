## -------------------------------------------------- ##
## EDSD 2023-2024: Demographic Forecasting
## Lecture 4
## Forecasting by Lee-Carter method
## Date: 07/03/2024
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
load("data/MORTSWE.Rdata")

## subset of my data
my.df <- MORT.SWE %>% 
  filter(Year>= 1950, Sex=="Male", Age <= 100)

## extracting the quantity of interest
x <- unique(my.df$Age)
t <- unique(my.df$Year)
m <- length(x)
n <- length(t)

## extract the rates
DEATHS <- matrix(my.df$Deaths,m,n)
EXPOS <- matrix(my.df$Exposures,m,n)
LRATES <- matrix(my.df$logRates,m,n)
matplot(x,LRATES,t="l",lty=1,col=viridis(n))

which(DEATHS==0,arr.ind = T)

my.df %>% 
  ggplot(aes(x=Year,y=Age,z=logRates))+
  geom_contour_filled()

## adjusting the log-rates (no -infty)
my.df2 <- my.df %>% 
  mutate(Deaths1=case_when(
    Deaths==0 ~ 1,
    TRUE      ~ Deaths),
    logRates1=log(Deaths1/Exposures))
my.df2 %>% 
  ggplot(aes(x=Year,y=Age,z=logRates1))+
  geom_contour_filled()
## computing Alpha
df.Alpha <- my.df2 %>% 
  group_by(Age) %>% 
  summarise(Alpha=mean(logRates1))
## plotting
df.Alpha %>% 
  ggplot(aes(x=Age))+
  geom_line(aes(y=logRates,group=Year),
            data=my.df,color="grey80")+
  geom_line(aes(y=Alpha),linewidth=1,color="darkgreen")+
    theme_bw()

## computing beta and kappa
LRATES1 <- matrix(my.df2$logRates1,m,n)
matplot(x,LRATES1,t="l",lty=1,col=viridis(n))

## compute the matrix of centred log-rates
Alpha <- df.Alpha$Alpha
LMX.centred <- LRATES1 - Alpha
matplot(x,LMX.centred,t="l",lty=1,col=viridis(n))

my.df2 %>% 
  mutate(lmx.centred=c(LMX.centred)) %>% 
  ggplot(aes(x=Year,y=Age,z=lmx.centred))+
  geom_contour_filled()

## perform the SVD
LCsvd <- svd(LMX.centred,nu=1,nv=1)

Beta <- c(LCsvd$u)
Kappa1 <- c(LCsvd$v)
sum(Beta)
sum(Kappa1)


plot(t,Kappa1)
plot(x,Beta)

LMX.centred.appox <- LCsvd$d[1]*Beta%*%t(Kappa1)

my.df2 %>% 
  select(Age,Year) %>% 
  mutate(lmx.centred=c(LMX.centred),
         lmx.approx=c(LMX.centred.appox)) %>%
  pivot_longer(-c(Year,Age)) %>% 
  ggplot(aes(x=Year,y=Age,z=value))+
  geom_contour_filled()+
  facet_wrap(.~name)

## including the constraints
## constraint 1
sum.Beta <- sum(Beta)
Beta <- Beta/sum.Beta
sum(Beta)
## constraint 2
Kappa1 <- LCsvd$d[1]*Kappa1*sum.Beta
sum(Kappa1)
## plotting
par(mfrow=c(1,2))
plot(x,Beta)
plot(t,Kappa1)
par(mfrow=c(1,1))


##---- step 3: adjust KAPPA -----
## function to compute difference between observed and fitted LC deaths
koptim <- function(par,alpha,beta,sum.dx,Exp){
  kappa <- par[1]
  lmx.lc <- alpha+beta*kappa
  z.lc <- exp(lmx.lc)*Exp
  sum.z.lc <- sum(z.lc)
  diff.lc <- abs(sum.dx-sum.z.lc)
  return(diff.lc)
}
## adjust Kappa every year
Kappa <- numeric(n)
for (i in 1:n){
  KappaSecStep <- optimize(f=koptim,interval=c(-100,100),alpha=Alpha,
                           beta=Beta,sum.dx=sum(DEATHS[,i]),Exp=EXPOS[,i])
  Kappa[i] <- KappaSecStep$minimum
}
## plotting
plot(t,Kappa1,ylim=range(Kappa1,Kappa),t="l",lwd=2)
lines(t,Kappa,col=4,lwd=2)
legend("topright",c("from SVD","second-step adjustment"),col=c(1,4),lwd=2)


## fitted log-mortality
Ones <- matrix(1,n)
ETAlc <- Alpha%*%t(Ones) + Beta%*%t(Kappa)
## basic plot
g <- my.df %>%
  mutate(logRates=case_when(
    is.infinite(logRates)~NA,
    TRUE~logRates),
    Fitted=c(ETAlc)) %>%
  ggplot(aes(x=Age,group=Year))+
  geom_point(aes(y=logRates))+
  geom_line(aes(y=Fitted),color="darkorange",linewidth=1.2)+
  scale_color_viridis_c() +
  theme_bw(base_size = 18) +
  labs(y= "Log Mortality Rate")
## animating with gganimate
library(gganimate)
gg <- g + transition_time(Year) +
  ggtitle("Year {frame_time}")
animate(gg, fps=4)









