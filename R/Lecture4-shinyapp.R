## -------------------------------------------------- ##
## EDSD 2023-2024: Demographic Forecasting
## Lecture 4 - shiny example
## Forecasting by Lee-Carter method
## Date: 07/03/2024
## Instructor: Ugofilippo Basellini
## -------------------------------------------------- ##


##---- SHINY APP EXAMPLE ----

## cleaning the workspace
rm(list=ls(all=TRUE))
## load useful packages
library(tidyverse)
library(patchwork)
library(shiny)
## loading data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("data/MORTSWE.Rdata")
## subset
my.df <- MORT.SWE %>% filter(Sex=="Male", Age <= 100)

## build your user interface
ui <- fluidPage(
  ## title of your shiny
  titlePanel("LC parameters"),
  ## display a slider that returns input$year to pass to the server function
  sliderInput(inputId = "year", label = "Year", step = 1,
              value = 1950, min = 1950, max = 2022),
  ## display a plot returned from the server
  plotOutput("plot_pars")
)

## build your server
server <- function(input, output){
  ## create an output that renders a plot
  output$plot_pars <- renderPlot({
    ## subsetting data based on given input (input$year)
    my.df <- my.df %>% filter(Year>=input$year)
    ## extract data
    x <- unique(my.df$Age)
    t <- unique(my.df$Year)
    n <- length(t)
    m <- length(x)
    ## matrices
    DEATHS <- matrix(my.df$Deaths,m,n)
    EXPOS <- matrix(my.df$Exposures,m,n)
    ##---- step 1: derive ALPHA -----
    ## adjust deaths and log-rates
    DEATHS1 <- DEATHS
    DEATHS1[DEATHS==0] <- min(DEATHS[DEATHS!=0])
    LRATES1 <- log(DEATHS1/EXPOS)
    ## compute the mean of observed rates
    Alpha <- apply(LRATES1,1,mean)
    ##---- step 2: derive BETA and KAPPA -----
    ## derive matrix of residuals ("centred" mortality rates)
    LMXres <- LRATES1 - Alpha
    ## performing the SVD
    LCsvd <- svd(LMXres,nu=1,nv=1)
    ## extract first left- and right-singular vectors of svd
    Beta <- c(LCsvd$u)
    Kappa1 <- c(LCsvd$v)
    ## including the constraints
    ## constraint 1
    sum.Beta <- sum(Beta)
    Beta <- Beta/sum.Beta
    ## constraint 2
    Kappa1 <- LCsvd$d[1]*Kappa1*sum.Beta
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
      KappaSecStep <- optimize(f=koptim,interval=c(-100,100),alpha=Alpha,beta=Beta,sum.dx=sum(DEATHS[,i]),
                               Exp=EXPOS[,i])
      Kappa[i] <- KappaSecStep$minimum
    }
    ## plot of parameters
    df.plot.1 <- tibble(Age=x,Alpha=Alpha,Beta=Beta)
    df.plot.2 <- tibble(Year=t,Kappa=Kappa)
    
    f1 <- df.plot.1 %>% pivot_longer(-Age) %>%
      ggplot(aes(x=Age,y=value,color=name)) +
      geom_line()+
      facet_wrap(.~name,scales="free_y")
    f2 <- df.plot.2 %>%
      ggplot(aes(x=Year,y=Kappa)) +
      geom_line()
    
    ## combining plots
    f1+f2
  })
}

## run the shiny app, which puts together the ui and server
shinyApp(ui = ui, server = server)