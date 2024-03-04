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

## loading data
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
load("data/TimeSeries.Rdata")

## start by inspecting our data
head(sp500.data)

