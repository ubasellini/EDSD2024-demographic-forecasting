## -------------------------------------------------- ##
## EDSD 2023-2024: Demographic Forecasting
## Lecture 2
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
my.df <- FERT.SWE %>% filter(Age==20, Year >= 1950)
