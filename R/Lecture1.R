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
