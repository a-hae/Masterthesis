# merge_slides.R
#-------------------------------------------------------------------------------
# Tasks;
# - merge all features in shapefile which are connected

###-----------------------------------------------------------------------------
setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla/Data")

## load packages ---------------------------------------------------------------

library(RSAGA)
library(rgdal)
library(rgeos)
library(dplyr)
library(raster)

## RSAGA SETTINGS --------------------------------------------------------------

env <- rsaga.env()
 