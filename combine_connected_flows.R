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
library(MazamaSpatialUtils)
## RSAGA SETTINGS --------------------------------------------------------------

#load data
slides <- readOGR(".","DF_Stolla_all")

slides_combined <- rmapshaper::ms_dissolve(slides)

slides_split <- disaggregate(slides_combined)

writeOGR(`slides_split`, dsn = "merged_slides", "merged_slides", driver="ESRI Shapefile", overwrite_layer =  TRUE)
