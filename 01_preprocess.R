
#-------------------------------------------------------------------------------
# Tasks;
# - classify above and below treeline
# - merge all features in shapefile which are connected
# - classify: define if feature are connected to river or not (column: connected)

###-----------------------------------------------------------------------------


## load packages ---------------------------------------------------------------

#library(RSAGA)
library(rgdal)
library(rgeos)
#library(dplyr)
library(raster)

## load data--------------------------------------------------------------------

setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla")

dtm <- raster("stolla_dtm5m.tif")

dbflows_ply <- readOGR(".","DF_Stolla_all")

source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/My_Projekt/Masterthesis/preprocess_fun.R")


## classify runout polygons within treeline-------------------------------------

dbflows_classtree <- classTreeline(dtm, dbflows_ply, treeline = 2100)

dbflows_tree <- dbflows_classtree[dbflows_classtree@data$below_treeline == "Yes", "below_treeline"]

plot(dbflows_tree)



