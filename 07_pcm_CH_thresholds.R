# Spatial PCM with crown height
# - Filterung after slides with crown height
# - change crown height raster for different thresholds between nonforested
#  and forested areas
# - spatial pcm

####----------------------------------------------------------------------------

library(runoptGPP)
library(raster)
library(rgdal)    
library(Rsagacmd)
library(dplyr)

# set saga enviroment
saga <- saga_gis(opt_lib = "sim_geomorphology",
                 temp_path = "C:/SpatialAnalyst_Temp")
# saga <- saga_gis(saga_bin = "D:/Users/te59fex/Downloads/saga/saga/saga_cmd.exe",
#                  opt_lib = "sim_geomorphology",
#                  temp_path = "D:/Users/te59fex/Documents/SpatialAnalyst_Temp")

# load functions
source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/My_Projekt/Masterthesis/pcm_fun.R")
source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/My_Projekt/Masterthesis/pcm_chm_fun.R")

#source("D:/Users/te59fex/OneDrive/Master/Masterarbeit/RProjekt/My_Projekt/Masterthesis/pcm_fun.R")

## load data---------------------------------------------------------------------
setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla/edit_files")

#setwd("D:/Users/te59fex/OneDrive/Master/Masterarbeit/Data/Stolla/edit_files")

# runout and source cells polygon 
runout_polygons <- readOGR(".", "DF_Stolla_merged_classified")
src_poly <- readOGR(".", "src_poly_5per")

setwd("../")
river <- readOGR(".", "Stolla_channel")

dem <- raster("stolla_fillsinks_dtm5m.tif")

river@proj4string@projargs <- proj4string(dem)
runout_polygons@proj4string@projargs <- proj4string(dem)
# crown heigth raster
chm <- raster("stolla_crown_hgt_reclass_v9.tif")
chm@crs@projargs <- proj4string(dem)
