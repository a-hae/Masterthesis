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
# crown heigth raster (classified with classify.R ]0,50[)
chm <- raster("stolla_crown_hgt_reclass_v9.tif")
chm@crs@projargs <- proj4string(dem)


## Filtering -------------------------------------------------------------------
##------------------------------------------------------------------------------
# use only slides for all threshold: have crwon heigth values 

# get crown heigtht for each slide (based on observed polygon)
# RUN IT ONLY ONCE! (need long)
runout_polygons_chm <- ch_per_slide(runout_polygons, chm)
#empty column for the threshold with existing class sides
runout_polygons_chm$filter <- NA

for (i in 1:length(runout_polygons)) {
  class_sgl <- runout_polygons_chm$chm[[i]]
  ls_thres <- NULL
  for (t in 1: length(thres_grid)) {
    if (any(class_sgl < thres_grid[t],  na.rm = TRUE) && any(class_sgl >= thres_grid[t], na.rm = TRUE)) {
      ls_thres <- append(ls_thres, thres_grid[t])
    }
  }
  runout_polygons_chm$filter[i] <- list(ls_thres) 
  
}

# choose only slide which have all thresolds frrom grid
runout_polygons$sample <- "No"
for (i in 1:length(runout_polygons)) {
  if (isTRUE(all.equal(thres_grid, runout_polygons_chm$filter[[i]]))) {
    runout_polygons$sample[i] <- "Yes"
  }
  
}

runout_polygons_sample <- runout_polygons[runout_polygons$sample == "Yes", ]
sample_ids <- runout_polygons_sample$Id