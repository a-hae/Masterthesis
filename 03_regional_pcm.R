library(runoptGPP)
library(raster)
library(rgdal)    
library(Rsagacmd)

saga <- saga_gis(opt_lib = "sim_geomorphology")

source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/func/custom_runoptGPP_fun.R")
source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/func/pcm_conn_fun.R")

# load data---------------------------------------------------------------------
setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla/edit_files")

# runout and source cells polygon ----------------------------------------------

runout_polygons <- readOGR(".", "DF_Stolla_merged_classified")
src_poly <- readOGR(".", "src_poly_5per")

setwd("../")
river <- readOGR(".", "Stolla_channel")

dem <- raster("stolla_fillsinks_dtm5m.tif")

## Get optimal parameter of random walk-----------------------------------------

setwd("results")

(load("rw_gridsearch_n15_seed10.Rd"))

# Get RW optimal parameter set
rw_opt <- rwGetOpt(rw_gridsearch_multi, measure = median)
#save(rw_opt, file = "rw_opt_params_n100_all.Rd")




