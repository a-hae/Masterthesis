library(Rsagacmd)


saga <- saga_gis( opt_lib = "sim_geomorphology")

## load data--------------------------------------------------------------------
setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla")

# Read Polygons using RGDAL package
setwd("edit_files")
runout_polygons <- readOGR(".", "DF_Stolla_merged_classified")
setwd("../")

dtm <- raster("stolla_dtm5m.tif")
hs <- raster("stolla_hs_dtm5m.tif")

setwd("DSM")
dsm_raw  <- flip(raster("2010_1m_avg_raster2.tif"), direction = "y")




