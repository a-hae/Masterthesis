library(Rsagacmd)
library(rgdal)  
library(raster)
library(terra)

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

## create dsm and topography maps ----------------------------------------------

dsm <- terra::resample(dsm_raw, dtm)
dsm <- mask(dsm, dtm)

# slope and aspect
slope <- raster::terrain(dsm, opt='slope')
aspect <- raster::terrain(dsm, opt='aspect')

# create hillshade
# numbers
hs <- raster::hillShade(slope, aspect,
                        angle=40,
                        direction=270)

plot(hs, col=grey(0:100/100), legend = FALSE, main = "Stolla Basin")
plot(dsm, add = TRUE, alpha = 0.4)
plot(runout_polygons, add = TRUE)

