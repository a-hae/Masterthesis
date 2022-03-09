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


# create crown height ----------------------------------------------------------

crown_hgt <- dsm - dtm

plot(hs, col=grey(0:100/100), legend = FALSE, main = "Stolla Basin")
plot(crown_hgt, add = TRUE, alpha = 0.3)
plot(runout_polygons, add = TRUE)

setwd("../")
writeRaster(crown_hgt, filename = "stolla_crown_hgt_2010.tif", format = "GTiff")

writeRaster(dsm, filename = "stolla_dsm_2010_5m.tif", format = "GTiff", overwrite = TRUE)

# Clean with land use map-------------------------------------------------------

# Load land use data
landuse_polygons <- readOGR(".", "landuse_Stolla")

# Create a grid for land_use
landuse <- terra::rasterize(landuse_polygons, dsm, field = "CODE_1")

# Create a forest grid
mask <- landuse
mask[mask == 1] <- 0
mask[mask == 2] <- 1
mask[mask == 3] <- 1
mask[mask == 4] <- 1
mask[mask == 5] <- 0
mask[mask == 0] <- NA

plot(mask)

mask_crown_hgt <- mask(crown_hgt, mask)
writeRaster(mask_crown_hgt, filename = "mask_crown_height.tif", format = "GTiff")
mask_crown_hgt[is.na(mask_crown_hgt)] = 0
mask_crown_hgt <- mask(mask_crown_hgt, dsm)
writeRaster(mask_crown_hgt, filename = "mask_crown_height_dsm.tif", format = "GTiff")
plot(mask_crown_hgt)

mask_crown_hgt


## classsify crown heigth ------------------------------------------------------


class.m <- c(-Inf,0,NaN,
             0, 1, 1,
             1, 2.5, 2.5,
             2.5, 5, 5,  
             5 , 10, 10,
             10, 30, 30,
             30, Inf, NaN)

# reshape the object into a matrix with columns and rows
rcl.m <- matrix(class.m, 
                ncol=3, 
                byrow=TRUE)

# reclassify the raster using the reclass object - rcl.m
asp.ns <- reclassify(mask_crown_hgt, 
                     rcl.m)


# plot
plot(asp.ns)
title("Crown height [m]")
# allow legend to plot outside of bounds
par(xpd=TRUE)


# export geotiff
writeRaster(asp.ns,
            filename="stolla_crown_hgt_reclass.tif",
            format="GTiff",
            options="COMPRESS=LZW",
            overwrite = TRUE,
            NAflag = -9999)

