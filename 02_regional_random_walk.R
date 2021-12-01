library(runoptGPP)
library(raster)
library(rgdal)
library(sp)
library(Rsagacmd)

## Load data --------------------------------------------------------------------

setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla")

# Load digital elevation model (DEM)
dem <- raster("stolla_dtm5m.tif")

# Load runout source points
#source_cells <- raster("src_cells_low10per.tif")

setwd("edit_files")
runout_polygons <- readOGR(".", "DF_Stolla_merged_classified")
runout_polygons$objectid <- 1:length(runout_polygons)

src_poly <- readOGR(".", "src_poly_5per")
setwd("../")

# change crs of src_poly
src_poly <- spTransform(src_poly, runout_polygons@proj4string)

#Initate saga gis
saga <- saga_gis(opt_lib = "sim_geomorphology")

### create sample data ---------------------------------------------------------

set.seed(10)
runout_polygons_sample <- sample(src_poly$rnt_ply, 15)

sample <- runout_polygons[runout_polygons_sample,]

writeOGR(`sample`, dsn = "edit_files", "DF_Stolla_merged_sample", driver="ESRI Shapefile", overwrite_layer =  TRUE)
