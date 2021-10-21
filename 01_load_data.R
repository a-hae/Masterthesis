setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/upper_maipo_river")

## load packages ---------------------------------------------------------------

library(RSAGA)
library(rgdal)
library(spdplyr)

# RSAGA SETTINGS ---------------------------------------------------------------

env <- rsaga.env()


##load data --------------------------------------------------------------------

# name of landuse shapefile
nm_landusepoly <- "landuse_uso_suelo"

# name of lithology shapefile
nm_lithopoly <- "lithology_cetaquaprj"

# load landuse polygon
landuse_poly <- readOGR(".", nm_landusepoly)

# load lithology polygon
litho_poly <- readOGR(".", nm_lithopoly)

# name of debris flow source point shapefile
nm_src_pts <- "debris_flow_source_points"


## landslides in group (landuse and lithology)----------------------------------

# add polygon attributes from landuse and lithology to each point
rsaga.geoprocessor(lib="shapes_points", module = 10, env = env, show.output.on.console = TRUE, param = list(
  INPUT = "debris_flow_source_points.shp", OUTPUT = "debris_flow_source_points_poly_attr", POLYGONS = "lithology_cetaquaprj.shp", FIELDS = "Nombre"))

rsaga.geoprocessor(lib="shapes_points", module = 10, env = env, show.output.on.console = TRUE, param = list(
  INPUT = "debris_flow_source_points_poly_attr.shp", OUTPUT = "debris_flow_source_points_poly_attr", POLYGONS = "landuse_uso_suelo.shp", FIELDS = "Sub_uso"))

# 

# name of  new debris flow source point shapefile
nm_src_pts_attr <- "debris_flow_source_points_poly_attr"

# load source points shpaefile
src_pts_attr <- readOGR(".", nm_src_pts_attr)

# convert it in normal dataframe
src_pts_attr_df <- as.data.frame(src_pts_attr)

# Count landslide in each group
src_pts_attr_df %>% group_by(Nombre) %>% count()

src_pts_attr_df %>% group_by(Sub_uso) %>% count()






