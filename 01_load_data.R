setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/upper_maipo_river")

## load packages ---------------------------------------------------------------

library(RSAGA)
library(rgdal)

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

# load source points shpaefile
src_pts <- readOGR(".", nm_src_pts)

## landslides in group (landuse and lithology)----------------------------------

# add polygon attributes from landuse and lithology to each point
rsaga.geoprocessor(lib="shapes_points", module = 10, env = env, show.output.on.console = TRUE, param = list(
  INPUT = "debris_flow_source_points.shp", OUTPUT = "debris_flow_source_points_poly_attr", POLYGONS = "lithology_cetaquaprj.shp", FIELDS = "Nombre"))

rsaga.geoprocessor(lib="shapes_points", module = 10, env = env, show.output.on.console = TRUE, param = list(
  INPUT = "debris_flow_source_points_poly_attr.shp", OUTPUT = "debris_flow_source_points_poly_attr", POLYGONS = "landuse_uso_suelo.shp", FIELDS = "Sub_uso"))





