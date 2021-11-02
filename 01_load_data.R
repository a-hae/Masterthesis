# 01_slides_group.R
#-------------------------------------------------------------------------------
# Tasks:
# - create saga enviroment
# - add polygon attributes of lithology and landuse to each point in 
#   event source shapefile
# - load new point sapefile 
# - group eventpoints after lithology groups and landuse groups
# - create randoms sample of points fpr each group

###-----------------------------------------------------------------------------

setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/upper_maipo_river")

## load packages ---------------------------------------------------------------

library(RSAGA)
library(rgdal)
library(dplyr)

# RSAGA SETTINGS ---------------------------------------------------------------

env <- rsaga.env()

set.seed(10)

## landslides in groups (landuse and lithology)---------------------------------
## and create random sample of each group --------------------------------------

# add polygon attributes from landuse and lithology to each point
rsaga.geoprocessor(lib="shapes_points", module = 10, env = env, show.output.on.console = TRUE, param = list(
  INPUT = "debris_flow_source_points.shp", OUTPUT = "debris_flow_source_points_poly_attr", POLYGONS = "lithology_cetaquaprj.shp", FIELDS = "Nombre"))

rsaga.geoprocessor(lib="shapes_points", module = 10, env = env, show.output.on.console = TRUE, param = list(
  INPUT = "debris_flow_source_points_poly_attr.shp", OUTPUT = "debris_flow_source_points_poly_attr", POLYGONS = "landuse_uso_suelo.shp", FIELDS = "Sub_uso"))


# name of  new debris flow source point shapefile
nm_src_pts_attr <- "debris_flow_source_points_poly_attr"

# load source points shpaefile
src_pts_attr <- readOGR(".", nm_src_pts_attr)

# convert it in normal dataframe
src_pts_attr_df <- as.data.frame(src_pts_attr)

# Count landslide in each group
litho_groups <- src_pts_attr_df %>% group_by(Nombre) %>% count()

landuse_groups <- src_pts_attr_df %>% group_by(Sub_uso) %>% count()

# create new dataframes for each group
for (k in litho_groups[[1]]) {
  # lfilter landslides
  group_filtered <- src_pts_attr_df %>% filter( Nombre == k)
  # if filtered group larger than 10 landslide, select 10 randomly
  if (count(group_filtered) >= 10) {
    #select due random sample 10 landslides 
    group_filtered <- group_filtered[sample(nrow(group_filtered), 10), ]
  }
  # create new variable name with group name
  assign(paste0("group_", k),group_filtered )
  
  }

for (l in landuse_groups[[1]]) {
  # lfilter landslides
  group_filtered <- src_pts_attr_df %>% filter( Sub_uso == l) 
  # if filtered group larger than 10 landslide, select 10 randomly
  if (count(group_filtered) > 10) {
    #select due random sample 10 landslides 
    group_filtered <- group_filtered[sample(nrow(group_filtered), 10), ]
    }
  # create new variable name with group name
  assign(paste0("group_", l), group_filtered)
  
}

for (k in landuse_groups[[1]]) {
  
  for (l in 1:length(src_pts_attr)) {
    if ( k == src_pts_attr$Sub_uso[[l]]) {
      print(src_pts_attr$OBJECTID_1[[l]])
      
      new_sp_df <- src_pts_attr[src_pts_attr$OBJECTID_1 == 10]
                                
    }
  }
  
}

coordinates(`group_Formaci_n Abanico`) <- `group_Formaci_n Abanico`[20:21]


# export shapefiles 
nm_output <- "source_points_group_Native"
writeOGR(`group_Formaci_n Abanico`, dsn = "grouped_source_points_", nm_output, driver="ESRI Shapefile")
