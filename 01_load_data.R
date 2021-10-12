setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/upper_maipo_river")


# RSAGA SETTINGS ###############################################################

library(RSAGA)

myenv <- rsaga.env()

# load lithogy and landuse data ################################################
library(rgdal)

#name of landuse shapefile
nm_landusepoly <- "landuse_uso_suelo"

#name of lithology shapefile
nm_lithopoly <- "lithology_cetaquaprj"

# load landuse polygon
landuse_poly_spain <- readOGR(".", nm_landusepoly)

# load lithology polygon
litho_poly_spain <- readOGR(".", nm_lithopoly)







