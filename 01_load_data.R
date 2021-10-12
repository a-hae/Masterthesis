setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/upper_maipo_river")

### load data

# FUNCTIONS ###################################################################
geoTiffToSGRD_custom <- function(file_name, write_name){
  #Convert a tif to a SAGA format
  require(rgdal)
  setwd("../")
  spgdf <- readGDAL(file_name)
  setwd("saga_data")
  writeGDAL(spgdf, write_name, drivername = "SAGA")
}






