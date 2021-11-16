
#-------------------------------------------------------------------------------
# Tasks;
# - 

###-----------------------------------------------------------------------------
setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla/Data/merged_slides")

## load packages ---------------------------------------------------------------

#library(RSAGA)
#library(rgdal)
#library(rgeos)
#library(dplyr)
#library(raster)


##load data---------------------------------------------------------------------
slides <- readOGR(".","merged_slides")
setwd("../")

river <- readOGR(".","Stolla_channel")


### check for connection to river-----------------------------------------------

# add new attribute: aggregate
slides$connected = "No"
# define index of connected column
con_idx <- which(colnames(slides@data) =="connected")

# add "yes" to all feature that are conected to river feature
for (s in 1:length(slides)) {
  overlap <- gOverlaps(slides[s,], river)
  touch <- gTouches(slides[s,], river)
  if (touch == TRUE || overlap == TRUE){
    slides[s,con_idx] <- "Yes"
  }
}

#export shapefile
writeOGR(`slides`, dsn = "merged_slides", "merged_slides", driver="ESRI Shapefile", overwrite_layer =  TRUE)


###-----------------------------------------------------------------------------


