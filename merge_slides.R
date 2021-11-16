# merge_slides.R
#-------------------------------------------------------------------------------
# Tasks;
# - merge all features in shapefile which are connected

###-----------------------------------------------------------------------------
setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla/Data")

## load packages ---------------------------------------------------------------

library(RSAGA)
library(rgdal)
library(rgeos)
library(dplyr)
library(raster)

## RSAGA SETTINGS --------------------------------------------------------------

env <- rsaga.env()

# Functions --------------------------------------------------------------------
FindConnected <- function(slidesSHP, func = "touch") {
  # add new attribute: aggregate
  slidesSHP$aggregate = 0
  # define index of aggregate column
  agg_idx <- which(colnames(slidesSHP@data)=="aggregate")
  
  list = 1:900
  z = 1
  
  for (s in 1:length(slidesSHP)) {
    for  (k in 1:length(slidesSHP)){
      if (slidesSHP[s,]$Id != slidesSHP[k,]$Id)  {
        # check for same boundary of two features
        if (func == "touch") {
          touch <- gTouches(slidesSHP[s,], slidesSHP[k,])
        }
        else if (func == "overlaps") {
          touch <- gOverlaps(slidesSHP[s,], slidesSHP[k,])
        }
        
        if (touch == TRUE) {
          # give pair of features one value
          # if number already their, take it
          if (slidesSHP[s,]$aggregate == 0 && slidesSHP[k,]$aggregate == 0){
            # else new number
            slidesSHP[s,agg_idx] = list[z]
            slidesSHP[k,agg_idx] = list[z]
            z = z+1
          } else if (slidesSHP[s,]$aggregate == 0 && slidesSHP[k,]$aggregate != 0) {
            slidesSHP[s,agg_idx] = slidesSHP[k,]$aggregate
            
          } else if (slidesSHP[s,]$aggregate != 0 && slidesSHP[k,]$aggregate == 0) {
            slidesSHP[k,agg_idx] = slidesSHP[s,]$aggregate
            
          }
        }  
      } 
      
    }
  }
  # change all zero in aggeggrate column in new unique values
  for (m in 1:length(slidesSHP)) {
    if (slidesSHP[m,]$aggregate == 0) {
      slidesSHP[m,agg_idx] = list[z]
      z = z+1
    }
  }
  return(slidesSHP)
}
  

### aggregate connected features -----------------------------------------------

#load data
slides <- readOGR(".","DF_Stolla_all_new")
# identify and define connected slides
slides_con <- FindConnected(slides)

# merge all slides with same number
slides_merged <- aggregate(slides_con, by = "aggregate")

# create new ID column
slides_merged$Id <- 0
# define index of id column
id_idx <- which(colnames(slides_merged@data)=="Id")

for (n in 1:length(slides_merged)) {
  slides_merged[n,id_idx] = n
}

# repeat merging until nothing change
slides1 <- slides
slides_merged1 <- slides_merged

while (length(slides1) > length(slides_merged1)) {
  slides1 = slides_merged
  slides_merged <- FindConnected(slides1)
}

# optimize connection of features with gOverlaps function
slides_merged_fin <- FindConnected(slides_merged, "overlaps")

# export merged slides file
writeOGR(`slides_merged_fin`, dsn = "merged_slides", "merged_slides", driver="ESRI Shapefile", overwrite_layer =  TRUE)

 