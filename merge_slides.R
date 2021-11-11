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
FindConnected <- function(slidesSHP) {
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
        #touch <- gTouches(slidesSHP[s,], slidesSHP[k,])
        overlaps <- gOverlaps(slidesSHP[s,], slidesSHP[k,])
        if (overlaps == TRUE) {
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
  



 