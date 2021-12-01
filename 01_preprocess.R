
#-------------------------------------------------------------------------------
# Tasks;
# - classify above and below treeline
# - create source celss for each polygon
# - merge all features in shapefile which are connected
# - classify: define if feature are connected to river or not (column: connected)

###-----------------------------------------------------------------------------


## load packages ---------------------------------------------------------------

#library(RSAGA)
library(rgdal)
library(rgeos)
#library(dplyr)
library(raster)

## load data--------------------------------------------------------------------

setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla")

dtm <- raster("stolla_dtm5m.tif")

dbflows_ply <- readOGR(".","DF_Stolla_all")

source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/My_Projekt/Masterthesis/preprocess_fun.R")


## classify runout polygons within treeline-------------------------------------

dbflows_classtree <- classTreeline(dtm, dbflows_ply, treeline = 2100)

dbflows_tree <- dbflows_classtree[dbflows_classtree@data$below_treeline == "Yes", "below_treeline"]

plot(dbflows_tree)


writeOGR(`dbflows_tree`, dsn = "edit_files", "DF_Stolla_all_class_treeline", driver="ESRI Shapefile", overwrite_layer =  TRUE)


## create source cells for each runout polygon ---------------------------------

# define proportion of upper cells to select
upper_prop <- 0.05

src_cells <- autoSelCells(dem = dtm, runout_ply = dbflows_tree[1,], prop= upper_prop, upper = TRUE)
src_cells[is.na(src_cells)] <- 0


for(i in 2:length(dbflows_tree)){
  #print(i)
  
  slide <- dbflows_tree[i,]
  
  nxt_src_cells <- autoSelCells(dem = dtm, runout_ply = slide, prop= upper_prop, upper = TRUE)
  nxt_src_cells[is.na(nxt_src_cells)] <- 0
  
  src_cells = src_cells + nxt_src_cells
  
}

src_cells[src_cells == 0] <- NA

plot(src_cells)

writeRaster(src_cells, filename = "src_cells_5per.tif", format = "GTiff", overwrite = TRUE)


## merge all features in shapefile which are connected--------------------------
library(MazamaSpatialUtils)

dbflows_con <- combineConnPolys(dbflows_tree)

# raster od source cells to polygon and add runout polygon Id, 
dbflows_con$Id <- 1: length(dbflows_con)
src_poly <- cellToPly(src_cells, dbflows_con)

writeOGR(`src_poly`, dsn = "edit_files", "src_poly_5per", driver="ESRI Shapefile", overwrite_layer =  TRUE)

##  check for connection to river-----------------------------------------------
river <- readOGR(".","Stolla_channel")

dbflows_class <- classConnPly(dbflows_con, river)

#export shapefile
writeOGR(`dbflows_class`, dsn = "edit_files", "DF_Stolla_merged_classified", driver="ESRI Shapefile", overwrite_layer =  TRUE)




