classifyTreeline <- function(dem, runout_ply, treeline) {
  # This function classfiy feature, which lay above or below based on 
  # a definded height 
  
  # dem: a digital elevation model as a raster object
  # runout_ply: runout polygons (SpatialPolygonsDataFrame)
  # treeline : heigth for classification
  # lowest: if TRUE the lowest point of polygon are used to compare to treeline
  #        if FALSE he highest point of polygon are used to compare to treeline
  
  
  runout_ply$below_treeline <- "No"
  
  tree_idx <- which(colnames(runout_ply@data) =="below_treeline")
  
  for (i in 1:length(runout_ply)){
    runout_ply_hgts <- extract(dem, runout_ply[i,], method = "simple", df = TRUE)
    # if one value in polygon under value: Yes
    for (v in slide_hgts[,2]) {
      if (v <= 1500) {
        slides[i,tree_idx] <- "Yes"
        break
      }
    }
    
  }
  
  return(runout_ply)
  
}


combineConnPolys <- function(runout_ply) {
  # This function combine all feature that are connected
  
  # dem: a digital elevation model as a raster object
  
  runout_ply_tree <- runout_ply[runout_ply@data$below_treeline == "Yes", "below_treeline"]
  
  runout_ply_combined <- rmapshaper::ms_dissolve(runout_ply_tree)
  
  runout_ply_split <- disaggregate(runout_ply_combined)
  
  runout_ply_splt$objectid <- 1:length(runout_ply_splt)
  
  # delete old column
  runout_ply_split$rmapshaperid <- NULL
  
  return(runout_ply_split)
  
}


classConnPly <- function(runout_ply, river_ply) {
  # This function classfiy feature, if they are connected to river polygon or not
  
  # dem: a digital elevation model as a raster object
  # runout_ply: runout polygons (SpatialPolygonsDataFrame)
  
  # add new attribute: aggregate
  runout_ply$connected = "No"
  # define index of connected column
  con_idx <- which(colnames(runout_ply@data) =="connected")
  
  # add "yes" to all feature that are conected to river feature
  for (s in 1:length(runout_ply)) {
    overlap <- gOverlaps(runout_ply[s,], river_ply)
    touch <- gTouches(runout_ply[s,], river_ply)
    if (touch == TRUE || overlap == TRUE){
      runout_ply[s,con_idx] <- "Yes"
    }
  }
  return(runout_ply)
  
}


autoSelCells <- function(dem = crop_dtm, runout_ply = slide, prop= 0.05, upper = TRUE){
  # This function creates a grid of the upper or lower percentage of cells
  # in a polygon (e.g. runout track) based on elevation.
  
  # dem: a digital elevation model as a raster object
  # runout_ply: a runout track polygon (SpatialPolygonsDataFrame)
  # prop: is percentage of cells (in proportions e.g 0.05 = 5%)
  # upper: if TRUE the upper elevation cells (source) are determined, 
  #        if FALSE the lower elevation cells (deposition) are determined
  #        
  
  
  slide_elev <- raster::extract(dem, runout_ply, cellnumbers = TRUE, df = TRUE)
  names(slide_elev) <- c("ID", "cell", "elev")
  
  num_cells <- nrow(slide_elev)
  cells_per <- prop* num_cells
  
  if(cells_per < 1){
    cells_per = 1
  }
  
  if(upper == TRUE){
    release_ids <- as.numeric(rownames(slide_elev[order(slide_elev$elev, decreasing = TRUE),])[1:cells_per])
  } else {
    release_ids <- as.numeric(rownames(slide_elev[order(slide_elev$elev, decreasing = FALSE),])[1:cells_per])
  }
  
  release_cells <- slide_elev$cell[release_ids]
  release_grid <- setValues(dem, NA)
  
  release_grid[release_cells] <- 1
  
  return(release_grid)
  
}