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