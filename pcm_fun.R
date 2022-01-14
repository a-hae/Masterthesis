# function for pcm :
#-------------------------------------------------------------------------------
#- allTrue
#- errorIndvConn
#- errMinBboxLength
#- mse
#- rmse
#- nintyTrue
#- nintyFiveTrue
#- pcmConn
#- pcmConnGridsearch
#- pcmGetConnOpt
#- pcmSpConn
#- runoutGeom
# ------------------------------------------------------------------------------
allTrue <- function(x){ all(x == TRUE)}

errorIndvConn <- function(x, pcm_opt = NULL){
  
  # Extracts the individual errors for each
  # runout path based on a set of optimized parameters
  
  # Can also submit pcmGetOpt object
  
  if(is.null(pcm_opt)){
    
    pcm_opt <- pcmGetConnOpt(x)
    
  }
  
  pcm_md_vec <- as.numeric(colnames(x[[1]][[1]]))
  pcm_mu_vec <- as.numeric(rownames(x[[1]][[1]]))
  
  # Index performance for opt param set
  ind_md <- which(pcm_md_vec == pcm_opt$pcm_md)
  ind_mu <- which(pcm_mu_vec == pcm_opt$pcm_mu)
  
  perf <- data.frame(relerr = rep(NA, length(x),
                                  error = NA, auroc = NA, conn = NA))
  
  for(i in 1:length(x)){
    perf$relerr[i] <- x[[i]]$relerr[ind_mu, ind_md]
    perf$error[i] <- x[[i]]$error[ind_mu, ind_md]
    perf$auroc[i] <- x[[i]]$auroc[ind_mu, ind_md]
    perf$conn[i] <- x[[i]]$connect[ind_mu, ind_md]
  }
  
  return(perf)
  
}

errMinBboxLength <- function(obs_poly, pred_raster, dem){
  #Calculates the relative error b/w bbox length estimates of slides
  sp.pred <- raster::rasterToPolygons(pred_raster, n = 4, dissolve = TRUE, na.rm=TRUE)
  geom_pred <- runoutGeom(sp.pred, elev = dem)
  geom_act <- runoutGeom(obs_poly, elev = dem)
  
  err <- geom_pred$length - geom_act$length
  rel_err <- abs(geom_act$length - geom_pred$length) / geom_act$length
  rel_diff<- (geom_pred$length - geom_act$length) / geom_act$length
  
  return(
    list(rel_error = rel_err,
         rel_difference = rel_diff,
         error = err
    ))
}

mse <- function(error){ mean(error^2)}

rmse <- function(error){
  sqrt(mean(error^2))
}

nintyTrue <- function(x) {
  if ((sum(x==TRUE)/length(x)) >= 0.9){
    return (TRUE)
  } else{ 
    return (FALSE)
  }
  
}

nintyFiveTrue <- function(x) {
  if ((sum(x==TRUE)/length(x)) >= 0.95){
    return (TRUE)
  } else{ 
    return (FALSE)
  }
  
}


pcmConn <- function(dem, slide_plys, slide_src, slide_id = 1, conn_obj,
                    rw_slp = 33, rw_ex = 3, rw_per = 2,
                    pcm_mu = 0.3, pcm_md = 75,
                    buffer_ext = 500, buffer_source = 50, gpp_iter = 1000,
                    predict_threshold = 0.5, plot_eval = FALSE,
                    return_features = FALSE, saga_lib)
{
  
  # If single runout polygon as input, assign slide_id value of 1
  if(length(slide_plys) == 1){
    slide_id <- 1
  }
  
  slide_plys$objectid <- 1:length(slide_plys)
  # Subset a single slide polygon
  slide_poly_single <- slide_plys[slide_id,]
  
  # Crop dem to slide polygon
  dem_grid <- raster::crop(dem, raster::extent(slide_poly_single) + buffer_ext)
  
  
  if(class(slide_src) == "SpatialPointsDataFrame"){
    # Subset corresponding source/start point of runout
    sel_over_start_point  <- sp::over(slide_src, slide_poly_single)
    sel_start_point <- slide_src[!is.na(sel_over_start_point$objectid),]
    
    if(!is.null(buffer_source)){
      # Create a buffer around source point to create a source/release area
      source_buffer <- rgeos::gBuffer(sel_start_point, width = buffer_source)
      source_grid <- raster::rasterize(source_buffer, dem_grid, field=1 )
      source_grid <- raster::mask(source_grid, slide_poly_single )
      source_plot <- raster::rasterToPolygons(source_grid)
    } else {
      # Just use source point
      source_plot <- sel_start_point
      source_grid <- raster::rasterize(matrix(sp::coordinates(sel_start_point)[1:2], ncol = 2), dem_grid, field = 1)
    }
  }
  
  if(class(slide_src) == "SpatialPolygonsDataFrame" ){
    sel_over_start_poly <- sp::over(slide_src, slide_poly_single)
    sel_start_poly <- slide_src[!is.na(sel_over_start_poly$objectid),]
    source_plot <- sel_start_poly
    source_grid <- raster::rasterize(sel_start_poly, dem_grid, field=1 )
    
  }
  
  
  if(class(slide_src) == "RasterLayer"){
    source_grid <- raster::crop(slide_src, dem_grid)
    source_grid <- raster::mask(source_grid, slide_poly_single )
    source_plot <- source_grid
  }
  
  # Run runout model using Rsagacmd (faster than RSAGA)
  gpp <- saga_lib$sim_geomorphology$gravitational_process_path_model(dem = dem_grid, release_areas = source_grid,
                                                                     #friction_mu_grid = mu.grid,
                                                                     process_path_model = 1,
                                                                     rw_slope_thres = rw_slp,
                                                                     rw_exponent = rw_ex,
                                                                     rw_persistence = rw_per,
                                                                     gpp_iterations = gpp_iter,
                                                                     friction_model = 5,
                                                                     friction_mu = pcm_mu,
                                                                     friction_mass_to_drag = pcm_md)
  
  
  # Rescale to values from 0 to 1
  rescale_process_area <- rasterCdf(gpp$process_area)
  
  # AUROC
  pred_values <- raster::getValues(rescale_process_area)
  pred_values[is.na(pred_values)] <- 0
  
  slide_area <- raster::rasterize(slide_poly_single, gpp$process_area, field=1, background = 0)
  
  pred_thres <- rescale_process_area
  pred_thres[pred_thres >= predict_threshold] = 1
  pred_thres[pred_thres < predict_threshold] = NA
  
  pred_area <- ROCR::prediction(predictions = pred_values, labels = raster::getValues(slide_area))
  perf_area <- ROCR::performance(pred_area, "tpr", "fpr")
  auroc_area <- ROCR::performance(pred_area, "auc")
  roc <- auroc_area@y.values[[1]]
  
  # Length loss
  errMinBox <- errMinBboxLength(obs_poly = slide_poly_single,
                                pred_raster = pred_thres,
                                dem = dem)
  
  length_relerr <- errMinBox[["rel_error"]]
  length_reldiff <- errMinBox[["rel_difference"]]
  length_error <- errMinBox[["error"]]
  
  
  # Test if connected
  
  in_obj <- unlist(extract(gpp$process_area, conn_obj))
  
  is_conn <- !all(is.na(in_obj))
  
  # Plot evaluation results
  if(plot_eval){
    
    slope <- raster::terrain(dem_grid, opt='slope')
    aspect <- raster::terrain(dem_grid, opt='aspect')
    # create hillshade
    # numbers
    hs <- raster::hillShade(slope, aspect,
                            angle=40,
                            direction=270)
    
    crop_river <- raster::crop(conn_obj, dem_grid)
    
    sp::plot(hs, col=grey.colors(100, start=0, end=1), legend = FALSE,
             main = paste("id", slide_id, "connected", is_conn,
                          "roc", round(roc, digits=2), "err.L", round(length_relerr, digits = 2), "\n",
                          "slp", rw_slp, "exp", rw_ex, "per", rw_per, "mu", pcm_mu, "M/D", pcm_md),
             cex.main = 0.7, cex.axis = 0.7, cex=0.7)
    
    if(!is.null(crop_river)){
      # don't plot river if not in crop area (won't work otherwise)
      sp::plot(crop_river, col = "#4193CB", add = TRUE)
    }
    
    sp::plot(rescale_process_area, alpha = 0.4, legend = TRUE, add=TRUE)
    
    sp::plot(slide_poly_single, add=TRUE)
    sp::plot(source_plot, add = TRUE, legend = FALSE)
    
  }
  
  
  if(return_features){
    return(
      list(id = slide_id,
           conn = is_conn,
           roc = roc,
           length.relerr = length_relerr,
           length.reldiff = length_reldiff,
           length.error = length_error,
           dem = dem_grid,
           actual.poly = slide_poly_single,
           #source.pnt = sel_start_point,
           source.area = source_grid,
           gpp.parea = gpp$process_area,
           gpp.stop = gpp$stop_positions,
           gpp.maxvel = gpp$max_velocity))
  } else {
    return(
      list(id =  slide_id,
           conn = is_conn,
           roc = roc,
           length.relerr = length_relerr,
           length.reldiff = length_reldiff,
           length.error = length_error))
  }
  
}

pcmConnGridsearch <- function(dem, conn_obj,
                              slide_plys, slide_src, slide_id = NULL,
                              rw_slp, rw_ex, rw_per,
                              pcm_mu_v, pcm_md_v,
                              gpp_iter = 1000,
                              buffer_ext = 500, buffer_source = NULL,
                              predict_threshold = 0.5,
                              save_res = FALSE, plot_eval = FALSE, saga_lib){
  
  if(is.null(slide_id)){
    slide_id = 1
  }
  
  column.names <- pcm_md_v
  row.names <- pcm_mu_v
  
  roc_result <- array(NA ,dim = c(length(pcm_mu_v),length(pcm_md_v)),dimnames = list(row.names, column.names))
  relerr_length_result <- roc_result
  reldiff_length_result <- roc_result
  err_length_result <- roc_result
  conn_result <- roc_result
  
  #roc[row, col]
  #roc[pcmmu, pcmmd]
  
  for(i in 1:length(pcm_md_v)){
    for(k in 1:length(pcm_mu_v)){
      
      result <- pcmConn(dem = dem, conn_obj = conn_obj,
                        slide_plys = slide_plys,
                        slide_src = slide_src,
                        slide_id = slide_id,
                        rw_slp = rw_slp,
                        rw_ex = rw_ex,
                        rw_per = rw_per,
                        pcm_mu = pcm_mu_v[k],
                        pcm_md = pcm_md_v[i],
                        buffer_ext = buffer_ext,
                        buffer_source = buffer_source,
                        gpp_iter = gpp_iter,
                        predict_threshold = predict_threshold,
                        plot_eval = FALSE,
                        saga_lib = saga_lib)
      
      roc_result[k, i] <- result$roc
      conn_result[k, i] <- result$conn
      relerr_length_result[k, i] <- result$length.relerr
      reldiff_length_result[k, i] <- result$length.reldiff
      err_length_result[k, i] <- result$length.error
      
      #roc_result[k, i] <- paste(pcmmd[i], pcmmu[k])
    }}
  
  result_pcm <- list(
    auroc = roc_result,
    relerr = relerr_length_result,
    reldiff = reldiff_length_result,
    error = err_length_result,
    connect = conn_result)
  
  if(save_res){
    save(result_pcm, file = paste("result_pcmconn_gridsearch_", slide_id, ".Rd", sep=""))
  }
  
  return(result_pcm)
  
}


pcmGetConnOpt <- function(x, performance = "relerr", measure = "median", conn_proportion = "allTrue",
                          from_save = FALSE, plot_opt = FALSE){
  
  # conn_proportion: allTrue, nintyTrue, nintyFiveTrue
  
  if(from_save){
    x <- list()
    
    files <- list.files(pattern = "result_pcm_gridsearch_")
    
    for(i in 1:length(files)){
      res_nm <- paste("result_pcmconn_gridsearch_", i, ".Rd", sep="")
      res_obj_nm <- load(res_nm)
      result_pcm <- get(res_obj_nm)
      x[[i]] <- result_pcm
    }
    
  }
  
  pcm_md_vec <- as.numeric(colnames(x[[1]][[1]]))
  pcm_mu_vec <- as.numeric(rownames(x[[1]][[1]]))
  
  n_train <- length(x)
  
  err_list <- list()
  roc_list <- list()
  conn_list <- list()
  
  for(i in 1:n_train){
    
    err_list[[i]] <- x[[i]][[performance]]
    roc_list[[i]] <- x[[i]][['auroc']]
    conn_list[[i]] <- x[[i]][['connect']]
    
  }
  
  err <- apply(simplify2array(err_list), 1:2, get(measure))
  roc <- apply(simplify2array(roc_list), 1:2, get(measure))
  conn <- apply(simplify2array(conn_list), 1:2, conn_proportion)
  
  # Filter results for only runout paths that connect to object (e.g. river)
  min_conn_err <- min(err[conn])
  
  min_conn_err <- err[conn][3]
  
  err_wh <- which(err==min_conn_err, arr.ind=TRUE)
  err[err_wh]
  
  # Use AUROC for tie breaking
  if(length(err_wh) > 2){
    err_wh <- err_wh[which(roc[err_wh]==max(roc[err_wh]), arr.ind=TRUE),]
  }
  
  # If still no tie break, take first one...
  if(length(err_wh) > 2){
    err_wh <- err_wh[1,]
  }
  
  opt_md <- pcm_md_vec[err_wh[2]] #col
  opt_mu <- pcm_mu_vec[err_wh[1]] #row
  
  opt_gpp_par <- data.frame(
    pcm_mu = opt_mu,
    pcm_md = opt_md
  )
  
  opt_gpp_par[paste0(measure, "_", performance)] <- err[err_wh[1], err_wh[2]]
  opt_gpp_par[paste0(measure, "_", "auroc")] <- roc[err_wh[1], err_wh[2]]
  
  if(plot_opt){
    
    err_df <- reshape2::melt(err)
    
    gg <- ggplot2::ggplot(data = err_df, ggplot2::aes(x=err_df$Var2, y=err_df$Var1, z=err_df$value)) +
      ggplot2::geom_tile(ggplot2::aes(fill = err_df$value)) +
      
      ggplot2::ylab(expression(paste("Sliding friction coefficient"))) +
      ggplot2::xlab("Mass-to-drag ratio (m)") +
      
      ggplot2::labs(fill="Median relative\nrunout distance\nerror") +
      
      ggplot2::scale_fill_viridis_c(direction = 1) +
      ggplot2::theme_light() +
      ggplot2::theme(text = ggplot2::element_text(family = "Arial", size = 8),
                     axis.title = ggplot2::element_text(size = 9),
                     axis.text = ggplot2::element_text(size = 8))
    
    print(gg)
  }
  
  return(opt_gpp_par)
  
}


pcmGetConnOpt_old <- function(x, performance = "relerr", measure = "median",
                          from_save = FALSE, plot_opt = FALSE){
  
  if(from_save){
    x <- list()
    
    #files <- list.files(pattern = "result_pcm_gridsearch_")
    
    for(i in list){
      res_nm <- paste("result_pcmconn_gridsearch_", i, ".Rd", sep="")
      res_obj_nm <- load(res_nm)
      result_pcm <- get(res_obj_nm)
      x[[i]] <- result_pcm
    }
    
  }
  
  
  pcm_md_vec <- as.numeric(colnames(x[[1]][[1]]))
  pcm_mu_vec <- as.numeric(rownames(x[[1]][[1]]))
  
  n_train <- length(x)
  
  conn_list <- list()
  
  for (i in 1:n_train) {
    conn_list[[i]] <- allTrue(simplify2array(x[[i]][['connect']]))
  }
  
  err_list <- list()
  roc_list <- list()
  
  for(i in 1:n_train){
    
    if (conn_list[[i]] == TRUE) {
      err_list[[i]] <- x[[i]][[performance]]
      roc_list[[i]] <- x[[i]][['auroc']]
    }
  }
  err_list <- err_list[!sapply(err_list,is.null)]
  roc_list <- roc_list[!sapply(roc_list,is.null)]
  
  err <- apply(simplify2array(err_list), 1:2, get(measure))
  roc <- apply(simplify2array(roc_list), 1:2, get(measure))
  #conn <- apply(simplify2array(conn_list), 1:2, allTrue)
  
  # Filter results for only runout paths that connect to object (e.g. river)
  #min_conn_err <- min(err[conn])
  
  #min_conn_err <- err[conn][3]
  min_conn_err <- min(err)
  
  err_wh <- which(err==min_conn_err, arr.ind=TRUE)
  err[err_wh]
  
  # Use AUROC for tie breaking
  if(length(err_wh) > 2){
    err_wh <- err_wh[which(roc[err_wh]==max(roc[err_wh]), arr.ind=TRUE),]
  }
  
  # If still no tie break, take first one...
  if(length(err_wh) > 2){
    err_wh <- err_wh[1,]
  }
  
  opt_md <- pcm_md_vec[err_wh[2]] #col
  opt_mu <- pcm_mu_vec[err_wh[1]] #row
  
  opt_gpp_par <- data.frame(
    pcm_mu = opt_mu,
    pcm_md = opt_md
  )
  
  opt_gpp_par[paste0(measure, "_", performance)] <- err[err_wh[1], err_wh[2]]
  opt_gpp_par[paste0(measure, "_", "auroc")] <- roc[err_wh[1], err_wh[2]]
  
  if(plot_opt){
    
    err_df <- reshape2::melt(err)
    
    gg <- ggplot2::ggplot(data = err_df, ggplot2::aes(x=err_df$Var2, y=err_df$Var1, z=err_df$value)) +
      ggplot2::geom_tile(ggplot2::aes(fill = err_df$value)) +
      
      ggplot2::ylab(expression(paste("Sliding friction coefficient"))) +
      ggplot2::xlab("Mass-to-drag ratio (m)") +
      
      ggplot2::labs(fill="Median relative\nrunout distance\nerror") +
      
      ggplot2::scale_fill_viridis_c(direction = 1) +
      ggplot2::theme_light() +
      ggplot2::theme(text = ggplot2::element_text(family = "Arial", size = 8),
                     axis.title = ggplot2::element_text(size = 9),
                     axis.text = ggplot2::element_text(size = 8))
    
    print(gg)
  }
  
  return(opt_gpp_par)
  
}

pcmSpConn <- function(dem, slide_plys, slide_src, slide_id = 1, conn_obj,
                      rw_slp = 33, rw_ex = 3, rw_per = 2,
                      mu_grid, md_grid,
                      buffer_ext = 500, buffer_source = 50, gpp_iter = 1000,
                      predict_threshold = 0.5, plot_eval = FALSE,
                      return_features = FALSE, saga_lib)
{
  
  # If single runout polygon as input, assign slide_id value of 1
  if(length(slide_plys) == 1){
    slide_id <- 1
  }
  
  slide_plys$objectid <- 1:length(slide_plys)
  # Subset a single slide polygon
  slide_poly_single <- slide_plys[slide_id,]
  
  # Crop dem to slide polygon
  dem_grid <- raster::crop(dem, raster::extent(slide_poly_single) + buffer_ext)
  
  
  if(class(slide_src) == "SpatialPointsDataFrame"){
    # Subset corresponding source/start point of runout
    sel_over_start_point  <- sp::over(slide_src, slide_poly_single)
    sel_start_point <- slide_src[!is.na(sel_over_start_point$objectid),]
    
    if(!is.null(buffer_source)){
      # Create a buffer around source point to create a source/release area
      source_buffer <- rgeos::gBuffer(sel_start_point, width = buffer_source)
      source_grid <- raster::rasterize(source_buffer, dem_grid, field=1 )
      source_grid <- raster::mask(source_grid, slide_poly_single )
      source_plot <- raster::rasterToPolygons(source_grid)
    } else {
      # Just use source point
      source_plot <- sel_start_point
      source_grid <- raster::rasterize(matrix(sp::coordinates(sel_start_point)[1:2], ncol = 2), dem_grid, field = 1)
    }
  }
  
  if(class(slide_src) == "SpatialPolygonsDataFrame" ){
    sel_over_start_poly <- sp::over(slide_src, slide_poly_single)
    sel_start_poly <- slide_src[!is.na(sel_over_start_poly$objectid),]
    source_plot <- sel_start_poly
    source_grid <- raster::rasterize(sel_start_poly, dem_grid, field=1 )
    
  }
  
  
  if(class(slide_src) == "RasterLayer"){
    source_grid <- raster::crop(slide_src, dem_grid)
    source_grid <- raster::mask(source_grid, slide_poly_single )
    source_plot <- source_grid
    crop_mu_grid <- raster::crop(mu_grid, dem_grid)
    crop_md_grid <- raster::crop(md_grid, dem_grid)
  }
  
  # Run runout model using Rsagacmd (faster than RSAGA)
  gpp <- saga_lib$sim_geomorphology$gravitational_process_path_model(dem = dem_grid, release_areas = source_grid,
                                                                     friction_mu_grid = crop_mu_grid,
                                                                     friction_mass_to_drag_grid = crop_md_grid,
                                                                     process_path_model = 1,
                                                                     rw_slope_thres = rw_slp,
                                                                     rw_exponent = rw_ex,
                                                                     rw_persistence = rw_per,
                                                                     gpp_iterations = gpp_iter,
                                                                     friction_model = 5)
  #friction_mu = pcm_mu,
  #friction_mass_to_drag = pcm_md)
  
  
  # Rescale to values from 0 to 1
  rescale_process_area <- rasterCdf(gpp$process_area)
  
  # AUROC
  pred_values <- raster::getValues(rescale_process_area)
  pred_values[is.na(pred_values)] <- 0
  
  slide_area <- raster::rasterize(slide_poly_single, gpp$process_area, field=1, background = 0)
  
  pred_thres <- rescale_process_area
  pred_thres[pred_thres >= predict_threshold] = 1
  pred_thres[pred_thres < predict_threshold] = NA
  
  pred_area <- ROCR::prediction(predictions = pred_values, labels = raster::getValues(slide_area))
  perf_area <- ROCR::performance(pred_area, "tpr", "fpr")
  auroc_area <- ROCR::performance(pred_area, "auc")
  roc <- auroc_area@y.values[[1]]
  
  # Length loss
  errMinBox <- errMinBboxLength(obs_poly = slide_poly_single,
                                pred_raster = pred_thres,
                                dem = dem)
  
  length_relerr <- errMinBox[["rel_error"]]
  length_reldiff <- errMinBox[["rel_difference"]]
  length_error <- errMinBox[["error"]]
  
  
  # Test if connected
  
  in_obj <- unlist(extract(gpp$process_area, conn_obj))
  
  is_conn <- !all(is.na(in_obj))
  
  # Plot evaluation results
  if(plot_eval){
    
    slope <- raster::terrain(dem_grid, opt='slope')
    aspect <- raster::terrain(dem_grid, opt='aspect')
    # create hillshade
    # numbers
    hs <- raster::hillShade(slope, aspect,
                            angle=40,
                            direction=270)
    
    crop_river <- raster::crop(conn_obj, dem_grid)
    
    sp::plot(hs, col=grey.colors(100, start=0, end=1), legend = FALSE,
             main = paste("id", slide_id, "connected", is_conn,
                          "roc", round(roc, digits=2), "err.L", round(length_relerr, digits = 2), "\n",
                          "slp", rw_slp, "exp", rw_ex, "per", rw_per, "mu", "SP", "M/D", "SP"),
             cex.main = 0.7, cex.axis = 0.7, cex=0.7)
    
    if(!is.null(crop_river)){
      # don't plot river if not in crop area (won't work otherwise)
      sp::plot(crop_river, col = "#4193CB", add = TRUE)
    }
    
    sp::plot(rescale_process_area, alpha = 0.4, legend = TRUE, add=TRUE)
    sp::plot(slide_poly_single, add=TRUE)
    #sp::plot(source_plot, add = TRUE, legend = FALSE)
    
  }
  
  
  if(return_features){
    return(
      list(id = slide_id,
           conn = is_conn,
           roc = roc,
           length.relerr = length_relerr,
           length.reldiff = length_reldiff,
           length.error = length_error,
           dem = dem_grid,
           actual.poly = slide_poly_single,
           #source.pnt = sel_start_point,
           source.area = source_grid,
           gpp.parea = gpp$process_area,
           gpp.stop = gpp$stop_positions,
           gpp.maxvel = gpp$max_velocity))
  } else {
    return(
      list(id =  slide_id,
           conn = is_conn,
           roc = roc,
           length.relerr = length_relerr,
           length.reldiff = length_reldiff,
           length.error = length_error))
  }
  
}

runoutGeom <- function(runout_plys, elev, ID = NULL) {
  #Create a dataframe to store the ID, length, width and (landslide) area
  if(is.null(ID)){
    bbDf <- data.frame(fid = 0:(length(runout_plys)-1), id = 1:length(runout_plys),
                       width = NA, length = NA, area = NA, surfacearea = NA,
                       maxelev = NA, minelev = NA, reachangle = NA)
  } else {
    bbDf <- data.frame(fid = 0:(length(runout_plys)-1), id = NA,
                       width = NA, length = NA, area = NA, surfacearea = NA,
                       maxelev = NA, minelev = NA, reachangle = NA)
  }
  
  
  n_features <- length(runout_plys@polygons)
  
  for (i in 1:n_features){
    runout_ply <- runout_plys[i,]
    
    if(!is.null(ID)){bbDf[i,]$id <- runout_ply@data[,ID]}
    #Get the vertices coordinates of from SpatialPolygons
    #pnts <- runout_ply@polygons[[1]]@Polygons[[1]]@coords
    pnts <- getVertices(runout_ply)
    #Calculate minimum area rectangle
    bb <- minbb(pnts)
    
    #Calcluate the difference in elevation between points
    elevExt <- raster::extract(elev, bb$box[1:4,])
    dElev12 <- sqrt( (elevExt[1] - elevExt[2])^2)
    dElev14 <- sqrt( (elevExt[1] - elevExt[4])^2)
    
    if(is.na(dElev14)){
      #If bounding box node extends beyond dem (e.g. in NA area)
      dElev14 <- sqrt( (elevExt[2] - elevExt[3])^2)
    }
    #Determine (and calculate) the length and width based on delta elevation
    
    if(is.na(dElev12)){
      #If bounding box node extends beyond dem (e.g. in NA area)
      dElev12 <- sqrt( (elevExt[4] - elevExt[3])^2)
    }
    
    if(dElev12 > dElev14) {
      length <- EuclDist(bb$box[1,1], bb$box[1,2], bb$box[2,1], bb$box[2,2])
      width <- EuclDist(bb$box[1,1], bb$box[1,2], bb$box[4,1], bb$box[4,2])
    } else {
      length <- EuclDist(bb$box[1,1], bb$box[1,2], bb$box[4,1], bb$box[4,2])
      width <- EuclDist(bb$box[1,1], bb$box[1,2], bb$box[2,1], bb$box[2,2])
    }
    
    bbDf[i,]$length <- length #planar
    bbDf[i,]$width <- width #planar
    bbDf[i,]$area <- rgeos::gArea(runout_ply) #area of landslide (*not area of bbox) {Rgeos}
    
    #Calculate the 'true' surface area
    elevCrop <- raster::crop(elev, runout_ply) #crop and mask used to speed up calculation
    elevMask <- raster::mask(elevCrop, runout_ply)
    elevMaskSGDF <- as(elevMask , "SpatialGridDataFrame")
    bbDf[i,]$surfacearea <- sp::surfaceArea(elevMaskSGDF) #surfacearea of landslide {sp}
    #Calculate the max. and min. elevation
    elevPnts <- raster::extract(elev, pnts)
    bbDf[i,]$maxelev <- max(elevPnts)
    bbDf[i,]$minelev <- min(elevPnts)
    bbDf[i,]$reachangle <- 90 - (atan( bbDf[i,]$length / ( bbDf[i,]$maxelev- bbDf[i,]$minelev))*180/pi)
    
  }
  return(bbDf)
}