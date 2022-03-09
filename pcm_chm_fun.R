# Dunrcton to optimice crown height impact on runout
# - ch_per_slides
#-





ch_per_slide <- function(run_slides, chm) {
  # return poylgon darafram with attribute chm
  # $chm: include all crown heigth values in corresponding slide
  
  #run_slides: runout polygon shapefile
  # chm: raster with crown heigth values
  
  #-----------------------------------------------------------------------------
  runout_polygons_chm <- run_slides
  
  runout_polygons_chm$chm <- 0
  
  
  
  # classes for each polygon
  for (i in 1:length(runout_polygons_chm)){
    class_single <- extract(chm, runout_polygons_chm[runout_polygons_chm$Id == i, "Id"])
    df <- as.data.frame(class_single)
    colnames(df)[1] <- "chm"
    #chms_count <- df %>% group_by(chm) %>% summarise(Freq=n()) #count(df)
    chms_count <- df %>% group_by(chm) %>% dplyr::summarise((Freq = n()))
    colnames(chms_count)[2] <- "Freq"
    runout_polygons_chm$chm[runout_polygons_chm$Id == i] <- list(chms_count$chm)
    
  }
  
  return(runout_polygons_chm)
}

#-------------------------------------------------------------------------------


pcmSpConn_thres <- function(chm, thres_grid, pcmmu_vec, dem, slide_plys, slide_src, slide_id = 1, conn_obj,
                            rw_slp = 33, rw_ex = 3, rw_per = 2,
                            mu_grid, md_grid,
                            buffer_ext = 500, buffer_source = 50, gpp_iter = 1000,
                            predict_threshold = 0.5, plot_eval = FALSE, saga_lib) {
  
  # save results for each friction and each crownheight threshold, 
  # model spatial pcm with changing crown height raster
  
  
  # change in each model the friction depending on the threshold
  for (m in 1:length(pcmmu_vec)) {
    
    res <- list()
    
    for (k in 1 : length(thres_grid)) {
      
      # all rastercell with a crown heigth below threshold, have basis values
      chm$mu[chm$stolla_crown_hgt_reclass_v9 < thres_grid[k]] <- 0.05
      # all rastercells with crown heights above threshold, have variing friction value
      chm$mu[chm$stolla_crown_hgt_reclass_v9 >= thres_grid[k]] <- pcmmu_vec[m]
      #res_class<- list()
      
      res[[k]] <- pcmSpConn(dem = dem, slide_plys = slide_plys, slide_src = slide_src, slide_id = slide_id, conn_obj = conn_obj,
                            rw_slp = rw_slp, rw_ex = rw_ex, rw_per = rw_per,
                            mu_grid = chm$mu, md_grid = chm$md,
                            buffer_ext = buffer_ext, buffer_source = buffer_source, gpp_iter = gpp_iter,
                            predict_threshold = predict_threshold, plot_eval = plot_eval,
                            return_features = FALSE,  saga_lib = saga_lib)
      
      
      
      
      
    }
    
    save(res, file = paste("pcmSpconn_thres_id_", slide_id, "_mu_", pcmmu_vec[m], ".Rd", sep=""))
  }
  

}



#-------------------------------------------------------------------------------

forested_area <- function(thres_grid, chm, poly_slides) {
  # return dataframe with forested and non forested area for each threshold
  
  # thres_grid: thresholds grid
  # chm: raster with crown height values (]0,50[])
  # pola_slides: spatialpolygon dataframe with polgons of observed slides
  
  df_thres_perc <- as.data.frame(matrix(nrow = length(thres_grid), ncol = 4))
  colnames(df_thres_perc) <- c("nonforest_pix", "forest_pix", "nonforest_perc","forest_perc" )
  
  #chm$mu <- 0
  
  for (t in 1 : length(thres_grid)) {
    
    # differ chm rastervalue in forested and nonforested
    chm$mu <- 0
    chm$mu[chm$stolla_crown_hgt_reclass_v9 < thres_grid[t]] <- 0
    chm$mu[chm$stolla_crown_hgt_reclass_v9 >= thres_grid[t]] <- 1
    
    #extract all rastercell value within all polygons
    all_chs <- extract(chm$mu, poly_slides)
    
    #list the values
    ls_ch = NULL
    for (i in 1: length(poly_slides)) {
      ls_ch <- append(ls_ch, all_chs[[i]])
    }
    
    # group raster values in non forested and forest and count them
    df_ch <- as.data.frame(ls_ch)
    colnames(df_ch)[1] <- "chm"
    chms_count <- df_ch %>% group_by(chm) %>% dplyr::summarise((Freq = n()))
    colnames(chms_count)[2] <- "Freq"
    
    sum_pix <- sum(chms_count$Freq)
    
    #calculte percantage of non forested and forested areas within all slides
    df_thres_perc$nonforest_pix[t] <- chms_count$Freq[chms_count$chm == 0]
    df_thres_perc$forest_pix[t] <- chms_count$Freq[chms_count$chm == 1]
    
    df_thres_perc$nonforest_perc[t] <- (df_thres_perc$nonforest_pix[t]*100)/sum_pix
    df_thres_perc$forest_perc[t] <- (df_thres_perc$forest_pix[t]*100)/sum_pix

  }
  return(df_thres_perc)
}
