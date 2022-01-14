library(runoptGPP)
library(raster)
library(rgdal)    
library(Rsagacmd)

saga <- saga_gis(opt_lib = "sim_geomorphology",
                 temp_path = "C:/SpatialAnalyst_Temp")

#source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/func/custom_runoptGPP_fun.R")
#source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/func/pcm_conn_fun.R")

source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/My_Projekt/all_skripts/pcm_fun.R")

# load data---------------------------------------------------------------------
setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla/edit_files")


# runout and source cells polygon ----------------------------------------------

runout_polygons <- readOGR(".", "DF_Stolla_merged_classified")

#choose only connected debris flows
runout_polygons_conn <- runout_polygons[runout_polygons$connected == "Yes", "connected"]

src_poly <- readOGR(".", "src_poly_5per")
src_poly@proj4string@projargs <- proj4string(runout_polygons)

setwd("../")
river <- readOGR(".", "Stolla_channel")
river@proj4string@projargs <- proj4string(runout_polygons)

dem <- raster("stolla_fillsinks_dtm5m.tif")
# Test PCM conn
#for(i in 1:length(runout_polygons)){
#  pcmConn(dem, slide_plys = runout_polygons, slide_src, slide_id = i, conn_obj = river,
#          rw_slp = 40, rw_ex = 3, rw_per = 1.6,
#          pcm_mu = 0.1, pcm_md = 40,
#          buffer_ext = 500, buffer_source = NULL, gpp_iter = 1000,
#          predict_threshold = 0.5, plot_eval = TRUE,
#          return_features = FALSE, saga_lib)

#}

## Get optimal parameter of random walk-----------------------------------------

setwd("opt_rw")

(load("rw_gridsearch_n15_seed10.Rd"))

# Get RW optimal parameter set
rw_opt <- rwGetOpt(rw_gridsearch_multi, measure = median)

#save(rw_opt, file = "rw_opt_params_n100_all.Rd")

## PCM optimization ------------------------------------------------------------
pcmmd_vec <- seq(20, 150, by=5) #mass-to-drag ratio
pcmmu_vec <- seq(0.04, 0.6, by=0.01) #sliding friction coefficient

library(foreach)

# Define which runout polygons are used for optimization
#polyid_vec <- 6:6 #c(,35,51,56,92)
polyid_vec <- 1:length(runout_polygons_conn)


#polyid_vec <- c(12, 24)
# Location to save temp files
setwd("../opt_pcm/conn")


# Run using parallelization
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

pcm_gridsearch_multi <-
  foreach(poly_id=polyid_vec, .packages=c('rgdal','raster', 'rgeos', 'ROCR', 'Rsagacmd', 'sf', 'runoptGPP')) %dopar% {
    
    .GlobalEnv$saga <- saga
    
    pcmConnGridsearch(dem, conn_obj = river,
                      slide_plys = runout_polygons_conn, slide_src = src_poly, slide_id = poly_id,
                      rw_slp = rw_opt$rw_slp_opt, rw_ex = rw_opt$rw_exp_opt, rw_per = rw_opt$rw_per_opt,
                      pcm_mu_v = pcmmu_vec, pcm_md_v = pcmmd_vec,
                      gpp_iter = 1000,                              # soll 1000
                      buffer_ext = 500,
                      predict_threshold = 0.5, save_res = TRUE,
                      plot_eval = FALSE, saga_lib = saga)
    
  }



parallel::stopCluster(cl)

#-------------------------------------------------------------------------------

