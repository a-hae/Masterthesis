library(runoptGPP)
library(raster)
library(rgdal)
library(sp)
library(Rsagacmd)

## Load data --------------------------------------------------------------------

setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla")

# Load digital elevation model (DEM)
dem <- raster("stolla_dtm5m.tif")

# Load runout source points
#source_cells <- raster("src_cells_low10per.tif")

setwd("edit_files")
runout_polygons <- readOGR(".", "DF_Stolla_merged_classified")
runout_polygons$objectid <- 1:length(runout_polygons)

src_poly <- readOGR(".", "src_poly_5per")
setwd("../")

# change crs of src_poly
src_poly <- spTransform(src_poly, runout_polygons@proj4string)

#Initate saga gis
saga <- saga_gis(opt_lib = "sim_geomorphology")

### create sample data ---------------------------------------------------------

set.seed(10)
runout_polygons_sample <- sample(src_poly$rnt_ply, 15)

sample <- runout_polygons[runout_polygons_sample,]

writeOGR(`sample`, dsn = "edit_files", "DF_Stolla_merged_sample", driver="ESRI Shapefile", overwrite_layer =  TRUE)


### Random walk ----------------------------------------------------------------
## grid seach optimization
steps <- 10
rwexp_vec <- seq(1.3, 3, len=steps)
rwper_vec <- seq(1.5, 2, len=steps)
rwslp_vec <- seq(20, 40, len=steps)

library(foreach)

# Define which runout polygons are used for optimization
polyid_vec <- 1:length(runout_polygons[runout_polygons_sample,])

# Set up cluster
cl <- parallel::makeCluster(4)
doParallel::registerDoParallel(cl)

# Run grid search loop
rw_gridsearch_multi <-
  foreach(poly_id=polyid_vec, .packages=c('rgdal','raster', 'rgeos', 'ROCR', 'Rsagacmd', 'sf', 'runoptGPP')) %dopar% {
    
    .GlobalEnv$saga <- saga
    
    rwGridsearch(dem, slide_plys = runout_polygons[runout_polygons_sample,], slide_src = src_poly,
                 slide_id = poly_id, slp_v = rwslp_vec, ex_v = rwexp_vec, per_v = rwper_vec,
                 gpp_iter = 500, buffer_ext = 500, save_res = FALSE,
                 plot_eval = FALSE, saga_lib = saga)
    
  }

parallel::stopCluster(cl)

rw_opt <- rwGetOpt(rw_gridsearch_multi, measure = median)
rw_opt


## save ------------------------------------------------------------------------

setwd("results")

save(rw_gridsearch_multi, file = "rw_gridsearch_n15_seed10.Rd")

# Get RW optimal parameter set
rw_opt <- rwGetOpt(rw_gridsearch_multi, measure = median)

save(rw_opt, file = "rw_opt_params_n15_seed10.Rd")


## validate paramters using spatial cross validation----------------------------

rw_spcv <- rwSPCV(x = rw_gridsearch_multi, slide_plys = runout_polygons[runout_polygons_sample,],
                  n_folds = 5, repetitions = 10)

freq_rw <- rwPoolSPCV(rw_spcv, plot_freq = TRUE)

freq_rw



