# Load packages and data-------------------------------------------------------
library(runoptGPP)
library(raster)
library(rgdal)
library(sp)
library(Rsagacmd)

setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/upper_maipo_river")

# Load digital elevation model (DEM)
dem <- raster("elev_alos_12_5m.tif")

# Load runout source points
nm_srcpoints <- "debris_flow_source_points"
source_points <- readOGR(".", nm_srcpoints)

# Load runout track polygons 
nm_runoutpoly <- "debris_flow_runout_polys"
runout_polygons <- readOGR(".", nm_runoutpoly)

# assign object ID based on row number

runout_polygons$objectid <- 1:length(runout_polygons)

# Select a debris flow and source point for this example
runout_polygon <- runout_polygons[77,]

sel_source_point  <- over(source_points, runout_polygon)
source_point <- source_points[!is.na(sel_source_point$objectid),]

plot(runout_polygon)
plot(source_point, add = TRUE)

# initialise SAGA GIS ----------------------------------------------------------
saga <- saga_gis(opt_lib = "sim_geomorphology")


# GPP random walk to simluate runout path
rwPerformance(dem, slide_plys = runout_polygon, slide_src = source_point,
              slp = 30, ex = 3, per = 2,
              gpp_iter = 1000, buffer_ext = 500, buffer_source = 50,
              plot_eval = TRUE, saga_lib = saga)

#GPP random walk: grid search optimization -------------------------------------

steps <- 3
# Exponent controlling lateral spread
rwexp_vec <- seq(1.3, 3, len=steps)
# Persistence factor to weight flow direction consistency
rwper_vec <- seq(1.5, 2, len=steps)
# Slope threshold - below lateral spreading is modeled.
rwslp_vec <- seq(20, 40, len=steps)

# compute perfomances values across grid seach space (AUROC)
rw_gridsearch <- rwGridsearch(dem, slide_plys = runout_polygon, slide_src = source_point,
                              #Input random walk grid search space
                              slp_v = rwslp_vec, ex_v = rwexp_vec, per_v = rwper_vec,
                              #Set number of simulation iterations
                              gpp_iter = 1000,
                              #Define processing extent size (m)
                              buffer_ext = 0,
                              #(Optional) Define size of buffer to make source area from point
                              buffer_source = 50,
                              saga_lib = saga)

# paramters set resulted in highest AUROC score
rw_opt_single <- rwGetOpt_single(rw_gridsearch)
rw_opt_single


# GGP PCM: model simulation: runout distance------------------------------------

pcm <- pcmPerformance(dem, slide_plys = runout_polygon, slide_src = source_point,
                      rw_slp = 40, rw_ex = 2.15, rw_per = 1.5,
                      pcm_mu = 0.15, pcm_md = 120,
                      gpp_iter = 1000, buffer_ext = 500, buffer_source = 50,
                      plot_eval = TRUE, return_features = TRUE, saga_lib = saga)

# Runout distance relative error
pcm$length.relerr

# Plot GPP PCM runout modelling ouputs
gpp_output <- stack(pcm$gpp.parea, pcm$gpp.stop, pcm$gpp.maxvel)
names(gpp_output) <- c("Process_area", "Stop_positions", "Max_velocity")
plot(gpp_output)


#GPP PCM: grid search optimization -------------------------------------

# The mass-to-drag ratio (m)
pcmmd_vec <- seq(20, 120, by=20)
# The sliding friction coefficient
pcmmu_vec <- seq(0.05, 0.3, by=0.1)

pcm_gridsearch <- pcmGridsearch(dem,
                                slide_plys = runout_polygon, slide_src = source_point,
                                #Plug-in random walk optimal parameters
                                rw_slp = rw_opt_single$rw_slp_opt,
                                rw_ex = rw_opt_single$rw_exp_opt,
                                rw_per = rw_opt_single$rw_per_opt,
                                #Input PCM grid search space
                                pcm_mu_v = pcmmu_vec,
                                pcm_md_v = pcmmd_vec,
                                #Set number of simulation iterations
                                gpp_iter = 1000,
                                #Define processing extent size (m)
                                buffer_ext = 500,
                                #(Optional) Define size of buffer to make source area from point
                                buffer_source = 50, 
                                saga_lib = saga)

# Get optimal parameters
pcmGetOpt_single(pcm_gridsearch)
