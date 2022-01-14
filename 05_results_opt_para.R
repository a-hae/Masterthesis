library(runoptGPP)


source("C:/Users/Annette/OneDrive/Master/Masterarbeit/RProjekt/My_Projekt/Masterthesis/pcm_fun.R")


## get optimal Paramter from files----------------------------------------------7

setwd("C:/Users/Annette/OneDrive/Master/Masterarbeit/Data/Stolla/opt_pcm/conn")


# load data
x <- list()

files <- list.files(pattern = "result_pcmconn_gridsearch_")


for(i in 1:length(files)){
  res_nm <- paste("result_pcmconn_gridsearch_", i, ".Rd", sep="")
  res_obj_nm <- load(res_nm)
  result_pcm <- get(res_obj_nm)
  x[[i]] <- result_pcm
}

pcm_gridsearch_multi <- x

# Optimale parameter
pcm_opt <- pcmGetOpt(pcm_gridsearch_multi, performance = "relerr", measure = "median", plot_opt = TRUE)
pcm_opt

# error for optimal paramters (for individual slides)
errorIndvConn(pcm_gridsearch_multi, pcm_opt = pcm_opt)

# Optimale parameter for all polygons are connected
pcm_connopt <- pcmGetConnOpt(pcm_gridsearch_multi, performance = "relerr", measure = "median",  conn_proportion = "nintyFiveTrue", plot_opt = TRUE, from_save = FALSE)
pcm_connopt


errorIndvConn(pcm_gridsearch_multi, pcm_opt = pcm_connopt)


## save ------------------------------------------------------------------------

setwd("results")

save(pcm_gridsearch_multi, file = "pcm_gridsearch_all.Rd")


save(pcm_opt, file = "pcm_opt_params_all.Rd")

save(pcm_connopt, file = "pcm_conn_opt_params_all.Rd")


#-------------------------------------------------------------------------------
#visualize

#runout_polygons_conn <- runout_polygons[runout_polygons$connected == "Yes", ]
runout_polygons_conn <- runout_polygons@data[runout_polygons$connected == "Yes", ]

runout <- cbind(runout_polygons_conn, errorIndvConn(x, pcm_opt = pcm_connopt))
plot(runout, "connected")
hist(runout$error)

median(runout$relerr)

err_conn <- errorIndvConn(x[[1:21]], pcm_opt = pcm_connopt)

hist(errorIndvConn(x, pcm_opt = pcm_opt)$error)


hist(errorIndvConn(x, pcm_opt = pcm_opt)$error)

median(runout$auroc)
IQR(errorIndvConn(x, pcm_opt = pcm_opt)$error)

err_df <- rbind(data.frame(error = errorIndvConn(x, pcm_opt = pcm_connopt)$error,
                           opt = "connect"),
                data.frame(error = errorIndvConn(x, pcm_opt = pcm_opt)$error,
                           opt = "default"))

boxplot(error ~ opt, data = err_df)

IQR(errorIndvConn(x, pcm_opt = pcm_opt)$error)

# Plot Individual Simulations w Opt results ###################################
# visualize  what was going on, help with diskussion
