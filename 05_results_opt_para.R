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
pcm_connopt <- pcmGetConnOpt(pcm_gridsearch_multi, performance = "relerr", measure = "median", plot_opt = TRUE, from_save = FALSE)
pcm_connopt

errorIndvConn(pcm_gridsearch_multi, pcm_opt = pcm_connopt)