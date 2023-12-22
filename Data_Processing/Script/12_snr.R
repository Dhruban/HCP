library(reshape2)
library(ggplot2)
dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_analysis <- "./Analysis"

dir_output <- paste0(dir_analysis, "/Cognition/output")
dir_temp <- paste0(dir_analysis, "/Cognition/temp")


load(paste0(dir_processing, "/Output/mean.RData"))
load(paste0(dir_processing, "/Output/sd.RData"))
load(paste0(dir_processing, "/Output/pacf1.RData"))
load(paste0(dir_processing, "/Output/pacf2.RData"))
load(paste0(dir_processing, "/Output/pacf3.RData"))

snr <- timeseries_mean / timeseries_sd

save(snr, file = paste0(dir_processing, "/Output/snr.RData"))
