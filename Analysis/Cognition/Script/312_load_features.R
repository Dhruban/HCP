dir_processing <- "./Data_Processing"
dir_analysis <- "./Analysis"

# Load Feature
load(paste0(dir_processing, "/Output/mean.RData"))
load(paste0(dir_processing, "/Output/sd.RData"))
load(paste0(dir_processing, "/Output/id.RData"))
load(paste0(dir_processing, "/Output/pacf1.RData"))
load(paste0(dir_processing, "/Output/pacf2.RData"))
load(paste0(dir_processing, "/Output/pacf3.RData"))
load(paste0(dir_processing, "/Output/acf2.RData"))
load(paste0(dir_processing, "/Output/acf3.RData"))
load(paste0(dir_processing, "/Output/acf4.RData"))
# load(paste0(dir.processing,"/Output/hurst_he.RData"))
load(paste0(dir_processing, "/Output/yycor.RData"))
load(paste0(dir_processing, "/Output/snr.RData"))
load(paste0(dir_processing, "/Output/region_volume.RData"))



load(paste0(dir_processing, "/Output/acf3_diff4.RData"))
load(paste0(dir_processing, "/Output/acf2_diff1.RData"))
load(paste0(dir_processing, "/Output/acf3_diff1.RData"))
load(paste0(dir_processing, "/Output/acf2_diff2.RData"))
load(paste0(dir_processing, "/Output/acf3_diff2.RData"))
load(paste0(dir_processing, "/Output/acf2_diff3.RData"))
load(paste0(dir_processing, "/Output/acf3_diff3.RData"))
load(paste0(dir_processing, "/Output/acf2_diff4.RData"))
load(paste0(dir_processing, "/Output/acf3_diff4.RData"))
