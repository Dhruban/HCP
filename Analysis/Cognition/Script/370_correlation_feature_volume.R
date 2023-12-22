dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_analysis <- "./Analysis"

dir_output <- paste0(dir_analysis, "/Cognition/output")
dir_temp <- paste0(dir_analysis, "/Cognition/temp")

library(R.matlab)
hcp_dir <- "./Data/Elvisha/"
yeo_volume <- as.vector(readMat(paste0(
    hcp_dir,
    "yeo/map_fs86_to_yeo7_weighted_sym.mat"
))$roivol)

load(paste0(dir_processing, "/Output/pacf1.RData"))
load(paste0(dir_processing, "/Output/pacf2.RData"))
load(paste0(dir_processing, "/Output/pacf3.RData"))

absolute_p1 <- abs(timeseries_pacf1)
absolute_p2 <- abs(timeseries_pacf2)
absolute_p3 <- abs(timeseries_pacf3)

mean_p1 <- apply(absolute_p1, 2, FUN = function(v) {
    return(mean(v))
})
mean_p2 <- apply(absolute_p2, 2, FUN = function(v) {
    return(mean(v))
})
mean_p3 <- apply(absolute_p3, 2, FUN = function(v) {
    return(mean(v))
})

print(paste0(
    "Correlation between volume and pacf lag 1--> ",
    cor(yeo_volume, mean_p1)
))
hist(yeo_volume)
hist(mean_p1)
plot(yeo_volume, mean_p1)
print(paste0(
    "Correlation between volume and pacf lag 2--> ",
    cor(yeo_volume, mean_p2)
))
print(paste0(
    "Correlation between volume and pacf lag 3--> ",
    cor(yeo_volume, mean_p3)
))
