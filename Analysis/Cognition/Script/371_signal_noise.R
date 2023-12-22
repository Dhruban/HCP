library(reshape2)
library(ggplot2)
dir.data <- "./Data"
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"

dir.output <- paste0(dir.analysis, "/Cognition/output")
dir.temp <- paste0(dir.analysis, "/Cognition/temp")


load(paste0(dir.processing, "/Output/mean.RData"))
load(paste0(dir.processing, "/Output/sd.RData"))
load(paste0(dir.processing, "/Output/pacf1.RData"))
load(paste0(dir.processing, "/Output/pacf2.RData"))
load(paste0(dir.processing, "/Output/pacf3.RData"))

signal_noise_matrix <- timeseries_mean / timeseries_sd

corr_matrix <- array(0, dim = c(86, 3))

signal_noise_matrix_person <- signal_noise_matrix[1, ]
timeseries_pacf1_person <- timeseries_pacf1[1, ]
timeseries_pacf3_person <- timeseries_pacf3[1, ]
cor(signal_noise_matrix_person, timeseries_pacf1_person, method = "spearman")
plot(signal_noise_matrix_person, timeseries_pacf1_person)
yeo.volume <- as.vector(
    readMat(paste0(hcp.dir, "yeo/map_fs86_to_yeo7_weighted_sym.mat"))$roivol)
plot(yeo.volume, timeseries_pacf1_person)
cor(yeo.volume, timeseries_pacf1_person, method = "spearman")


timeseries_pacf1_person <- timeseries_pacf1[3, ]
timeseries_pacf3_person <- timeseries_pacf3[3, ]


plot(yeo.volume, timeseries_pacf3_person)
cor(yeo.volume, timeseries_pacf3_person, method = "spearman")

cor(signal_noise_matrix_person, timeseries_pacf3_person, method = "spearman")
plot(signal_noise_matrix_person, timeseries_pacf3_person)


cor(timeseries_pacf1_person, timeseries_pacf3_person, method = "spearman")
plot(timeseries_pacf1_person, timeseries_pacf3_person)

for (i in 1:86) {
    corr_matrix[i, 1] <- cor(signal_noise_matrix[, i], timeseries_pacf1[, i])
    corr_matrix[i, 2] <- cor(signal_noise_matrix[, i], timeseries_pacf2[, i])
    corr_matrix[i, 3] <- cor(signal_noise_matrix[, i], timeseries_pacf3[, i])
}
corr_array <- melt(corr_matrix)
colnames(corr_array) <- c("region", "lag", "corr")
ggplot(data = corr_array, aes(x = region, y = corr)) +
    geom_line(aes(colour = as.factor(lag)))
