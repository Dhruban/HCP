# import
library(tidyverse)
library(gbm)
library(caTools)
library(ROCR)
library(dplyr)
library(reshape2)
library(MuMIn)
library(lme4)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_analysis <- "./Analysis"
dir_output <- paste0(dir_analysis, "/Age/output")

load(paste0(dir_processing, "/Output/mean.RData"))
load(paste0(dir_processing, "/Output/sd.RData"))
load(paste0(dir_processing, "/Output/id.RData"))
load(paste0(dir_processing, "/Output/pacf1.RData"))
load(paste0(dir_processing, "/Output/pacf2.RData"))
load(paste0(dir_processing, "/Output/pacf3.RData"))
load(paste0(dir_processing, "/Output/acf2.RData"))
load(paste0(dir_processing, "/Output/acf3.RData"))
load(paste0(dir_processing, "/Output/acf4.RData"))
load(paste0(dir_processing, "/Output/hurst_he.RData"))
load(paste0(dir_processing, "/Output/yycor.RData"))
load(paste0(dir_processing, "/Output/snr.RData"))

pacf1_frame <- data.frame(timeseries_pacf1)
pacf1_frame <- as.matrix(scale(pacf1_frame))
# print(colMeans(pacf1_frame))
melt_pacf1 <- melt(data.frame(t(pacf1_frame)))
colnames(melt_pacf1) <- c("person", "pacf1")


hurst_frame <- data.frame(timeseries_hurst_he)
hurst_frame <- as.matrix(scale(hurst_frame))
# print(colMeans(hurst_frame))
melt_hurst <- melt(data.frame(t(hurst_frame)))
colnames(melt_hurst) <- c("person", "hurst")


snr_frame <- data.frame(snr)
snr_frame <- as.matrix(scale(snr_frame))
# print(colMeans(snr_frame))
melt_snr <- melt(data.frame(t(snr_frame)))
colnames(melt_snr) <- c("person", "snr")

dataframe <- cbind(melt_pacf1, melt_hurst[, 2], melt_snr[, 2])

colnames(dataframe) <- c("person", "pacf1", "hurst", "snr")

fm1 <- lmer(pacf1 ~ hurst + snr + (hurst | person) + (snr | person), dataframe)

r.squaredGLMM(fm1)


pacf2_frame <- data.frame(timeseries_pacf2)
pacf2_frame <- as.matrix(scale(pacf2_frame))
# print(colMeans(pacf1_frame))
melt_pacf2 <- melt(data.frame(t(pacf2_frame)))
colnames(melt_pacf2) <- c("person", "pacf2")


hurst_frame <- data.frame(timeseries_hurst_he)
hurst_frame <- as.matrix(scale(hurst_frame))
# print(colMeans(hurst_frame))
melt_hurst <- melt(data.frame(t(hurst_frame)))
colnames(melt_hurst) <- c("person", "hurst")


snr_frame <- data.frame(snr)
snr_frame <- as.matrix(scale(snr_frame))
# print(colMeans(snr_frame))
melt_snr <- melt(data.frame(t(snr_frame)))
colnames(melt_snr) <- c("person", "snr")

dataframe <- cbind(melt_pacf2, melt_hurst[, 2], melt_snr[, 2])

colnames(dataframe) <- c("person", "pacf2", "hurst", "snr")

fm2 <- lmer(pacf2 ~ hurst + snr + (hurst | person) + (snr | person), dataframe)

r.squaredGLMM(fm2)


pacf3_frame <- data.frame(timeseries_pacf2)
pacf3_frame <- as.matrix(scale(pacf3_frame))
# print(colMeans(pacf1_frame))
melt_pacf3 <- melt(data.frame(t(pacf3_frame)))
colnames(melt_pacf3) <- c("person", "pacf3")


hurst_frame <- data.frame(timeseries_hurst_he)
hurst_frame <- as.matrix(scale(hurst_frame))
# print(colMeans(hurst_frame))
melt_hurst <- melt(data.frame(t(hurst_frame)))
colnames(melt_hurst) <- c("person", "hurst")


snr_frame <- data.frame(snr)
snr_frame <- as.matrix(scale(snr_frame))
# print(colMeans(snr_frame))
melt_snr <- melt(data.frame(t(snr_frame)))
colnames(melt_snr) <- c("person", "snr")

dataframe <- cbind(melt_pacf3, melt_hurst[, 2], melt_snr[, 2])

colnames(dataframe) <- c("person", "pacf3", "hurst", "snr")

fm3 <- lmer(pacf3 ~ hurst + snr + (hurst | person) + (snr | person), dataframe)

r.squaredGLMM(fm3)
