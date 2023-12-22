# import
library(tidyverse)
library(dplyr)
library(tibble)
library(cli)
library(gbm)
library(caTools)
library(ROCR)


dir.data <- "./Data"
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"
dir.output <- paste0(dir.analysis, "/Sex/Output")

# Load avg Feature
load(paste0(dir.processing, "/Output/mean_all.RData"))
load(paste0(dir.processing, "/Output/sd_all.RData"))
load(paste0(dir.processing, "/Output/subject_id_all.RData"))
load(paste0(dir.processing, "/Output/pacf1_all.RData"))
load(paste0(dir.processing, "/Output/pacf2_all.RData"))
load(paste0(dir.processing, "/Output/pacf3_all.RData"))
load(paste0(dir.processing, "/Output/acf2_all.RData"))
load(paste0(dir.processing, "/Output/acf3_all.RData"))
load(paste0(dir.processing, "/Output/acf4_all.RData"))
load(paste0(dir.processing, "/Output/yycor_all.RData"))
# load(paste0(dir.processing, "/Output/region_volume.RData"))
# load(paste0(dir.processing, "/Output/snr_all.RData"))

subject_id <- as.numeric(subject_id_all)

feature <- c()
if ("mean" %in% xvars) {
  print("mean taken")
  feature <- cbind(feature, timeseries_mean_arr[1,,])
}
if ("sd" %in% xvars) {
  print("sd taken")
  feature <- cbind(feature, timeseries_sd_arr[1,,])
}
if ("pacf" %in% xvars) {
  print("pacf taken")
  feature <- cbind(
    feature, timeseries_pacf1_arr[1,,], timeseries_pacf2_arr[1,,],
    timeseries_pacf3_arr[1,,]
  )
}

if ("acf" %in% xvars) {
  print("acf taken")
  feature <- cbind(feature, timeseries_acf2_arr[1,,], timeseries_acf3_arr[1,,], timeseries_acf4_arr[1,,])
}

if ("yycor" %in% xvars) {
  print("yycor taken")
  feature <- cbind(feature, yy_cor_arr[1,,])
}

if ("zyycor" %in% xvars) {
  print("zyycor taken")
  yy_cor <- 0.5 * log((1 + yy_cor_arr[1,,]) / (1 - yy_cor_arr[1,,]))
  feature <- cbind(feature, yy_cor)
}

if ("snr" %in% xvars) {
  print("snr taken")
  feature <- cbind(feature, timeseries_mean_arr[1,,]/timeseries_sd_arr[1,,])
}


# load dependent variable
unrestricted_elvisha <- read.csv(paste0(
  dir.data,
  "/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"
))

elvisa_gender <- unrestricted_elvisha %>%
  # filter(Subject %in% subject_id) %>%
  filter(Subject %in% subject_id_all) %>%
  select(Subject, Gender)

## M = 1 , F = 0

elvisa_gender$Gender[elvisa_gender$Gender == "M"] <- 1
elvisa_gender$Gender[elvisa_gender$Gender == "F"] <- 0

label <- as.numeric(elvisa_gender$Gender)
mydata <- cbind(subject_id, label, feature)

getdata <- function(index, splitratio) {
  set.seed(seeds[index])
  if (splitBy == "Normal") {
    split <- sample.split(mydata[, 2], SplitRatio = splitratio)
  } else if (splitBy == "Combined") {
    split <- sample.split(Combined, SplitRatio = splitratio)
  } else if (splitBy == "Gender") {
    split <- sample.split(Gender, SplitRatio = splitratio)
  } else if (splitBy == "Age") {
    split <- sample.split(Age, SplitRatio = splitratio)
  }
  train_data <- subset(mydata, split == "TRUE")
  test_data <- subset(mydata, split == "FALSE")
  feature_train <- train_data[, 3:dim(train_data)[2]]
  label_train <- train_data[, 2]
  feature_test <- test_data[, 3:dim(train_data)[2]]
  label_test <- test_data[, 2]



  return(list(
    "train_data" = train_data,
    "test_data" = test_data,
    "feature_train" = feature_train,
    "feature_test" = feature_test,
    "label_train" = label_train,
    "label_test" = label_test
  ))
}

