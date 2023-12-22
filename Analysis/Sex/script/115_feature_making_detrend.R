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

# # Load Feature
# load(paste0(dir.processing, "/Output/mean.RData"))
# load(paste0(dir.processing, "/Output/sd.RData"))
# load(paste0(dir.processing, "/Output/id.RData"))
# load(paste0(dir.processing, "/Output/pacf1.RData"))
# load(paste0(dir.processing, "/Output/pacf2.RData"))
# load(paste0(dir.processing, "/Output/pacf3.RData"))
# load(paste0(dir.processing, "/Output/acf2.RData"))
# load(paste0(dir.processing, "/Output/acf3.RData"))
# load(paste0(dir.processing, "/Output/acf4.RData"))
# load(paste0(dir.processing, "/Output/yycor.RData"))
# load(paste0(dir.processing, "/Output/snr.RData"))

# # Load avg Feature
# load(paste0(dir.processing, "/Output/mean_all.RData"))
# load(paste0(dir.processing, "/Output/sd_all.RData"))
# load(paste0(dir.processing, "/Output/subject_id_all.RData"))
# load(paste0(dir.processing, "/Output/pacf1_all.RData"))
# load(paste0(dir.processing, "/Output/pacf2_all.RData"))
# load(paste0(dir.processing, "/Output/pacf3_all.RData"))
# load(paste0(dir.processing, "/Output/acf2_all.RData"))
# load(paste0(dir.processing, "/Output/acf3_all.RData"))
# load(paste0(dir.processing, "/Output/acf4_all.RData"))
# load(paste0(dir.processing, "/Output/yycor_all.RData"))
# # load(paste0(dir.processing, "/Output/snr_all.RData"))

# Load Volume Matched Feature
load(paste0(dir.processing, "/Output/ts_detrend/mean.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/sd.RData"))
load(paste0(dir.processing, "/Output/subject_id_all.RData"))
# load(paste0(dir.processing, "/Output/filtered_id.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/pacf1.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/pacf2.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/pacf3.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/acf2.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/acf3.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/acf4.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/yy_cor.RData"))
load(paste0(dir.processing, "/Output/ts_detrend/snr.RData"))

feature <- c()

if ("mean" %in% xvars) {
  print("mean taken")
  feature <- cbind(feature, timeseries_mean)
}
if ("sd" %in% xvars) {
  print("sd taken")
  feature <- cbind(feature, timeseries_sd)
}
if ("pacf" %in% xvars) {
  print("pacf taken")
  feature <- cbind(
    feature, timeseries_pacf1, timeseries_pacf2,
    timeseries_pacf3
  )
}

if ("acf" %in% xvars) {
  print("acf taken")
  feature <- cbind(feature, timeseries_acf2, timeseries_acf3, timeseries_acf4)
}

if ("yycor" %in% xvars) {
  print("yycor taken")
  feature <- cbind(feature, yy_cor)
  # feature <- cbind(feature, yy.cor)
}

if ("snr" %in% xvars) {
  print("snr taken")
  feature <- cbind(feature, timeseries_snr)
}

# load dependent variable
unrestricted_elvisha <- read.csv(paste0(
  dir.data,
  "/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"
))

# subject_id <- setdiff(subject_id_all, filtered_id)
# subject_id <- filtered_id
subject_id <- subject_id_all

elvisa_gender <- unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>%
  # filter(Subject %in% subject_id_all) %>%
  # filter(Subject %in% filtered_id) %>%
  select(Subject, Gender)

## M = 1 , F = 0

elvisa_gender$Gender[elvisa_gender$Gender == "M"] <- 1
elvisa_gender$Gender[elvisa_gender$Gender == "F"] <- 0

label <- as.numeric(elvisa_gender$Gender)
# subject_id <- as.numeric(subject_id)
subject_id <- as.numeric(subject_id_all)
# subject_id <- as.numeric(filtered_id)
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



  return(list("train_data" = train_data,
   "test_data" = test_data,
   "feature_train" = feature_train,
   "feature_test" = feature_test,
   "label_train" = label_train,
   "label_test" = label_test))
}


# split <- sample.split(mydata[, 2], SplitRatio = 0.8)

# train_data <- subset(mydata, split == "TRUE")
# test_data <- subset(mydata, split == "FALSE")
# feature_train <- train_data[, 3:690]
# label_train <- train_data[, 2]
# feature_test <- test_data[, 3:690]
# label_test <- test_data[, 2]
# old_feature_train <- train_data[, 691:dim(train_data)[2]]
# old_feature_test <- test_data[, 691:dim(train_data)[2]]
