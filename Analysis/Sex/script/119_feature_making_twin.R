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

# # Load first twin Feature
# load(paste0(dir.processing, "/Output/ts_first_twin/mean.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/sd.RData"))
# load(paste0(dir.processing, "/Output/first_twin_subject_id.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/pacf1.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/pacf2.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/pacf3.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/acf2.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/acf3.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/acf4.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/yy_cor.RData"))
# load(paste0(dir.processing, "/Output/ts_first_twin/snr.RData"))
# subject_id <- as.numeric(first_twin_subject_id)

# Load second twin Feature
load(paste0(dir.processing, "/Output/ts_second_twin/mean.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/sd.RData"))
load(paste0(dir.processing, "/Output/second_twin_subject_id.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/pacf1.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/pacf2.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/pacf3.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/acf2.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/acf3.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/acf4.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/yy_cor.RData"))
load(paste0(dir.processing, "/Output/ts_second_twin/snr.RData"))
subject_id <- as.numeric(second_twin_subject_id)

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
}

if ("zyycor" %in% xvars) {
  print("zyycor taken")
  yy_cor <- 0.5 * log((1 + yy_cor) / (1 - yy_cor))
  feature <- cbind(feature, yy_cor)
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

elvisa_gender <- unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>%
  select(Subject, Gender)

## M = 1 , F = 0

elvisa_gender$Gender[elvisa_gender$Gender == "M"] <- 1
elvisa_gender$Gender[elvisa_gender$Gender == "F"] <- 0

label <- as.numeric(elvisa_gender$Gender)
feature <- as.matrix(feature)

print("number of variable")
print(dim(feature)[2] / 86)
mydata <- cbind(subject_id, label, feature)

seeds <- sample.int(99999,10)

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
  
  print(paste0("Train data percentage= ",sum(as.numeric( split))*100/length(split)))
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