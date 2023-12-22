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


# Load ar Feature
load(paste0(dir.processing, "/Output/id.RData"))
load(paste0(dir.processing, "/Output/ar_coeff_6.RData"))  #86 ar1,...,ar6, 86 intercept, 86 error
load(paste0(dir.processing, "/Output/yycor.RData"))
# load(paste0(dir.processing, "/Output/region_volume.RData"))

feature <- c()

if ("intercept" %in% xvars) {
  print("intercept taken")
  new_feature <- tsorder[,(6*86+1):(7*86)]
  feature <- cbind(feature, new_feature)
}
if ("residual_sd" %in% xvars) {
  print("residual_sd taken")
  new_feature <- tsorder[,(7*86+1):(8*86)]
  feature <- cbind(feature, new_feature)
}
if ("ar_coef" %in% xvars) {
  print("ar_coef taken")
  new_feature <- tsorder[,1:(6*86)]
  feature <- cbind(feature, new_feature)
}

if ("yycor" %in% xvars) {
  print("yycor taken")
  feature <- cbind(feature, yy.cor)
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
subject_id <- as.numeric(subject_id)
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

