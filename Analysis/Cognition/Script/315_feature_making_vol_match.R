# import
library(tidyverse)
library(gbm)
library(caTools)
library(ROCR)



# for Sayan
dir.data <- "./Data"
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"
dir.output <- paste0(dir.analysis, "/Cognition/Output")

load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/mean.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/sd.RData"))
# load(paste0(dir.processing, "/Output/subject_id_all.RData"))
load(paste0(dir.processing, "/Output/filtered_id.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/pacf1.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/pacf2.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/pacf3.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/acf2.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/acf3.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/acf4.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/yy_cor.RData"))
load(paste0(dir.processing, "/Output/timeseries_volume_matched_diff/snr.RData"))

# load(paste0(dir.processing, "/Output/timeseries_volume_matched/chatterjee_acf1.RData"))
# load(paste0(dir.processing, "/Output/timeseries_volume_matched/chatterjee_acf2.RData"))
# load(paste0(dir.processing, "/Output/timeseries_volume_matched/chatterjee_acf3.RData"))



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

if ("chatterjee_acf" %in% xvars) {
  print("chatterjee_acf taken")
  feature <- cbind(feature, chatterjee_acf1, chatterjee_acf2, chatterjee_acf3)
}


# load dependent variable
unrestricted_elvisha <- read.csv(paste0(
  dir.data,
  "/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"
))

print(paste("variable", variable))


subject_id <- filtered_id
# subject_id <- setdiff(subject_id_all, filtered_id)

elvisa_cognition <- unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>% # only subjects in feature
  select(Subject, eval(variable), Age, Gender)

elvisa_cognition$Gender[elvisa_cognition$Gender == "M"] <- 1
elvisa_cognition$Gender[elvisa_cognition$Gender == "F"] <- 0
elvisa_cognition$Gender <- as.numeric(elvisa_cognition$Gender)

elvisa_cognition$Age <- as.factor(elvisa_cognition$Age)
elvisa_cognition$Age <- as.numeric(elvisa_cognition$Age)

elvisa_cognition <- elvisa_cognition %>% mutate(
  combined = (10 * Age) + Gender
)
elvisa_cognition$combined <- as.factor(elvisa_cognition$combined)
elvisa_cognition$combined <- as.numeric(elvisa_cognition$combined)

# removing subjects that do not have cognition scores
nonnanindex <- !is.na(elvisa_cognition[, 2])

label <- as.numeric(elvisa_cognition[, 2])
new_subject_id <- as.numeric(elvisa_cognition$Subject)

Combined <- as.numeric(elvisa_cognition$Combined)
Combined <- Combined[nonnanindex]

Gender <- as.numeric(elvisa_cognition$Gender)
Gender <- Gender[nonnanindex]

Age <- as.numeric(elvisa_cognition$Age)
Age <- Gender[nonnanindex]

label <- label[nonnanindex]
new_subject_id <- new_subject_id[nonnanindex]
feature <- feature[nonnanindex, ]

# normlize feature (if we want try this in future)
# feature = t(t(feature)/colSums(feature))

# standardize feature

# feature <- data.frame(feature)
# feature <- scale(feature)
feature <- as.matrix(feature)

# print("mean and standart error for crosscheck")
# print(colMeans(feature)) # faster version of apply(scaled.dat, 2, mean)
# print(apply(feature, 2, sd))

print("number of variable")
print(dim(feature)[2] / 86)


mydata <- cbind(new_subject_id, label, feature)

variable <- paste0(variable, "_volume_match")

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
  
  
  
  return(list("train_data" = train_data, "test_data" = test_data, "feature_train" = feature_train, "feature_test" = feature_test, "label_train" = label_train, "label_test" = label_test))
}
