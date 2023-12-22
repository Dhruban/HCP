### feature making with individual mean of four scan

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

load(paste0(dir.processing, "/Output/subject_id_all.RData"))

# Load Feature
load(paste0(dir.processing, "/Output/mean_all.RData"))
load(paste0(dir.processing, "/Output/sd_avarage.RData"))

load(paste0(dir.processing, "/Output/pacf1_avarage.RData"))
load(paste0(dir.processing, "/Output/pacf2_avarage.RData"))
load(paste0(dir.processing, "/Output/pacf3_avarage.RData"))
load(paste0(dir.processing, "/Output/acf2_avarage.RData"))
load(paste0(dir.processing, "/Output/acf3_avarage.RData"))
load(paste0(dir.processing, "/Output/acf4_avarage.RData"))
load(paste0(dir.processing, "/Output/yycor_avarage.RData"))


feature <- c()

if ("mean" %in% xvars) {
  print("mean taken")
  feature <- cbind(
    feature, timeseries_mean_arr[1, , ], timeseries_mean_arr[2, , ],
    timeseries_mean_arr[3, , ], timeseries_mean_arr[4, , ]
  )
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
  print("pacf taken")
  feature <- cbind(feature, yy.cor)
}

if ("snr" %in% xvars) {
  print("snr taken")
  feature <- cbind(feature, snr)
}

if ("period" %in% xvars) {
  print("period taken")
  feature <- cbind(feature, timeseries_period_chunks)
}
if ("acf_diff" %in% xvars) {
  print("acf diff taken")
  feature <- cbind(
    feature,
    acf2_diff3, acf3_diff3, acf2_diff4, acf3_diff4
  )
}


# load dependent variable
unrestricted_elvisha <- read.csv(paste0(
  dir.data,
  "/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"
))

print(paste("variable", variable))

elvisa_cognition <- unrestricted_elvisha %>%
  filter(Subject %in% subject_id_all) %>% # only subjects in feature
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
feature <- data.frame(feature)
feature <- scale(feature)
feature <- as.matrix(feature)

# print("mean and standart error for crosscheck")
# print(colMeans(feature)) # faster version of apply(scaled.dat, 2, mean)
# print(apply(feature, 2, sd))

print("number of variable")
print(dim(feature)[2] / 86)


mydata <- cbind(new_subject_id, label, feature)

variable <- paste0(variable, "_mean")

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
