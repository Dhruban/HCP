#import
library(tidyverse)
library(gbm)

library(caTools)
library(ROCR)


dir.data <- "./Data" 
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"
dir.output <- paste0(dir.analysis,"/Age/output")

# Load Feature
load(paste0(dir.processing,"/Output/mean.RData"))
load(paste0(dir.processing,"/Output/sd.RData"))
load(paste0(dir.processing,"/Output/id.RData"))
load(paste0(dir.processing,"/Output/pacf1.RData"))
load(paste0(dir.processing,"/Output/pacf2.RData"))
load(paste0(dir.processing,"/Output/pacf3.RData"))
load(paste0(dir.processing,"/Output/acf2.RData"))
load(paste0(dir.processing,"/Output/acf3.RData"))
load(paste0(dir.processing,"/Output/acf4.RData"))
# load(paste0(dir.processing,"/Output/hurst_he.RData"))
# load(paste0(dir.processing,"/Output/yycor.RData"))
feature <- cbind(timeseries_mean, timeseries_sd, timeseries_pacf1, timeseries_pacf2, timeseries_pacf3, timeseries_acf2, timeseries_acf3, timeseries_acf4)
# feature <- cbind(timeseries_hurst_he)

# load dependent variable
unrestricted_elvisha <- read.csv(paste0(dir.data,"/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"))

elvisa_age <- unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>% # only subjects in feature
  select(Subject, Age, Gender)

elvisa_age$Gender[elvisa_age$Gender == 'M'] = 1
elvisa_age$Gender[elvisa_age$Gender == 'F'] = 0

elvisa_age$Age <- as.factor(elvisa_age$Age)
elvisa_age$Age <- as.numeric(elvisa_age$Age)

label <- as.numeric(elvisa_age$Age)
gender <- as.numeric(elvisa_age$Gender)
subject_id <- as.numeric(subject_id)

# normlize feature (if we want try this in future)
# feature = t(t(feature)/colSums(feature))

# standardize feature
feature = data.frame(feature)
feature = scale(feature)
feature = as.matrix(feature)

print("mean and standart error for crosscheck")
print(colMeans(feature))  # faster version of apply(scaled.dat, 2, mean)
print(apply(feature, 2, sd))


getdata <- function(index,splitratio,  male = FALSE, female = FALSE) {
  mydata <- cbind(subject_id, label, feature)
  if (male == TRUE) {
    mydata <- mydata[gender == 1,]
  }
  if (female == TRUE) {
    mydata <- mydata[gender == 0,]
  }
  # as 4th age category has very low number of data we are removing that catagory
  mydata=mydata[mydata[,2]!=4,]
  
  set.seed(seeds[index])
  split <- sample.split(mydata[,2], SplitRatio = splitratio)

  train_data <- subset(mydata, split == "TRUE")
  test_data <- subset(mydata, split == "FALSE")
  feature_train <- train_data[,3:dim(train_data)[2]]
  label_train <- train_data[,2]
  feature_test <- test_data[,3:dim(train_data)[2]]
  label_test <- test_data[,2]
  return (list("train_data"= train_data,"test_data" =test_data, "feature_train"=feature_train,"feature_test" =feature_test,"label_train"=label_train,"label_test"=label_test))
}

