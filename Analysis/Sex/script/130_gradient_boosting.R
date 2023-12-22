#import
library(tidyverse)
library(gbm)

library(caTools)
library(ROCR)
# Load Feature
load("./mean.RData")
load("./sd.RData")
load("./id.RData")
load("./pacf1.RData")
load("./pacf2.RData")
load("./pacf3.RData")
load("./acf2.RData")
load("./acf3.RData")
load("./acf4.RData")

feature <- cbind(timeseries_mean, timeseries_sd, timeseries_pacf1, timeseries_pacf2, timeseries_pacf3, timeseries_acf2, timeseries_acf3, timeseries_acf4)
# feature <- cbind(timeseries_mean, timeseries_sd, timeseries_pacf1)
# feature <- cbind(timeseries_pacf1)

# load dependent variable
# unrestricted_elvisha <- read.csv("./unrestricted_elvisha9_3_9_2021_10_37_53.csv")

elvisa_gender <- unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>%
  select(Subject, Gender)

## M = 1 , F = 0

elvisa_gender$Gender[elvisa_gender$Gender == 'M'] = 1
elvisa_gender$Gender[elvisa_gender$Gender == 'F'] = 0

label <- as.numeric(elvisa_gender$Gender)
subject_id <- as.numeric(subject_id)
mydata <- cbind(subject_id, label, feature)
split <- sample.split(mydata[,2], SplitRatio = 0.5)

train_data <- subset(mydata, split == "TRUE")
test_data <- subset(mydata, split == "FALSE")
feature_train <- train_data[,3:dim(train_data)[2]]
label_train <- train_data[,2]
feature_test <- test_data[,3:dim(train_data)[2]]
label_test <- test_data[,2]



train_data <-  train_data[,2:dim(train_data)[2]]
test_data <-  test_data[,2:dim(test_data)[2]]

train_data <- data.frame(train_data)
test_data <- data.frame(test_data)
test_feature <- test_data[, -label]
train_feature <- train_data[, -label]

mygbm <- gbm(formula = label~., distribution = "bernoulli", data = train_data, n.trees = 1000, cv.folds = 20)

#coef = coef(myglmnet, s =cvglmnet$lambda.min ) 
#coef_num = as.numeric(coef)
#coef_num = coef_num[2:259]

ntree_opt_cv <- gbm.perf(mygbm, method = "cv")

probs <- predict(mygbm, test_feature, type = "response")
ROCPred <- prediction(probs, label_test)
ROCPer <- performance(ROCPred, measure = "tpr",
                      x.measure = "fpr")

auc <- performance(ROCPred, measure = "auc")
auc <- auc@y.values[[1]]
print(auc)
plot(ROCPer)

predict_test <- ifelse(probs >0.5, 1, 0)
table(label_test, predict_test)

missing_classerr <- mean(predict_test != label_test)
print(paste('Accuracy =', 1 - missing_classerr))
