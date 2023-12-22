library(tidyverse)
library(readxl)
library(writexl)
library(glmnet)
library(ROCR)
library(reshape2)
library(ggplot2)



mix<-1#1 for lasso, 0 for ridge
splitratio <- 0.8

dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"


dir.output <- paste0(dir.analysis,"/Age/output")
dir.temp <- paste0(dir.analysis,"/Age/temp")
dir.script <- paste0(dir.analysis,"/Age/script")

source(paste0(dir.script,"/99_functions.R"))

path.result <- paste0(dir.output,"/result.xlsx")

# if result does not exist create new
result <- list()

if (file.exists(path.result)){
  result_sheets<- as.character(excel_sheets(path.result))
  for( sheet_name in result_sheets){
    result[sheet_name] <- list(read_excel(path.result, sheet = sheet_name))
  }
  # result <- read.csv(path.result)
}

train_scores <- c()
test_scores <- c()

folder_name <- glue::glue("elastic_2class_split_{splitratio}_mix_{mix}")
folder_path <- paste0(dir.output,"/",folder_name)
dir.create(folder_path)

for (i in 1:2){
    # i = 1
    dataset <- getdata(i,splitratio, male = TRUE)
  
    feature_train = dataset$feature_train
    feature_test = dataset$feature_test
    label_train = dataset$label_train
    label_test = dataset$label_test
    train_data <- dataset$train_data
    test_data <- dataset$test_data
    
    cvglmnet <- cv.glmnet(feature_train, label_train,family= "multinomial", gamma = mix)
    myglmnet <- glmnet(feature_train, label_train ,family= "multinomial", alpha = mix,lambda = cvglmnet$lambda.1se )
    coef = coef(myglmnet, s =cvglmnet$lambda.1se )
    
    
    probs_train <- predict(myglmnet, feature_train, type = "response")
    predict_train <- numeric(length(label_train))
    for (i in 1: length(label_train)){
      predict_train[i] = which.max(probs_train[i,,1])
    }
    
    missing_classerr_train <- mean(predict_train != label_train)
    
    probs_test <- predict(myglmnet, feature_test, type = "response")
    predict_test <- numeric(length(label_test))
    for (i in 1: length(label_test)){
      predict_test[i] = which.max(probs_test[i,,1])
    }
    missing_classerr_test <- mean(predict_test != label_test)
    
    train_scores <- c(train_scores,1-missing_classerr_train)
    test_scores <- c(test_scores,1-missing_classerr_test)
    
}

mean_train = mean(train_scores)
sd_train = sd(train_scores)
mean_train_str = format(round(mean_train, 2), nsmall = 2)
sd_train_str = format(round(sd_train, 2), nsmall = 2)
result["accuracy"] <- add_entry(result["accuracy"],glue::glue("elastic(mix={mix})"), glue::glue("train_{splitratio}"), glue::glue("{mean_train_str} ({sd_train_str})"))

mean_test = mean(test_scores)
sd_test = sd(test_scores)
mean_test_str = format(round(mean_test, 2), nsmall = 2)
sd_test_str = format(round(sd_test, 2), nsmall = 2)
result["accuracy"] <- add_entry(result["accuracy"],glue::glue("elastic(mix={mix})"), glue::glue("test_{splitratio}"), glue::glue("{mean_test_str} ({sd_test_str})"))



# write temp files

# if we want to add timestamp
# nowtimestring =gsub(":","_",format(Sys.time(), "%Y_%b_%d_%X"))
# nowtimestring = glue::glue("{nowtimestring}_")
#otherwise
nowtimestring =""

train_accu_file_name = paste0(nowtimestring, glue::glue("train_accu_result.csv"))
train_accu_file_path = paste0(folder_path,"/",train_file_name )

test_accu_file_name = paste0(nowtimestring, glue::glue("test_accu_result.csv"))
test_accu_file_path = paste0(folder_path,"/",test_file_name )

write.csv(train_scores,train_accu_file_path )
write.csv(test_scores,test_accu_file_path )

write.csv(train_scores,train_file_path )
write.csv(test_scores,test_file_path )

# write result
# write.csv(result_sd_mean, path.result, row.names = FALSE)
write_xlsx(result, path.result)
write.csv(seeds,paste0(dir.output,"/seed.txt"),row.names = FALSE,col.names = FALSE)
