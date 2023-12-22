library(tidyverse)
library(gbm)
library(readxl)
library(writexl)
library(caTools)
library(ROCR)

dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"

dir.script <- paste0(dir.analysis,"/Cognition/Script")
dir.output <- paste0(dir.analysis,"/Cognition/Output")
path.result <- paste0(dir.output,"/Result.xlsx")

source(paste0(dir.script,"/99_functions.R"))

splitratio <- 0.8

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

for (i in 1:10){
  dataset <- getdata(i,splitratio)
  train_data <- dataset$train_data
  test_data <- dataset$test_data
  label_train <- dataset$label_train
  label_test <- dataset$label_test
  
  train_data <-  train_data[,2:dim(train_data)[2]]
  test_data <-  test_data[,2:dim(test_data)[2]]
  train_data <- data.frame(train_data)
  test_data <- data.frame(test_data)
  train_feature <- train_data[, -1]
  test_feature <- test_data[, -1]
  
  mygbm <- gbm:::gbm(formula = label~., data = train_data, n.trees = 500, cv.folds = 20)
  ntree_opt_cv <- gbm.perf(mygbm, method = "cv")
  
  pred_train <- predict(mygbm, train_feature, type = "response")
  pred_test <- predict(mygbm, test_feature, type = "response")

  train_scores <- c(train_scores,R2_Score(pred_train,label_train))
  test_scores <- c(test_scores, R2_Score(pred_test,label_test))
  
  print("train")
  print(train_scores)
  print("test")
  print(test_scores)
  print(paste0("done",i));
  
}

nowtimestring =gsub(":","_",format(Sys.time(), "%Y_%b_%d_%X"))
train_file_name = paste0(nowtimestring, glue::glue("_train_{splitratio}_gredient_boosting.csv"))
train_file_path = paste0(dir.temp,"/",train_file_name )


test_file_name = paste0(nowtimestring, glue::glue("_test_{splitratio}_gredient_boosting.csv"))
test_file_path = paste0(dir.temp,"/",test_file_name )

mean_train = mean(train_scores)
sd_train = sd(train_scores)
mean_train_str = format(round(mean_train, 2), nsmall = 2)
sd_train_str = format(round(sd_train, 2), nsmall = 2)
result["mean_sd"] <- add_entry(result["mean_sd"],glue::glue("gredient_boosting"), glue::glue("train_{splitratio}"), glue::glue("{mean_train_str} ({sd_train_str})"))

mean_test = mean(test_scores)
sd_test = sd(test_scores)
mean_test_str = format(round(mean_test, 2), nsmall = 2)
sd_test_str = format(round(sd_test, 2), nsmall = 2)
result["mean_sd"] <- add_entry(result["mean_sd"],glue::glue("gredient_boosting"), glue::glue("test_{splitratio}"), glue::glue("{mean_test_str} ({sd_test_str})"))

median_train = median(train_scores)
iqr_train = IQR(train_scores)
median_train_str = format(round(median_train, 2), nsmall = 2)
iqr_train_str = format(round(iqr_train, 2), nsmall = 2)
result["median_iqr"] <- add_entry(result["median_iqr"],glue::glue("gredient_boosting"), glue::glue("train_{splitratio}"), glue::glue("{median_train_str} ({iqr_train_str})"))

median_test = median(test_scores)
iqr_test = IQR(test_scores)
median_test_str = format(round(median_test, 2), nsmall = 2)
iqr_test_str = format(round(iqr_test, 2), nsmall = 2)
result["median_iqr"] <- add_entry(result["median_iqr"],glue::glue("gredient_boosting"), glue::glue("test_{splitratio}"), glue::glue("{median_test_str} ({iqr_test_str})"))


# write temp files
write.csv(train_scores,train_file_path )
write.csv(test_scores,test_file_path )

# write result
# write.csv(result_sd_mean, path.result, row.names = FALSE)
write_xlsx(result, path.result)
