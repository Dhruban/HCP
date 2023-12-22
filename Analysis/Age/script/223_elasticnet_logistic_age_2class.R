library(tidyverse)
library(readxl)
library(writexl)
library(glmnet)
library(ROCR)
library(caTools)
library(reshape2)
library(ggplot2)
library(doParallel)
registerDoParallel(12)

dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"


dir.output <- paste0(dir.analysis,"/Age/output")
dir.temp <- paste0(dir.analysis,"/Age/temp")
dir.script <- paste0(dir.analysis,"/Age/script")

source(paste0(dir.script, "/99_functions.R"))

# path.result <- paste0(dir.output, glue::glue("/{variable}_result.xlsx"))

path_xvar_result <- paste0(dir.output, 
                           #glue::glue("/xvar_result.xlsx")
                           glue::glue("/overall_mean_age_result.xlsx"))

xvar_result <- list()

if (file.exists(path_xvar_result)) {
  xvar_result_sheets <- as.character(excel_sheets(path_xvar_result))
  for (sheet_name in xvar_result_sheets) {
    xvar_result[sheet_name] <- list(read_excel(path_xvar_result, sheet = sheet_name))
  }
}

x_var_name <- paste(xvars, sep = "_", collapse = "_")

folder_name <- glue::glue(
  "splitBy_{splitBy}/elastic/split_{splitratio}/mix_{mix}/{x_var_name}"
)
folder_path <- paste0(dir.temp, "/all_mean/", folder_name)
dir.create(folder_path)

train_scores_accu <- c()
test_scores_accu <- c()
train_scores_auc <- c()
test_scores_auc <- c()

all_coeffs <- array(0, dim = c(length(seeds), dim(feature)[2]))


for (i in 1:length(seeds)){
  print(i)
  dataset <- getdata_2class(i,splitratio)
  
  feature_train = dataset$feature_train
  feature_test = dataset$feature_test
  label_train = dataset$label_train
  label_test = dataset$label_test
  train_data <- dataset$train_data
  test_data <- dataset$test_data

  print("cvglmnet start")
  cvglmnet <- cv.glmnet(feature_train, label_train,family= binomial(link = "logit"), gamma = mix, parallel = TRUE)
  print("cvglmnet end")
  
  myglmnet <- glmnet(feature_train, label_train ,family= binomial(link = "logit"), alpha = mix,lambda = cvglmnet$lambda.1se )
  coef = coef(myglmnet, s =cvglmnet$lambda.1se )
  
  coef_num = as.numeric(coef)
  coef_beta= coef_num[-1]
  region_num= 86
  region_coef=matrix(coef_beta,nrow=region_num,byrow=FALSE)
  colnames(region_coef) <- paste0(1:dim(region_coef)[2])
  rownames(region_coef) <- paste0(1:dim(region_coef)[1])
  
  
  probs_train <- predict(myglmnet, feature_train, type = "response")
  ROCPred_train <- prediction(probs_train, label_train)
  ROCPer_train <- performance(ROCPred_train, measure = "tpr",
                             x.measure = "fpr")
  
  auc_train <- performance(ROCPred_train, measure = "auc")
  auc_train<- auc_train@y.values[[1]]
  
  png(glue::glue("{folder_path}/ROC_train_{i}.png"))
  plot(ROCPer_train, main="train_data")
  dev.off()
  
  predict_train <- ifelse(probs_train >0.5, 1, 0)
  print("train")
  print(table(label_train, predict_train))
  
  missing_classerr_train <- mean(predict_train != label_train)
  print(paste0(i,"-accu_tr-",1-missing_classerr_train))
  
  
  probs_test <- predict(myglmnet, feature_test, type = "response")
  ROCPred_test <- prediction(probs_test, label_test)
  ROCPer_test <- performance(ROCPred_test, measure = "tpr",
                        x.measure = "fpr")
  
  auc_test <- performance(ROCPred_test, measure = "auc")
  auc_test<- auc_test@y.values[[1]]
  print(paste0(i,"-",auc_test))
  
  png(glue::glue("{folder_path}/ROC_test_{i}.png"))
  plot(ROCPer_test, main="Test_data")
  dev.off()
  
  predict_test <- ifelse(probs_test >0.5, 1, 0)
  print("test")
  print(table(label_test, predict_test))
  
  missing_classerr_test <- mean(predict_test != label_test)
  print(paste0(i,"-accu_ts-",1-missing_classerr_test))
  
  absolute_coef <- abs(region_coef)
  longData<-melt(absolute_coef)
  # longData<-longData[longData$value!=0,]
  
  ggplot(longData, aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill=value)) +
    scale_fill_gradient(low="white", high="red") +
    labs(x="Featues", y="Regions", title="Absolute Coefficients") +
    theme_bw() + theme(axis.text.x=element_text(size=9, angle=0, vjust=0.3),
                       axis.text.y=element_text(size=9),
                       plot.title=element_text(size=11))
  ggsave(paste0(folder_path,glue::glue("/Coefficient_plot_{i}.pdf")))
  
  train_scores_accu <- c(train_scores_accu,1-missing_classerr_train)
  test_scores_accu <- c(test_scores_accu,1-missing_classerr_test)
  train_scores_auc <- c(train_scores_auc,auc_train)
  test_scores_auc <- c(test_scores_auc,auc_test)
  
}

save(all_coeffs, file = paste0(folder_path, "/all_coefficients.RData"))
save(train_scores_accu, file = paste0(folder_path, "/train_scores_accu.RData"))
save(test_scores_accu, file = paste0(folder_path, "/test_scores_accu.RData"))
save(train_scores_auc, file = paste0(folder_path, "/train_scores_auc.RData"))
save(test_scores_auc, file = paste0(folder_path, "/test_scores_auc.RData"))

mean_train_accu = mean(train_scores_accu)
sd_train_accu = sd(train_scores_accu)
mean_train_str_accu = format(round(mean_train_accu, 2), nsmall = 2)
sd_train_str_accu = format(round(sd_train_accu, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_accuracy_mean_sd")] <-
  add_entry(xvar_result[glue::glue("{splitBy}_{mix}_accuracy_mean_sd")],
            glue::glue(x_var_name),
            glue::glue("train_{splitratio}"),
            glue::glue("{mean_train_str_accu} ({sd_train_str_accu})"))

mean_test_accu = mean(test_scores_accu)
sd_test_accu = sd(test_scores_accu)
mean_test_str_accu = format(round(mean_test_accu, 2), nsmall = 2)
sd_test_str_accu = format(round(sd_test_accu, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_accuracy_mean_sd")] <-
  add_entry(xvar_result[glue::glue("{splitBy}_{mix}_accuracy_mean_sd")],
            glue::glue(x_var_name),
            glue::glue("test_{splitratio}"),
            glue::glue("{mean_test_str_accu} ({sd_test_str_accu})"))

mean_train_auc = mean(train_scores_auc)
sd_train_auc = sd(train_scores_auc)
mean_train_str_auc = format(round(mean_train_auc, 2), nsmall = 2)
sd_train_str_auc = format(round(sd_train_auc, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_auc_mean_sd")] <-
  add_entry(xvar_result[glue::glue("{splitBy}_{mix}_auc_mean_sd")],
            glue::glue(x_var_name),
            glue::glue("train_{splitratio}"),
            glue::glue("{mean_train_str_auc} ({sd_train_str_auc})"))


mean_test_auc = mean(test_scores_auc)
sd_test_auc = sd(test_scores_auc)
mean_test_str_auc = format(round(mean_test_auc, 2), nsmall = 2)
sd_test_str_auc = format(round(sd_test_auc, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_auc_mean_sd")] <-
  add_entry(xvar_result[glue::glue("{splitBy}_{mix}_auc_mean_sd")],
            glue::glue(x_var_name),
            glue::glue("test_{splitratio}"),
            glue::glue("{mean_test_str_auc} ({sd_test_str_auc})"))



median_train_accu = median(train_scores_accu)
iqr_train_accu = IQR(train_scores_accu)
median_train_str_accu = format(round(median_train_accu, 2), nsmall = 2)
iqr_train_str_accu = format(round(iqr_train_accu, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_accuracy_median_iqr")] <-
  add_entry(xvar_result[glue::glue("{splitBy}_{mix}_accuracy_median_iqr")],
            glue::glue(x_var_name),
            glue::glue("train_{splitratio}"),
            glue::glue("{median_train_str_accu} ({iqr_train_str_accu})"))

median_test_accu = median(test_scores_accu)
iqr_test_accu = IQR(test_scores_accu)
median_test_str_accu = format(round(median_test_accu, 2), nsmall = 2)
iqr_test_str_accu = format(round(iqr_test_accu, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_accuracy_median_iqr")] <-
  add_entry(xvar_result[glue::glue("{splitBy}_{mix}_accuracy_median_iqr")],
            glue::glue(x_var_name),
            glue::glue("test_{splitratio}"),
            glue::glue("{median_test_str_accu} ({iqr_test_str_accu})"))

median_train_auc = median(train_scores_auc)
iqr_train_auc = IQR(train_scores_auc)
median_train_str_auc = format(round(median_train_auc, 2), nsmall = 2)
iqr_train_str_auc = format(round(iqr_train_auc, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_auc_median_iqr")] <-
  add_entry(xvar_result[glue::glue("{splitBy}_{mix}_auc_median_iqr")],
            glue::glue(x_var_name),
            glue::glue("train_{splitratio}"),
            glue::glue("{median_train_str_auc} ({iqr_train_str_auc})"))

median_test_auc = median(test_scores_auc)
iqr_test_auc = IQR(test_scores_auc)
median_test_str_auc = format(round(median_test_auc, 2), nsmall = 2)
iqr_test_str_auc = format(round(iqr_test_auc, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_auc_median_iqr")] <-
  add_entry(xvar_result[glue::glue("{splitBy}_{mix}_auc_median_iqr")],
            glue::glue(x_var_name),
            glue::glue("test_{splitratio}"),
            glue::glue("{median_test_str_auc} ({iqr_test_str_auc})"))

# write temp files

# if we want to add timestamp
# nowtimestring =gsub(":","_",format(Sys.time(), "%Y_%b_%d_%X"))
# nowtimestring = glue::glue("{nowtimestring}_")
#otherwise
nowtimestring =""

train_accu_file_name = paste0(nowtimestring, glue::glue("train_accu_result.csv"))
train_accu_file_path = paste0(folder_path,"/",train_accu_file_name )
train_auc_file_name = paste0(nowtimestring, glue::glue("train_auc_result.csv"))
train_auc_file_path = paste0(folder_path,"/",train_auc_file_name)

test_accu_file_name = paste0(nowtimestring, glue::glue("test_accu_result.csv"))
test_accu_file_path = paste0(folder_path,"/",test_accu_file_name )
test_auc_file_name = paste0(nowtimestring, glue::glue("test_auc_result.csv"))
test_auc_file_path = paste0(folder_path,"/",test_auc_file_name )

write.csv(train_scores_accu,train_accu_file_path )
write.csv(test_scores_accu,test_accu_file_path )
write.csv(train_scores_auc,train_auc_file_path )
write.csv(test_scores_auc,test_auc_file_path )

# write result
# write.csv(result_sd_mean, path.result, row.names = FALSE)
write_xlsx(xvar_result, path_xvar_result)
write.csv(seeds,paste0(dir.output,"/seed.txt"),row.names = FALSE,col.names = FALSE)

