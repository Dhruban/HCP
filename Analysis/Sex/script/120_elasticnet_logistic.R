library(tidyverse)
library(readxl)
library(writexl)
library(glmnet)
library(ROCR)
library(caTools)
library(reshape2)
library(ggplot2)
library(doParallel)

dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"
dir.output <- paste0(dir.analysis, "/Sex/output")
dir.temp <- paste0(dir.analysis, "/Sex/temp")
dir.script <- paste0(dir.analysis, "/Sex/script")
source(paste0(dir.script, "/99_functions.R"))

path_xvar_result <- paste0(
  dir.output,
  # glue::glue("/xvar_result.xlsx")
  # glue::glue("/volume_matched_sex_result.xlsx")
  # glue::glue("/detrend_sex_result.xlsx")
  # glue::glue("/ar_coef_sex_result.xlsx")
  # glue::glue("/family_split_sex_result.xlsx")
  # glue::glue("/1st_twin_sex_result.xlsx")
  glue::glue("/2nd_twin_sex_result.xlsx")
)

xvar_result <- list()

if (file.exists(path_xvar_result)) {
  xvar_result_sheets <- as.character(excel_sheets(path_xvar_result))
  for (sheet_name in xvar_result_sheets) {
    xvar_result[sheet_name] <- list(read_excel(path_xvar_result, sheet = sheet_name))
  }
}

# x_var_name <- paste(xvars, sep = "_", collapse = "_")

folder_name <- glue::glue(
  "splitBy_{splitBy}/elastic/split_{splitratio}/mix_{mix}/{x_var_name}"
)

# folder_path <- paste0(dir.temp, "/", folder_name)
# folder_path <- paste0(dir.temp, "/volume_matched/", folder_name)
# folder_path <- paste0(dir.temp, "/detrend/", folder_name)
# folder_path <- paste0(dir.temp, "/ar_coef/", folder_name)
# folder_path <- paste0(dir.temp, "/family_split_transition/", folder_name)
# folder_path <- paste0(dir.temp, "/family_split_transition_twin/", folder_name)
folder_path <- paste0(dir.temp, "/second_twin_fixed/", folder_name)
# folder_path <- paste0(dir.temp, "/first_twin/", folder_name)
# folder_path <- paste0(dir.temp, "/second_twin/", folder_name)

dir.create(folder_path, recursive = TRUE)

train_scores_accu <- c()
test_scores_accu <- c()
train_scores_auc <- c()
test_scores_auc <- c()

# ratio <- c()

all_coeffs <- array(0, dim = c(length(seeds), dim(feature)[2]))

for (i in 1:length(seeds)) {
  print(i)
  dataset <- getdata(i, splitratio)

  feature_train <- dataset$feature_train
  feature_test <- dataset$feature_test
  label_train <- dataset$label_train
  label_test <- dataset$label_test
  train_data <- dataset$train_data
  test_data <- dataset$test_data
  # ratio<- c(ratio , dataset$twin_size/ (dataset$twin_size+dataset$non_twin_size))

  print("cvglmnet start")
  c1 <- makeCluster(7)
  registerDoParallel(c1)
  cvglmnet <- cv.glmnet(feature_train,
    label_train,
    family = binomial(link = "logit"),
    gamma = mix,
    parallel = TRUE
  )
  stopCluster(c1)
  print("cvglmnet end")
  myglmnet <- glmnet(feature_train,
    label_train,
    family = binomial(link = "logit"),
    alpha = mix,
    lambda = cvglmnet$lambda.1se
  )

  coef <- coef(myglmnet, s = cvglmnet$lambda.1se)
  coef_num <- as.numeric(coef)
  coef_beta <- coef_num[-1]
  all_coeffs[i, ] <- coef_beta
  region_num <- 86
  region_coef <- matrix(coef_beta, nrow = region_num, byrow = FALSE)
  colnames(region_coef) <- paste0(1:dim(region_coef)[2])
  rownames(region_coef) <- paste0(1:dim(region_coef)[1])


  probs_train <- predict(myglmnet, feature_train, type = "response")
  ROCPred_train <- prediction(probs_train, label_train)
  ROCPer_train <- performance(ROCPred_train,
    measure = "tpr",
    x.measure = "fpr"
  )

  auc_train <- performance(ROCPred_train, measure = "auc")
  auc_train <- auc_train@y.values[[1]]

  # png(glue::glue("{folder_path}/ROC_train_{i}.png"))
  # plot(ROCPer_train, main = "train_data")
  # dev.off()

  predict_train <- ifelse(probs_train > 0.5, 1, 0)
  print("train")
  print(table(label_train, predict_train))

  missing_classerr_train <- mean(predict_train != label_train)
  print(paste0(i, "-accu_tr-", 1 - missing_classerr_train))
  print(paste0(i, "-AUROC_tr-", auc_train))

  probs_test <- predict(myglmnet, feature_test, type = "response")
  ROCPred_test <- prediction(probs_test, label_test)
  ROCPer_test <- performance(ROCPred_test,
    measure = "tpr",
    x.measure = "fpr"
  )

  auc_test <- performance(ROCPred_test, measure = "auc")
  auc_test <- auc_test@y.values[[1]]

  # png(glue::glue("{folder_path}/ROC_test_{i}.png"))
  # plot(ROCPer_test, main = "Test_data")
  # dev.off()

  predict_test <- ifelse(probs_test > 0.5, 1, 0)
  print("test")
  print(table(label_test, predict_test))

  missing_classerr_test <- mean(predict_test != label_test)
  print(paste0(i, "-accu_ts-", 1 - missing_classerr_test))
  print(paste0(i, "-AUROC_ts-", auc_test))

  absolute_coef <- abs(region_coef)
  longData <- melt(absolute_coef)
  # longData<-longData[longData$value!=0,]

  # ggplot(longData, aes(x = Var2, y = Var1)) +
  #   geom_raster(aes(fill = value)) +
  #   scale_fill_gradient(low = "white", high = "red") +
  #   labs(x = "Featues", y = "Regions", title = "Absolute Coefficients") +
  #   theme_bw() +
  #   theme(
  #     axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
  #     axis.text.y = element_text(size = 9),
  #     plot.title = element_text(size = 11)
  #   )
  # ggsave(paste0(folder_path, glue::glue("/Coefficient_plot_{i}.pdf")))

  train_scores_accu <- c(train_scores_accu, 1 - missing_classerr_train)
  test_scores_accu <- c(test_scores_accu, 1 - missing_classerr_test)
  train_scores_auc <- c(train_scores_auc, auc_train)
  test_scores_auc <- c(test_scores_auc, auc_test)
}

save(all_coeffs, file = paste0(folder_path, "/all_coefficients.RData"))
save(train_scores_accu, file = paste0(folder_path, "/train_scores_accu.RData"))
save(test_scores_accu, file = paste0(folder_path, "/test_scores_accu.RData"))
save(train_scores_auc, file = paste0(folder_path, "/train_scores_auc.RData"))
save(test_scores_auc, file = paste0(folder_path, "/test_scores_auc.RData"))
