library(tidyverse)
library(readxl)
library(writexl)
library(glmnet)
library(ROCR)
library(reshape2)
library(ggplot2)

library(doParallel)
# registerDoParallel(10)





mix <- 0.5 # 1 for lasso, 0 for ridge
splitratio <- 0.8 # train/test

dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"


dir.output <- paste0(dir.analysis, "/Cognition/Output")
dir.temp <- paste0(dir.analysis, "/Cognition/Temp")
dir.script <- paste0(dir.analysis, "/Cognition/Script")

source(paste0(dir.script, "/99_functions.R"))

path.result <- paste0(dir.output, glue::glue("/{variable}_result.xlsx"))

path_xvar_result <- paste0(
  dir.output,
  glue::glue("/{variable}_xvar_result.xlsx")
)

# if result does not exist create new
result <- list()
xvar_result <- list()

if (file.exists(path.result)) {
  result_sheets <- as.character(excel_sheets(path.result))
  for (sheet_name in result_sheets) {
    result[sheet_name] <- list(read_excel(path.result, sheet = sheet_name))
  }
}

if (file.exists(path_xvar_result)) {
  result_sheets <- as.character(excel_sheets(path_xvar_result))
  for (sheet_name in result_sheets) {
    xvar_result[sheet_name] <- list(read_excel(path_xvar_result, sheet = sheet_name))
  }
}

x_var_name <- paste(xvars, sep = "_", collapse = "_")

folder_name <- glue::glue(
  "{variable}/splitBy_{splitBy}/elastic/split_{splitratio}/mix_{mix}/{x_var_name}"
)
folder_path <- paste0(dir.temp, "/", folder_name)
dir.create(folder_path, recursive = TRUE)

train_scores <- c()
test_scores <- c()
all_coeffs <- array(0, dim = c(10, dim(feature)[2]))

for (i in 1:10) {
  print(i)

  dataset <- getdata(i, splitratio)

  feature_train <- dataset$feature_train
  feature_test <- dataset$feature_test
  label_train <- dataset$label_train
  label_test <- dataset$label_test

  print("cvglmnet start")
  c1 <- makeCluster(8)
  registerDoParallel(c1)
  cvglmnet <- cv.glmnet(feature_train, label_train,
    gamma = mix, family = gaussian(), parallel = TRUE
  )
  stopCluster(c1)
  print("cvglmnet end")

  myglmnet <- glmnet(feature_train, label_train,
    alpha = mix, lambda = cvglmnet$lambda.min, family = gaussian()
  )

  coef <- coef(myglmnet, s = cvglmnet$lambda.1se)

  pred_train <- predict(myglmnet, feature_train)

  print(R2_Score(pred_train, label_train))

  train_scores <- c(train_scores, R2_Score(pred_train, label_train))

  pred_test <- predict(myglmnet, feature_test)

  print(R2_Score(pred_test, label_test))

  test_scores <- c(test_scores, R2_Score(pred_test, label_test))
  print(train_scores)
  print(test_scores)
  print(paste0("done", i))

  # coefficent plot

  coef_num <- as.numeric(coef)
  coef_beta <- coef_num[-1]
  all_coeffs[i, ] <- coef_beta
  region_num <- 86
  region_coef <- matrix(coef_beta, nrow = region_num, byrow = FALSE)
  colnames(region_coef) <- paste0(1:dim(region_coef)[2])
  rownames(region_coef) <- paste0(1:dim(region_coef)[1])

  test_result <- R2_Score(pred_test, label_test)
  test_result_str <- format(round(test_result, 2), nsmall = 2)

  absolute_coef <- abs(region_coef)
  longData <- melt(absolute_coef)
  ggplot(longData, aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Featues", y = "Regions", title = "Absolute Coefficients") +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
      axis.text.y = element_text(size = 9),
      plot.title = element_text(size = 11)
    )
  ggsave(paste0(folder_path, "/", glue::glue("{i}_coefficients_{test_result_str}.png")))

  png(glue::glue("{folder_path}/{i}_boxplot_{test_result_str}.png"))
  boxplot(label_train, label_test)
  dev.off()
}

save(all_coeffs, file = paste0(folder_path, "/all_coefficients.RData"))
save(test_scores, file = paste0(folder_path, "/test_scores.RData"))
save(train_scores, file = paste0(folder_path, "/train_scores.RData"))
