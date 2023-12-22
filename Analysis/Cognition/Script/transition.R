dir.data <- "./Data"
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"
dir.script <- paste0(dir.analysis, "/Cognition/script")

mean_train_scores <- c()
mean_test_scores <- c()

source(paste0(dir.script , "/99_functions.R"))
source(paste0(dir.script , "/310_generate_seed.R"))
source(paste0(dir.script , "/311_constants.R"))


for (j in 1:50){
  
  source(paste0(dir.script , "/313_featur_making.R"))
  source(paste0(dir.script , "/320_elasticnet.R"))
  mean_train_score <- c(mean_train_scores, mean(train_scores))
  mean_test_scores <- c(mean_test_scores, mean(test_scores))
  print(mean(train_scores))
  print(mean(test_scores))
  save(mean_train_score,file = paste0(folder_name, "/mean_train_score.RData"))
  save(mean_test_scores, file= paste0(folder_name, "/mean_test_scores.RData"))
}

