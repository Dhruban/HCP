library(ggplot2)
dir.data <- "./Data"
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"
dir.script <- paste0(dir.analysis, "/Sex/script")

mean_train_accus <- c()
mean_test_accus <- c()

mean_train_aucs <- c()
mean_test_aucs <- c()

sd_train_accus <- c()
sd_test_accus <- c()

sd_train_aucs <- c()
sd_test_aucs <- c()

mean_ratio <-c()
sd_ratio <- c()

size<- c()

source(paste0(dir.script , "/99_functions.R"))
source(paste0(dir.script , "/110_generate_seed.R"))
source(paste0(dir.script , "/111_constants.R"))

j=31
# for (j in 1:50){

source(paste0(dir.script , "/118a_feature_making_family_split.R"))
source(paste0(dir.script , "/120_elasticnet_logistic.R"))
mean_train_accus <- c(mean_train_accus, mean(train_scores_accu))
mean_test_accus <- c(mean_test_accus, mean(test_scores_accu))
print(paste0("mean(train_scores_accu)= ",mean(train_scores_accu)))
print(paste0("mean(test_scores_accu)= ",mean(test_scores_accu)))
save(mean_train_accus,file = paste0(folder_path, "/mean_train_accus.RData"))
save(mean_test_accus, file= paste0(folder_path, "/mean_test_accus.RData"))

mean_train_aucs <- c(mean_train_aucs, mean(train_scores_auc))
mean_test_aucs <- c(mean_test_aucs, mean(test_scores_auc))
print(paste0("mean(train_scores_auc)= ",mean(train_scores_auc)))
print(paste0("mean(test_scores_auc)= ",mean(test_scores_auc)))
save(mean_train_aucs,file = paste0(folder_path, "/mean_train_aucs.RData"))
save(mean_test_aucs, file= paste0(folder_path, "/mean_test_aucs.RData"))

sd_train_accus <- c(sd_train_accus, sd(train_scores_accu))
sd_test_accus <- c(sd_test_accus, sd(test_scores_accu))
print(paste0("sd(train_scores_accu)= ",sd(train_scores_accu)))
print(paste0("sd(test_scores_accu)= ",sd(test_scores_accu)))
save(sd_train_accus,file = paste0(folder_path, "/sd_train_accus.RData"))
save(sd_test_accus, file= paste0(folder_path, "/sd_test_accus.RData"))

sd_train_aucs <- c(sd_train_aucs, sd(train_scores_auc))
sd_test_aucs <- c(sd_test_aucs, sd(test_scores_auc))
print(paste0("sd(train_scores_auc)= ",sd(train_scores_auc)))
print(paste0("sd(test_scores_auc)= ",sd(test_scores_auc)))
save(sd_train_aucs,file = paste0(folder_path, "/sd_train_aucs.RData"))
save(sd_test_aucs, file= paste0(folder_path, "/sd_test_aucs.RData"))

mean_ratio <- c(mean_ratio,mean(ratio))
sd_ratio <- c(sd_ratio,sd(ratio))

size<- c(size, 996-5*j)

res_data <- data.frame(
  Mean = mean_train_accus,
  StandardError = sd_train_accus,
  Total_size= size
)
mean_plot <- ggplot(res_data, aes(x = Total_size, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - StandardError, ymax = Mean + StandardError), width = 0.2) +
  ggtitle("Train Accuracy") +
  labs(x = "Total Size", y = "Mean +- Standard Error") +
  theme_classic()

ggsave(paste0(folder_path, "/mean_train_accus.png"), plot = mean_plot, width = 6, height = 4, dpi = 300)

res_data <- data.frame(
  Mean = mean_test_accus,
  StandardError = sd_test_accus,
  Total_size= size
)
mean_plot <- ggplot(res_data, aes(x = Total_size, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - StandardError, ymax = Mean + StandardError), width = 0.2) +
  ggtitle("Test Accuracy") +
  labs(x = "Total Size", y = "Mean +- Standard Error") +
  theme_classic()

ggsave(paste0(folder_path, "/mean_test_accus.png"), plot = mean_plot, width = 6, height = 4, dpi = 300)

res_data <- data.frame(
  Mean = mean_train_aucs,
  StandardError = sd_train_aucs,
  Total_size= size
)
mean_plot <- ggplot(res_data, aes(x = Total_size, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - StandardError, ymax = Mean + StandardError), width = 0.2) +
  ggtitle("Train AUROC") +
  labs(x = "Total Size", y = "Mean +- Standard Error") +
  theme_classic()

ggsave(paste0(folder_path, "/mean_train_aucs.png"), plot = mean_plot, width = 6, height = 4, dpi = 300)

res_data <- data.frame(
  Mean = mean_test_aucs,
  StandardError = sd_test_aucs,
  Total_size= size
)
mean_plot <- ggplot(res_data, aes(x = Total_size, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - StandardError, ymax = Mean + StandardError), width = 0.2) +
  ggtitle("Test AUROC") +
  labs(x = "Total Size", y = "Mean +- Standard Error") +
  theme_classic()

ggsave(paste0(folder_path, "/mean_test_aucs.png"), plot = mean_plot, width = 6, height = 4, dpi = 300)


retio_data <- data.frame(
  MeanRatio = mean_ratio,
  StandardRatio = sd_ratio,
  Total_size= size
)
mean_plot <- ggplot(retio_data, aes(x = Total_size, y = MeanRatio)) +
  geom_point() +
  geom_errorbar(aes(ymin = MeanRatio - StandardRatio, ymax = MeanRatio + StandardRatio), width = 0.2) +
  ggtitle("Ratio") +
  labs(x = "Total Size", y = "Mean +- Standard Error") +
  theme_classic()

ggsave(paste0(folder_path, "/ratio.png"), plot = mean_plot, width = 6, height = 4, dpi = 300)

# }

