library(ROCR)
library(reshape2)
library(ggplot2)
library(matrixStats)
library(ggnewscale)
library(tidyverse)
dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_analysis <- "./Analysis"

dir_output <- paste0(dir_analysis, "/Cognition/Output")
dir_temp <- paste0(dir_analysis, "/Cognition/Temp")

library(R.matlab)
hcp_dir <- "./Data/Elvisha/"
yeo_names <- as.vector(
  readMat(paste0(hcp_dir, "yeo/map_fs86_to_yeo7_weighted_sym.mat"))$winner.vec
)

load(paste0(dir_processing, "/Output/symmetric_permutation.RData"))
load(paste0(
  dir_temp,
  "/CogCrystalComp_AgeAdj_splitBy_Normal_elastic_split_0.8_mix_0.5/all_coefficients.RData"
))
absolute_coef <- abs(all_coeffs)
mean_abs <- apply(absolute_coef, 2, FUN = function(v) {
  return(mean(v))
})
region_name <- read.table(paste0(dir_data, "/regions.txt"), header = FALSE)$V1
region_num <- 86
region_coef <- matrix(mean_abs, nrow = region_num, byrow = FALSE)
region_coef <- region_coef[, 3:5]
colnames(region_coef) <- c("pacf1", "pacf2", "pacf3")
rownames(region_coef) <- yeo_names
long_data <- melt(region_coef)
long_data <- as.data.frame(long_data)
replicated <- as.vector(replicate(3, region_name))
long_data <- long_data %>% add_column(region = replicated)
maxvalue <- max(long_data$value)

ggplot() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "1"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "2"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "3"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "4"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "5"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "6"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "7"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "8"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  geom_raster(
    data = dplyr::filter(long_data, Var1 == "9"),
    aes(x = Var2, y = region, fill = value)
  ) +
  scale_fill_gradient2(low = "white", high = "red", limits = c(0, maxvalue)) +
  new_scale_fill() +
  # theme(legend.position="none")+
  ggtitle("Mean Total Cognition") +
  facet_grid(
    rows = vars(Var1),
    scales = "free", space = "free_y"
  ) +
  theme(
    title = element_text(colour = "white"),
    axis.text.x.bottom = element_text(colour = "white"),
    axis.text.y.left = element_text(colour = "white"),
    plot.background = element_rect(fill = "black"),
    axis.text.x = element_text(size = 5, angle = 0, vjust = 0.3),
    axis.text.y = element_text(size = 6),
    strip.text.y = element_blank()
  )
ggsave(paste0(dir_output, "/abs_mean_plot.png"), width = 3, height = 9)
