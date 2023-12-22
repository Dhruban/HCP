library(ROCR)
library(reshape2)
library(ggplot2)
library(matrixStats)
library(ggnewscale)
dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_analysis <- "./Analysis"

dir_output <- paste0(dir_analysis, "/Cognition/output")
dir_temp <- paste0(dir_analysis, "/Cognition/temp")

library(R.matlab)
hcp_dir <- "./Data/Elvisha/"
yeo_names <- as.vector(
  readMat(paste0(hcp_dir, "yeo/map_fs86_to_yeo7_weighted_sym.mat"))$winner.vec
)
yeo_argsort <- order(yeo_names)

load(paste0(
  dir_temp,
  "/CogTotalComp_AgeAdj_splitBy_Gender_elastic_split_0.8_mix_0/all_coefficients.RData"
))
absolute_coef <- abs(all_coeffs)
mean_abs <- apply(absolute_coef, 2, FUN = function(v) {
  return(mean(v))
})
region_name <- read.table(paste0(dir_data, "/regions.txt"), header = FALSE)$V1
region_num <- 86
region_coef <- matrix(mean_abs, nrow = region_num, byrow = FALSE)
colnames(region_coef) <- c(
  "mean", "sd", "pacf1", "pacf2", "pacf3", "acf2", "acf3", "acf4"
)
rownames(region_coef) <- yeo_names
long_data <- melt(region_coef)
long_data < as.data.frame(long_data)
replicated <- as.vector(replicate(8, region_name))
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
    axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
    axis.text.y = element_text(size = 6),
    strip.text.y = element_blank()
  )
ggsave(paste0(dir_output, "/abs_mean_plot_ridge.pdf"), width = 5, height = 9)
