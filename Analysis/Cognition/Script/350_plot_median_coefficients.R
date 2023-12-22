library(ROCR)
library(reshape2)
library(ggplot2)
library(matrixStats)
dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_analysis <- "./Analysis"

dir_output <- paste0(dir_analysis, "/Cognition/output")
dir_temp <- paste0(dir_analysis, "/Cognition/temp")

load(paste0(dir_output, "/all_coefficients.RData"))
load(paste0(dir_output, "/test_scores.RData"))
load(paste0(dir_output, "/train_scores.RData"))

plot_coef_abs <- function(data, filepath, name) {
  region_name <- read.table(paste0(dir_data, "/regions.txt"), header = FALSE)$V1
  region_num <- 86
  region_coef <- matrix(data, nrow = region_num, byrow = FALSE)
  colnames(region_coef) <- c(
    "mean", "sd", "pacf1", "pacf2", "pacf3", "acf2", "acf3", "acf4"
  )
  rownames(region_coef) <- region_name
  long_data <- melt(region_coef)
  ggplot(long_data, aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    scale_fill_gradient(low = "white", high = "red") +
    labs(x = "Featues", y = "Regions", title = name) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
      axis.text.y = element_text(size = 6),
      plot.title = element_text(size = 11)
    )
  ggsave(filepath)
}

plot_coef <- function(data, filepath, name) {
  region_name <- read.table(paste0(dir_data, "/regions.txt"), header = FALSE)$V1
  region_num <- 86
  region_coef <- matrix(data, nrow = region_num, byrow = FALSE)
  colnames(region_coef) <-
    c("mean", "sd", "pacf1", "pacf2", "pacf3", "acf2", "acf3", "acf4")
  rownames(region_coef) <- region_name
  long_data <- melt(region_coef)
  ggplot(long_data, aes(x = Var2, y = Var1)) +
    geom_raster(aes(fill = value)) +
    facet_grid(scales = "free", space = "free_y") +
    scale_fill_gradient2(low = "blue", high = "red", mid = "white") +
    labs(x = "Featues", y = "Regions", title = name) +
    theme_bw() +
    theme(
      axis.text.x = element_text(size = 9, angle = 0, vjust = 0.3),
      axis.text.y = element_text(size = 6),
      plot.title = element_text(size = 11)
    )
  ggsave(filepath)
}

absolute_coef <- abs(all_coeffs)

median_abs <- apply(absolute_coef, 2, FUN = function(v) {
  return(median(v))
})
weighted_median_abs <- apply(absolute_coef, 2, FUN = function(v) {
  return(weightedMedian(v, test_scores))
})

mean_abs <- apply(absolute_coef, 2, FUN = function(v) {
  return(mean(v))
})
weighted_mean_abs <- apply(absolute_coef, 2, FUN = function(v) {
  return(weighted.mean(v, test_scores))
})



plot_coef_abs(
  median_abs,
  paste0(dir_output, "/median_abs.png"), "Median Absolute"
)
plot_coef_abs(
  weighted_median_abs,
  paste0(dir_output, "/weighted_median_abs.png")
)
plot_coef_abs(mean_abs, paste0(dir_output, "/mean_abs.png"), "Mean Absolute")
plot_coef_abs(weighted_mean_abs, paste0(dir_output, "/weighted_mean_abs.png"))


median <- apply(all_coeffs, 2, FUN = function(v) {
  return(median(v))
})
weighted_median <- apply(all_coeffs, 2, FUN = function(v) {
  return(weightedMedian(v, test_scores))
})

mean <- apply(all_coeffs, 2, FUN = function(v) {
  return(mean(v))
})
weighted_mean <- apply(all_coeffs, 2, FUN = function(v) {
  return(weighted.mean(v, test_scores))
})

plot_coef(median, paste0(dir_output, "/median.png"), "Median")
plot_coef(weighted_median, paste0(dir_output, "/weighted_median.png"))
plot_coef(mean, paste0(dir_output, "/mean.png"), "Mean")
plot_coef(weighted_mean, paste0(dir_output, "/weighted_mean.png"))
