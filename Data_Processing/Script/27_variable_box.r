library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")
dir_temp <- paste0(dir_processing, "/Temp")

load(paste0(dir_processing, "/Output/subject_id_all.RData"))

region_name <- read.table(paste0(dir_data, "/regions.txt"), header = FALSE)$V1

timeseries_all_dir <- paste0(dir_output, "/timeseries_all")

load(paste0(timeseries_all_dir, "/mean_all.RData"))
load(paste0(timeseries_all_dir, "/sd_all.RData"))

load(paste0(timeseries_all_dir, "/pacf1_all.RData"))
load(paste0(timeseries_all_dir, "/pacf2_all.RData"))
load(paste0(timeseries_all_dir, "/pacf3_all.RData"))

load(paste0(timeseries_all_dir, "/acf2_all.RData"))
load(paste0(timeseries_all_dir, "/acf3_all.RData"))
load(paste0(timeseries_all_dir, "/acf4_all.RData"))

load(paste0(timeseries_all_dir, "/yy_cor_all.RData"))

source(paste0(dir_processing, "/Script/01_library.R"))
source(paste0(dir_processing, "/Script/03_functions.R"))

subject_size <- length(subject_id_all)

boxplot_path <- paste0(dir_temp, "/timeseries_boxplot")
create_dir(boxplot_path)

timeseries_mean_arr_flatten <- timeseries_mean_all
dim(timeseries_mean_arr_flatten) <- c(subject_size * 4, 86)

timeseries_sd_arr_flatten <- timeseries_sd_all
dim(timeseries_sd_arr_flatten) <- c(subject_size * 4, 86)

timeseries_pacf1_arr_flatten <- timeseries_pacf1_all
dim(timeseries_pacf1_arr_flatten) <- c(subject_size * 4, 86)

timeseries_pacf2_arr_flatten <- timeseries_pacf2_all
dim(timeseries_pacf2_arr_flatten) <- c(subject_size * 4, 86)

timeseries_pacf3_arr_flatten <- timeseries_pacf3_all
dim(timeseries_pacf3_arr_flatten) <- c(subject_size * 4, 86)

timeseries_acf2_arr_flatten <- timeseries_acf2_all
dim(timeseries_acf2_arr_flatten) <- c(subject_size * 4, 86)

timeseries_acf3_arr_flatten <- timeseries_acf3_all
dim(timeseries_acf3_arr_flatten) <- c(subject_size * 4, 86)

timeseries_acf4_arr_flatten <- timeseries_acf4_all
dim(timeseries_acf4_arr_flatten) <- c(subject_size * 4, 86)

for (i in 1:subject_size) {
    pdf(
        file = paste0(boxplot_path, "/", subject_id_all[i], ".pdf"),
        width = 10, height = 4
    )
    for (region in seq_len(86)) {
        par(mfrow = c(1, 8))
        par(oma = c(0, 0, 3, 0))
        boxplot(
            timeseries_mean_all[, i, region],
            timeseries_mean_arr_flatten[, region],
            main = "mean"
        )
        boxplot(
            timeseries_sd_all[, i, region],
            timeseries_sd_arr_flatten[, region],
            main = "sd"
        )

        boxplot(
            timeseries_acf2_all[, i, region],
            timeseries_acf2_arr_flatten[, region],
            main = "acf2"
        )
        boxplot(
            timeseries_acf3_all[, i, region],
            timeseries_acf3_arr_flatten[, region],
            main = "acf3"
        )
        boxplot(
            timeseries_acf4_all[, i, region],
            timeseries_acf4_arr_flatten[, region],
            main = "acf4"
        )

        boxplot(
            timeseries_pacf1_all[, i, region],
            timeseries_pacf1_arr_flatten[, region],
            main = "pacf1"
        )
        boxplot(
            timeseries_pacf2_all[, i, region],
            timeseries_pacf2_arr_flatten[, region],
            main = "pacf2"
        )
        boxplot(
            timeseries_pacf3_all[, i, region],
            timeseries_pacf3_arr_flatten[, region],
            main = "pacf3"
        )

        mtext(region_name[region], cex = 1.2, outer = TRUE)
    }
    dev.off()


    print(paste0(i, " of ", subject_size))
    print(subject_id_all[i])
}
