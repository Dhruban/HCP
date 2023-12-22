# run 312_load_features.R

library(tidyverse)

# library(httpgd)
# hgd()
dir_data <- "./Data"
dir_analysis <- "./Analysis"
dir_output <- paste0(dir_analysis, "/Cognition/output")

n <- 86


draw_corr <- function(mydata, name) {
    corr_vec <- numeric(n)


    for (i in 1:n) {
        corr_vec[i] <- cor(mydata[, i], region_volume[, i])
    }


    x <- c(1:86)
    y <- corr_vec
    y_min <- min(y) - 0.025
    y_max <- max(y)

    data <- data.frame(x, y)
    region_name <- read.table(paste0(dir_data, "/regions.txt"),
        header = FALSE
    )$V1
    rownames(data) <- region_name
    # Create scatter plot with vertical lines
    p <- ggplot(data, aes(x = x, y = y)) +
        expand_limits(y = c(y_min - 0.025, y_max + 0.2)) +
        geom_point() +
        geom_segment(aes(x = x, xend = x, y = y_min, yend = y),
            linetype = "solid", color = "red"
        ) +
        geom_text(aes(label = region_name),
            vjust = 0.3, hjust = -0.1, angle = 90
        ) +
        labs(
            title = paste0("Correlation Plot of ", name),
            x = "Region", y = "Correlation"
        )

    # plot(p)
    ggsave(paste0(dir_output, "/corr_plot/", name, ".png"),
        p,
        width = 12, height = 8, dpi = 300
    )


    x <- c(1:86)
    y <- corr_vec
    sorted_indices <- order(y)
    y <- y[sorted_indices]
    y_min <- min(y) - 0.025

    data <- data.frame(x, y)
    region_name <- read.table(paste0(dir_data, "/regions.txt"),
        header = FALSE
    )$V1
    region_name <- region_name[sorted_indices]
    rownames(data) <- region_name
    # Create scatter plot with vertical lines
    p <- ggplot(data, aes(x = x, y = y)) +
        expand_limits(y = c(y_min - 0.025, y_max + 0.2)) +
        geom_point() +
        geom_segment(aes(x = x, xend = x, y = y_min, yend = y),
            linetype = "solid", color = "red"
        ) +
        geom_text(aes(label = region_name),
            vjust = 0.3, hjust = -0.1, angle = 90
        ) +
        labs(
            title = paste0("Sorted Correlation Plot of ", name),
            x = "Region", y = "Correlation"
        )

    # plot(p)
    ggsave(paste0(dir_output, "/corr_plot/", name, "_sorted.png"),
        p,
        width = 12, height = 8, dpi = 300
    )
}

draw_corr(timeseries_mean, "mean")
draw_corr(timeseries_sd, "sd")
draw_corr(timeseries_acf2, "acf2")
draw_corr(timeseries_acf3, "acf3")
draw_corr(timeseries_acf4, "acf4")
draw_corr(timeseries_pacf1, "pacf1")
draw_corr(timeseries_pacf2, "pacf2")
draw_corr(timeseries_pacf3, "pacf3")
draw_corr(snr, "snr")
