library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
dir_temp <- paste0(dir_processing, "/Temp")

load(paste0(dir_processing, "/Output/id.RData"))
subject_size <- length(subject_id)
region_name <- read.table(paste0(dir_data, "/regions.txt"), header = FALSE)$V1

print("starting loop")

for (i in 1:subject_size) { # length of subject_list
    fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")

    if (file.exists(fname_timeseries)) {
        load(fname_timeseries)

        rest1_lr <- timeseries[1:1200, ]

        pdf(
            file = paste0(dir_temp, "/pacf/", subject_id[i], ".pdf"),
            width = 10, height = 5
        )
        for (region in seq_len(dim(rest1_lr)[2])) {
            pacf(rest1_lr[, region],
                lag.max = 999, plot = TRUE,
                main = paste0("region ", region, "-", region_name[region])
            )
        }
        dev.off()
    }
    print(paste0(i, " of ", subject_size))
    # print(fname_timeseries)
}
