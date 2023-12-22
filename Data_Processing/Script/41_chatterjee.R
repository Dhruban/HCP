library("XICOR")

library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/id.RData"))

subject_size <- length(subject_id)


yy_chatterjee <- array(0, dim = c(subject_size, 86, 86))

count <- 0



for (sub in 1:subject_size) {
    fname_timeseries <- paste0(
        dir_timeseries, "/", subject_id[sub], "_TS.RData"
    )
    if (file.exists(fname_timeseries)) {
        load(fname_timeseries)
        count <- count + 1
        rest1_lr <- timeseries[1:1200, ]
        for (i in 1:86) {
            for (j in 1:86) {
                yy_chatterjee[sub, i, j] <- xicor(rest1_lr[, i], rest1_lr[, j])
            }
        }
    } else {
        print("file does not exist")
    }
    # print(fname_timeseries)
    # print(paste0(sub, " of ", subject_size, " done"))
    cat(paste0(sub, ","))
}

save(yy_chatterjee, file = paste0(dir_output, "/yy_chatterjee_mat.RData"))
