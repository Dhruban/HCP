# experiment

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
dir_temp <- paste0(dir_processing, "/Temp")

load(paste0(dir_processing, "/Output/id.RData"))
subject_size <- length(subject_id)
timeseries_period <- array(0, dim = c(subject_size, 86))

calculate_period <- function(arr) {
    arr_peaks <- which(diff(sign(diff(arr))) < 0) + 1
    arr_bottoms <- which(diff(sign(diff(arr))) > 0) + 1

    if (length(arr_peaks) == 0 || length(arr_bottoms) == 0) {
        stop("No peaks or bottoms found in the array.")
    }

    diff_positions <- abs(arr_peaks - arr_bottoms)
    return(diff_positions)
}

count <- 0

for (i in 1:subject_size) { # length of subject_list
    fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")

    if (file.exists(fname_timeseries)) {
        load(fname_timeseries)

        resr1_lr <- timeseries[1:1200, ]
        count <- count + 1
        timeseries_period[count, ] <- apply(resr1_lr, 2, FUN = function(v) {
            return(mean(calculate_period(v)))
        })
    }
    print(fname_timeseries)
}
save(timeseries_period, file = paste0(dir_processing, "/Output/period.RData"))
