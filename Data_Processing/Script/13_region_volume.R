library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

source(paste0(dir_processing, "/Script/library.R"))

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/id.RData"))

subject_size <- length(subject_id)

region_volume <- array(0, dim = c(subject_size, 86))
count <- 0


for (i in 1:subject_size) {
    fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")
    if (file.exists(fname_timeseries)) {
        load(fname_timeseries)
        count <- count + 1
        region_volume[count, ] <- volume[, 1]
    } else {
        continue
    }
    print(fname_timeseries)
}

save(region_volume, file = paste0(dir_output, "/region_volume.RData"))
