# stores the ids which dont have any 'NA' in 'subject_id_all.RData'

library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

source(paste0(dir_processing, "/Script/library.R"))

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/id.RData"))

subject_size <- length(subject_id)

subject_id_all <- c()

for (i in 1:subject_size) {
  fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")
  if (file.exists(fname_timeseries)) {
    load(fname_timeseries)
    if (!any(is.na(timeseries))) {
      subject_id_all <- c(subject_id_all, subject_id[i])
    }
  }
}

save(subject_id_all, file = paste0(dir_output, "/subject_id_all.RData"))
