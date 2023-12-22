dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

load(paste0(dir_output, "/tsorder.RData"))

max_per_region <- apply(tsorder,2,max)
