library(gdata)
library(pracma)
library(Kendall)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")
timeseries_all_dir <- paste0(dir_output, "/test_kendall")


source(paste0(dir_processing, "/Script/01_library.R"))

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/subject_id_all.RData"))

subject_size <- length(subject_id_all)

p_kendall <- array(0, dim = c(subject_size, 86 ))


for (i in 1:subject_size) {
  fname_timeseries <- paste0(dir_timeseries, "/", subject_id_all[i], "_TS.RData")
  load(fname_timeseries)
  rest1_lr <- timeseries[1:1200, ]
  
  for ( j in 1:86 ){
    timeseries <- rest1_lr[,j]
    p_kendall[i,j] <- MannKendall(timeseries)$sl[[1]]
  }
  
  cat(paste0(i,","))
}

save(p_kendall,
     file = paste0(timeseries_all_dir, "/p_kendall.RData")
)
write.csv(p_kendall,
          file = paste0(timeseries_all_dir, "/p_kendall.csv"))