library("XICOR")

library(gdata)
library(pracma)



chatterjee_acf <- function(myarray, lag) {
  array_length <- length(myarray)
  array_first <- myarray[(lag + 1):array_length]
  array_second <- myarray[1:(array_length - lag)]
  # print(length(array_first))
  # print(length(array_second))
  myacf <- max(
    xicor(array_first, array_second), xicor(array_second, array_first)
  )
  return(myacf)
}

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

output_vol_dir <- paste0(dir_output, "/timeseries_volume_matched")

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")

load(paste0(dir_output, "/filtered_id.RData"))
subject_id <- filtered_id

subject_size <- length(subject_id)

chatterjee_acf1 <- array(0, dim = c(subject_size, 86))
chatterjee_acf2 <- array(0, dim = c(subject_size, 86))
chatterjee_acf3 <- array(0, dim = c(subject_size, 86))

for (i in seq_len(subject_size)) {
  fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")
  load(fname_timeseries)
  rest1_lr <- timeseries[1:1200, ]

  chatterjee_acf1[i, ] <- apply(rest1_lr, 2, FUN = function(v) {
    return(chatterjee_acf(v, 1))
  })
  chatterjee_acf2[i, ] <- apply(rest1_lr, 2, FUN = function(v) {
    return(chatterjee_acf(v, 2))
  })
  chatterjee_acf3[i, ] <- apply(rest1_lr, 2, FUN = function(v) {
    return(chatterjee_acf(v, 3))
  })
  cat(paste0(i, ", "))
}

save(chatterjee_acf1, file = paste0(output_vol_dir, "/chatterjee_acf1.RData"))
save(chatterjee_acf2, file = paste0(output_vol_dir, "/chatterjee_acf2.RData"))
save(chatterjee_acf3, file = paste0(output_vol_dir, "/chatterjee_acf3.RData"))
