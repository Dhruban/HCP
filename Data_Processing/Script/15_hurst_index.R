# calculates hurst index from rest1lr timeseries

library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"

source(paste0(dir_processing, "/Script/library.R"))

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/id.RData"))

subject_size <- length(subject_id)

timeseries_hurst_hs <- array(0, dim = c(subject_size, 86))
timeseries_hurst_hrs <- array(0, dim = c(subject_size, 86))
timeseries_hurst_he <- array(0, dim = c(subject_size, 86))
timeseries_hurst_hal <- array(0, dim = c(subject_size, 86))
timeseries_hurst_ht <- array(0, dim = c(subject_size, 86))


count <- 0

for (i in 1:subject_size) {
  fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")
  if (file.exists(fname_timeseries)) {
    load(fname_timeseries)

    rest1_lr <- timeseries[1:1200, ]
    count <- count + 1
    timeseries_hurst_hs[count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(hurstexp(v, display = FALSE)$Hs)
    })
    timeseries_hurst_hrs[count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(hurstexp(v, display = FALSE)$Hrs)
    })
    timeseries_hurst_he[count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(hurstexp(v, display = FALSE)$He)
    })
    timeseries_hurst_hal[count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(hurstexp(v, display = FALSE)$Hal)
    })
    timeseries_hurst_ht[count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(hurstexp(v, display = FALSE)$Ht)
    })
  } else {
    continue
  }
  print(fname_timeseries)
}
save(timeseries_hurst_hs,
  file = paste0(dir_processing, "/Output/hurst_hs.RData")
)
save(timeseries_hurst_hrs,
  file = paste0(dir_processing, "/Output/hurst_hrs.RData")
)
save(timeseries_hurst_he,
  file = paste0(dir_processing, "/Output/hurst_he.RData")
)
save(timeseries_hurst_hal,
  file = paste0(dir_processing, "/Output/hurst_hal.RData")
)
save(timeseries_hurst_ht,
  file = paste0(dir_processing, "/Output/hurst_ht.RData")
)
