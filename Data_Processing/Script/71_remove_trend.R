library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
dir_temp <- paste0(dir_processing, "/Temp")
dir_output <- paste0(dir_processing, "/Output")

load(paste0(dir_processing, "/Output/subject_id_all.RData"))
subject_size <- length(subject_id_all)

dur_total = 0 
for (i in 1:subject_size) { # length of subject_list
  start_time <- Sys.time()
  # i = 1
  fname_timeseries <- paste0(dir_timeseries, "/", subject_id_all[i], "_TS.RData")
  
 
  load(fname_timeseries)
  
  rest1_lr <- timeseries[1:1200, ]
  rest1_lr <- apply(rest1_lr, 2, FUN = function(v){
    time_index <- seq_along(v)
    lm_model <- lm(v ~ time_index)
    trend_component <- predict(lm_model)
    detrended_series <- v - trend_component + lm_model$coefficients[[1]]
    return(detrended_series)
  })
  
  rest1_rl <- timeseries[1201:2400, ]
  rest1_rl <- apply(rest1_rl, 2, FUN = function(v){
    time_index <- seq_along(v)
    lm_model <- lm(v ~ time_index)
    trend_component <- predict(lm_model)
    detrended_series <- v - trend_component + lm_model$coefficients[[1]]
    return(detrended_series)
  })
  
  rest2_lr <- timeseries[2401:3600, ]
  rest2_lr <- apply(rest2_lr, 2, FUN = function(v){
    time_index <- seq_along(v)
    lm_model <- lm(v ~ time_index)
    trend_component <- predict(lm_model)
    detrended_series <- v - trend_component + lm_model$coefficients[[1]]
    return(detrended_series)
  })
  
  rest2_rl <- timeseries[3601:4800, ]
  rest2_rl <- apply(rest2_rl, 2, FUN = function(v){
    time_index <- seq_along(v)
    lm_model <- lm(v ~ time_index)
    trend_component <- predict(lm_model)
    detrended_series <- v - trend_component + lm_model$coefficients[[1]]
    return(detrended_series)
  })
  
  timeseries <- rbind(rest1_lr, rest1_rl, rest2_lr, rest2_rl)
  print(dim(timeseries))
  
  save(timeseries, file = paste0(dir_output, "/detrended/" , subject_id_all[i], "_TS.RData" ))
  
  print(fname_timeseries)
  end_time <- Sys.time()
  dur <- difftime(end_time,start_time)
  dur_total = dur_total + dur
  remaining <- (dur_total/i) * (subject_size - i )
  formatted_time <- format(remaining, format = "%H:%M:%S")
  print(paste0("remaining ", formatted_time) )
}
