library(gdata)
library(pracma)
library(dplyr)
library(fable)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/id.RData"))

subject_size <- length(subject_id)

myorder <- 4

tsorder <- array(0 , dim = c(subject_size, 86*(myorder+2)))

dur_total = 0 

for (i in seq_len(subject_size)) {
  start_time <- Sys.time()
  fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")
  load(fname_timeseries)
  rest1_lr <- timeseries[1:1200, ]
  
  local_coeffs <-  array(0 , dim = c(86, myorder+2))
  
  local_coeffs <- apply(rest1_lr, 2, FUN = function(v) {
    rest1_lr_t <- as_tibble(v)
    rest1_lr_t["index"] <- 1:1200
    my_model <- rest1_lr_t %>%
      as_tsibble(index = index) %>%
      model(arima = ARIMA(value ~ 1 + pdq(myorder, 0, 0)) )
    my_model<-my_model$arima[[1]]
    coeffs <- my_model$fit$model$coef
    mysd <- sd(my_model$fit$model$residuals)
    return(c(coeffs, mysd))
    
  })
  tsorder[i,] <- c(t(local_coeffs))
  cat(paste0(i,", "))
  end_time <- Sys.time()
  dur <- difftime(end_time,start_time)
  dur_total = dur_total + dur
  remaining <- (dur_total/i) * (subject_size - i )
  formatted_time <- format(remaining, format = "%H:%M:%S")
  print(paste0("remaining ", formatted_time) )
}

save(tsorder, file = paste0(dir_output, "/ar_coeff_",myorder, ".RData"))
