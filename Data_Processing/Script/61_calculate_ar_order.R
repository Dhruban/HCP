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

tsorder <- array(0 , dim = c(subject_size, 86))

for (i in seq_len(subject_size)) {
  fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")
  load(fname_timeseries)
  rest1_lr <- timeseries[1:1200, ]
  
  tsorder[i, ] <- apply(rest1_lr, 2, FUN = function(v) {
    rest1_lr_t <- as_tibble(v)
    rest1_lr_t["index"] <- 1:1200
    my_model <- rest1_lr_t %>%
      as_tsibble(index = index) %>%
      model(arima = ARIMA(value ~ 1 + pdq(0:10, 0, 0)) )
    my_model<-my_model$arima[[1]]
    p <- my_model$fit$spec$p
    return(p)
    
  })
  cat(paste0(i,", "))
}

save(tsorder, file = paste0(dir_output, "/tsorder.RData"))
  