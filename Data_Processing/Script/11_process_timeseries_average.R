# library(gdata)
# library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

source(paste0(dir_processing, "/Script/01_library.R"))

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/subject_id_all.RData"))

print("output directory")
print(dir_output)

subject_size <- length(subject_id_all)

# making empty arrays
timeseries_mean_arr <- array(0, dim = c(4, subject_size, 86))
timeseries_sd_arr <- array(0, dim = c(4, subject_size, 86))
timeseries_pacf1_arr <- array(0, dim = c(4, subject_size, 86))
timeseries_pacf2_arr <- array(0, dim = c(4, subject_size, 86))
timeseries_pacf3_arr <- array(0, dim = c(4, subject_size, 86))
timeseries_acf2_arr <- array(0, dim = c(4, subject_size, 86))
timeseries_acf3_arr <- array(0, dim = c(4, subject_size, 86))
timeseries_acf4_arr <- array(0, dim = c(4, subject_size, 86))
yy_cor_arr <- array(0, dim = c(4, subject_size, (86 * 85 / 2)))


count <- 0
for (i in 1:subject_size) {
  fname_timeseries <- paste0(
    dir_timeseries, "/",
    subject_id_all[i], "_TS.RData"
  )
  if (file.exists(fname_timeseries)) {
    load(fname_timeseries)
    count <- count + 1
    rest1_lr <- timeseries[1:1200, ]
    rest1_rl <- timeseries[1201:2400, ]
    rest2_lr <- timeseries[2401:3600, ]
    rest2_rl <- timeseries[3601:4800, ]

    yy_cor_arr[1, count, ] <- upperTriangle(abs(cor(rest1_lr)), diag = FALSE)
    timeseries_mean_arr[1, count, ] <- as.array(colMeans(rest1_lr))
    timeseries_sd_arr[1, count, ] <- as.array(apply(rest1_lr, 2, sd))
    timeseries_pacf1_arr[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[1, , ])
    })
    timeseries_pacf2_arr[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[2, , ])
    })
    timeseries_pacf3_arr[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf2_arr[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf3_arr[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[4, , ])
    })
    timeseries_acf4_arr[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[5, , ])
    })

    yy_cor_arr[2, count, ] <- upperTriangle(abs(cor(rest1_rl)), diag = FALSE)
    timeseries_mean_arr[2, count, ] <- as.array(colMeans(rest1_rl))
    timeseries_sd_arr[2, count, ] <- as.array(apply(rest1_rl, 2, sd))
    timeseries_pacf1_arr[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[1, , ])
    })
    timeseries_pacf2_arr[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[2, , ])
    })
    timeseries_pacf3_arr[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf2_arr[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf3_arr[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[4, , ])
    })
    timeseries_acf4_arr[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[5, , ])
    })

    yy_cor_arr[3, count, ] <- upperTriangle(abs(cor(rest2_lr)), diag = FALSE)
    timeseries_mean_arr[3, count, ] <- as.array(colMeans(rest2_lr))
    timeseries_sd_arr[3, count, ] <- as.array(apply(rest2_lr, 2, sd))
    timeseries_pacf1_arr[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[1, , ])
    })
    timeseries_pacf2_arr[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[2, , ])
    })
    timeseries_pacf3_arr[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf2_arr[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf3_arr[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[4, , ])
    })
    timeseries_acf4_arr[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[5, , ])
    })

    yy_cor_arr[4, count, ] <- upperTriangle(abs(cor(rest2_rl)), diag = FALSE)
    timeseries_mean_arr[4, count, ] <- as.array(colMeans(rest2_rl))
    timeseries_sd_arr[4, count, ] <- as.array(apply(rest2_rl, 2, sd))
    timeseries_pacf1_arr[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[1, , ])
    })
    timeseries_pacf2_arr[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[2, , ])
    })
    timeseries_pacf3_arr[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
      return(pacf(v, lag.max = 3, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf2_arr[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf3_arr[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[4, , ])
    })
    timeseries_acf4_arr[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
      return(acf(v, lag.max = 4, plot = FALSE)$acf[5, , ])
    })

    print(fname_timeseries)
    print(paste0(i, " of ", subject_size))
  }
}



# Taking average
timeseries_mean <- apply(timeseries_mean_arr, c(2, 3), mean)
timeseries_sd <- apply(timeseries_sd_arr, c(2, 3), mean)
timeseries_pacf1 <- apply(timeseries_pacf1_arr, c(2, 3), mean)
timeseries_pacf2 <- apply(timeseries_pacf2_arr, c(2, 3), mean)
timeseries_pacf3 <- apply(timeseries_pacf3_arr, c(2, 3), mean)
timeseries_acf2 <- apply(timeseries_acf2_arr, c(2, 3), mean)
timeseries_acf3 <- apply(timeseries_acf3_arr, c(2, 3), mean)
timeseries_acf4 <- apply(timeseries_acf4_arr, c(2, 3), mean)
yy_cor <- apply(yy_cor_arr, c(2, 3), mean)

# saving
save(timeseries_mean, file = paste0(dir_output, "/mean_avarage.RData"))
save(timeseries_sd, file = paste0(dir_output, "/sd_avarage.RData"))
save(timeseries_pacf1, file = paste0(dir_output, "/pacf1_avarage.RData"))
save(timeseries_pacf2, file = paste0(dir_output, "/pacf2_avarage.RData"))
save(timeseries_pacf3, file = paste0(dir_output, "/pacf3_avarage.RData"))
save(timeseries_acf2, file = paste0(dir_output, "/acf2_avarage.RData"))
save(timeseries_acf3, file = paste0(dir_output, "/acf3_avarage.RData"))
save(timeseries_acf4, file = paste0(dir_output, "/acf4_avarage.RData"))
save(yy_cor, file = paste0(dir_output, "/yycor_avarage.RData"))

save(timeseries_mean_arr, file = paste0(dir_output, "/mean_all.RData"))
save(timeseries_sd_arr, file = paste0(dir_output, "/sd_all.RData"))
save(timeseries_pacf1_arr, file = paste0(dir_output, "/pacf1_all.RData"))
save(timeseries_pacf2_arr, file = paste0(dir_output, "/pacf2_all.RData"))
save(timeseries_pacf3_arr, file = paste0(dir_output, "/pacf3_all.RData"))
save(timeseries_acf2_arr, file = paste0(dir_output, "/acf2_all.RData"))
save(timeseries_acf3_arr, file = paste0(dir_output, "/acf3_all.RData"))
save(timeseries_acf4_arr, file = paste0(dir_output, "/acf4_all.RData"))
save(yy_cor_arr, file = paste0(dir_output, "/yycor_all.RData"))
