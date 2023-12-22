library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")
dir_timeseries <- paste0(dir_output, "/detrended")
dir_temp <- paste0(dir_processing, "/Temp")

load(paste0(dir_output, "/subject_id_all.RData"))
# load(paste0(dir_output, "/filtered_id.RData"))

source(paste0(dir_processing, "/Script/01_library.R"))
source(paste0(dir_processing, "/Script/03_functions.R"))

# timeseries_all_dir <- paste0(dir_output, "/timeseries_all")
timeseries_all_dir <- paste0(dir_output, "/ts_detrend")

create_dir(timeseries_all_dir)

# subject_id_all <- setdiff(subject_id_all, filtered_id)
subject_id_all <- subject_id_all
print(length(subject_id_all))
subject_size <- length(subject_id_all)


# making empty arrays
timeseries_mean_all <- array(0, dim = c(4, subject_size, 86))
timeseries_sd_all <- array(0, dim = c(4, subject_size, 86))
timeseries_pacf1_all <- array(0, dim = c(4, subject_size, 86))
timeseries_pacf2_all <- array(0, dim = c(4, subject_size, 86))
timeseries_pacf3_all <- array(0, dim = c(4, subject_size, 86))
timeseries_acf2_all <- array(0, dim = c(4, subject_size, 86))
timeseries_acf3_all <- array(0, dim = c(4, subject_size, 86))
timeseries_acf4_all <- array(0, dim = c(4, subject_size, 86))
yy_cor_all <- array(0, dim = c(4, subject_size, (86 * 85 / 2)))

count <- 0

for (i in 1:subject_size) {
    fname_timeseries <- paste0(
        dir_timeseries, "/",
        subject_id_all[i], "_TS.RData"
    )
    # if (file.exists(fname_timeseries)) {
    load(fname_timeseries)
    count <- count + 1
    rest1_lr <- timeseries[1:1200, ]
    rest1_rl <- timeseries[1201:2400, ]
    rest2_lr <- timeseries[2401:3600, ]
    rest2_rl <- timeseries[3601:4800, ]

    yy_cor_all[1, count, ] <- upperTriangle(abs(cor(rest1_lr)), diag = FALSE)
    timeseries_mean_all[1, count, ] <- as.array(colMeans(rest1_lr))
    timeseries_sd_all[1, count, ] <- as.array(apply(rest1_lr, 2, sd))
    timeseries_pacf1_all[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[1, , ])
    })
    timeseries_pacf2_all[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[2, , ])
    })
    timeseries_pacf3_all[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf2_all[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf3_all[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[4, , ])
    })
    timeseries_acf4_all[1, count, ] <- apply(rest1_lr, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[5, , ])
    })

    yy_cor_all[2, count, ] <- upperTriangle(abs(cor(rest1_rl)), diag = FALSE)
    timeseries_mean_all[2, count, ] <- as.array(colMeans(rest1_rl))
    timeseries_sd_all[2, count, ] <- as.array(apply(rest1_rl, 2, sd))
    timeseries_pacf1_all[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[1, , ])
    })
    timeseries_pacf2_all[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[2, , ])
    })
    timeseries_pacf3_all[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf2_all[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf3_all[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[4, , ])
    })
    timeseries_acf4_all[2, count, ] <- apply(rest1_rl, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[5, , ])
    })

    yy_cor_all[3, count, ] <- upperTriangle(abs(cor(rest2_lr)), diag = FALSE)
    timeseries_mean_all[3, count, ] <- as.array(colMeans(rest2_lr))
    timeseries_sd_all[3, count, ] <- as.array(apply(rest2_lr, 2, sd))
    timeseries_pacf1_all[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[1, , ])
    })
    timeseries_pacf2_all[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[2, , ])
    })
    timeseries_pacf3_all[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf2_all[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf3_all[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[4, , ])
    })
    timeseries_acf4_all[3, count, ] <- apply(rest2_lr, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[5, , ])
    })

    yy_cor_all[4, count, ] <- upperTriangle(abs(cor(rest2_rl)), diag = FALSE)
    timeseries_mean_all[4, count, ] <- as.array(colMeans(rest2_rl))
    timeseries_sd_all[4, count, ] <- as.array(apply(rest2_rl, 2, sd))
    timeseries_pacf1_all[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[1, , ])
    })
    timeseries_pacf2_all[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[2, , ])
    })
    timeseries_pacf3_all[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
        return(pacf(v, lag.max = 3, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf2_all[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[3, , ])
    })
    timeseries_acf3_all[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[4, , ])
    })
    timeseries_acf4_all[4, count, ] <- apply(rest2_rl, 2, FUN = function(v) {
        return(acf(v, lag.max = 4, plot = FALSE)$acf[5, , ])
    })
    print(paste0(i, " of ", subject_size))
    print(fname_timeseries)
}

# save(timeseries_mean_all, file = paste0(timeseries_all_dir, "/mean_all.RData"))
# save(timeseries_sd_all, file = paste0(timeseries_all_dir, "/sd_all.RData"))
# 
# save(timeseries_acf2_all, file = paste0(timeseries_all_dir, "/acf2_all.RData"))
# save(timeseries_acf3_all, file = paste0(timeseries_all_dir, "/acf3_all.RData"))
# save(timeseries_acf4_all, file = paste0(timeseries_all_dir, "/acf4_all.RData"))
# 
# save(timeseries_pacf1_all,
#     file = paste0(timeseries_all_dir, "/pacf1_all.RData")
# )
# save(timeseries_pacf2_all,
#     file = paste0(timeseries_all_dir, "/pacf2_all.RData")
# )
# save(timeseries_pacf3_all,
#     file = paste0(timeseries_all_dir, "/pacf3_all.RData")
# )
# 
# save(yy_cor_all, file = paste0(timeseries_all_dir, "/yy_cor_all.RData"))

timeseries_mean= timeseries_mean_all[1,,]
timeseries_sd= timeseries_sd_all[1,,]
timeseries_snr= (timeseries_mean_all[1,,]/timeseries_sd_all[1,,])
timeseries_acf2=timeseries_acf2_all[1,,]
timeseries_acf3= timeseries_acf3_all[1,,]
timeseries_acf4= timeseries_acf4_all[1,,]
timeseries_pacf1= timeseries_pacf1_all[1,,]
timeseries_pacf2= timeseries_pacf2_all[1,,]
timeseries_pacf3= timeseries_pacf3_all[1,,]
yy_cor= yy_cor_all[1,,]

save(timeseries_mean, file = paste0(timeseries_all_dir, "/mean.RData"))
save(timeseries_sd, file = paste0(timeseries_all_dir, "/sd.RData"))
save(timeseries_snr, file = paste0(timeseries_all_dir, "/snr.RData"))
save(timeseries_acf2, file = paste0(timeseries_all_dir, "/acf2.RData"))
save(timeseries_acf3, file = paste0(timeseries_all_dir, "/acf3.RData"))
save(timeseries_acf4, file = paste0(timeseries_all_dir, "/acf4.RData"))

save(timeseries_pacf1,
     file = paste0(timeseries_all_dir, "/pacf1.RData")
)
save(timeseries_pacf2,
     file = paste0(timeseries_all_dir, "/pacf2.RData")
)
save(timeseries_pacf3,
     file = paste0(timeseries_all_dir, "/pacf3.RData")
)

save(yy_cor, file = paste0(timeseries_all_dir, "/yy_cor.RData"))
