# experiment

dir_data <- "./Data"
dir_processing <- "./Data_Processing"

source(paste0(dir_processing, "/Script/library.R"))

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/id.RData"))

subject_size <- length(subject_id)


acf2_diff1 <- array(0, dim = c(subject_size, 86))
acf3_diff1 <- array(0, dim = c(subject_size, 86))

acf2_diff2 <- array(0, dim = c(subject_size, 86))
acf3_diff2 <- array(0, dim = c(subject_size, 86))

acf2_diff3 <- array(0, dim = c(subject_size, 86))
acf3_diff3 <- array(0, dim = c(subject_size, 86))

acf2_diff4 <- array(0, dim = c(subject_size, 86))
acf3_diff4 <- array(0, dim = c(subject_size, 86))


count <- 0


for (i in 1:subject_size) {
    fname_timeseries <- paste0(dir_timeseries, "/", subject_id[i], "_TS.RData")
    if (file.exists(fname_timeseries)) {
        load(fname_timeseries)
        count <- count + 1
        rest1_lr <- timeseries[1:1200, ]
        subacf_diff1 <- apply(rest1_lr, 2,
            FUN = function(v) {
                return(acf(v, lag.max = 1500, plot = FALSE)$acf)
            }
        )
        subacf_diff2 <- apply(subacf_diff1, 2,
            FUN = function(v) {
                return(acf(v, lag.max = 1500, plot = FALSE)$acf)
            }
        )
        subacf_diff3 <- apply(subacf_diff2, 2,
            FUN = function(v) {
                return(acf(v, lag.max = 1500, plot = FALSE)$acf)
            }
        )
        subacf_diff4 <- apply(subacf_diff3, 2,
            FUN = function(v) {
                return(acf(v, lag.max = 1500, plot = FALSE)$acf)
            }
        )

        acf2_diff1[count, ] <- subacf_diff1[2, ]
        acf3_diff1[count, ] <- subacf_diff1[3, ]

        acf2_diff2[count, ] <- subacf_diff2[2, ]
        acf3_diff2[count, ] <- subacf_diff2[3, ]

        acf2_diff3[count, ] <- subacf_diff3[2, ]
        acf3_diff3[count, ] <- subacf_diff3[3, ]

        acf2_diff4[count, ] <- subacf_diff4[2, ]
        acf3_diff4[count, ] <- subacf_diff4[3, ]
    } else {
        print("something is wrong")
        continue
    }
    print(fname_timeseries)
}

save(acf2_diff1, file = paste0(dir_processing, "/Output/acf2_diff1.RData"))
save(acf3_diff1, file = paste0(dir_processing, "/Output/acf3_diff1.RData"))

save(acf2_diff2, file = paste0(dir_processing, "/Output/acf2_diff2.RData"))
save(acf3_diff2, file = paste0(dir_processing, "/Output/acf3_diff2.RData"))

save(acf2_diff3, file = paste0(dir_processing, "/Output/acf2_diff3.RData"))
save(acf3_diff3, file = paste0(dir_processing, "/Output/acf3_diff3.RData"))

save(acf2_diff4, file = paste0(dir_processing, "/Output/acf2_diff4.RData"))
save(acf3_diff4, file = paste0(dir_processing, "/Output/acf3_diff4.RData"))
