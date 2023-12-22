library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")
timeseries_all_dir <- paste0(dir_output, "/timeseries_ccf")

source(paste0(dir_processing, "/Script/01_library.R"))

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_processing, "/Output/subject_id_all.RData"))

subject_size <- length(subject_id_all)

ccf0 <- array(0, dim = c(subject_size, 86 * 86 ))
ccf1 <- array(0, dim = c(subject_size, 86 * 86 ))
ccf2 <- array(0, dim = c(subject_size, 86 * 86 ))
ccf3 <- array(0, dim = c(subject_size, 86 * 86 ))

for (i in 1:subject_size) {
  
  fname_timeseries <- paste0(dir_timeseries, "/", subject_id_all[i], "_TS.RData")

  load(fname_timeseries)
  # count <- count + 1
  rest1_lr <- timeseries[1:1200, ]
  ind_ccf0 <- array(0, dim =c(86,86))
  ind_ccf1 <- array(0, dim =c(86,86))
  ind_ccf2 <- array(0, dim =c(86,86))
  ind_ccf3 <- array(0, dim =c(86,86))
  for ( j in 1:86 ){
    for(k in 1:86){
      myccf <- ccf(rest1_lr[,j], rest1_lr[,k], lag.max = 3)
      ind_ccf0[j,k] = myccf$acf[4]
      ind_ccf1[j,k] = myccf$acf[5]
      ind_ccf2[j,k] = myccf$acf[6]
      ind_ccf3[j,k] = myccf$acf[7]
    }
  }
  ccf0[i, ] <- c(ind_ccf0)
  ccf1[i, ] <- c(ind_ccf1)
  ccf2[i, ] <- c(ind_ccf2)
  ccf3[i, ] <- c(ind_ccf3)
  
  cat(paste0(i,","))
  
}

save(ccf0,
     file = paste0(timeseries_all_dir, "/ccf0.RData")
)
save(ccf1,
     file = paste0(timeseries_all_dir, "/ccf1.RData")
)
save(ccf2,
     file = paste0(timeseries_all_dir, "/ccf2.RData")
)
save(ccf3,
     file = paste0(timeseries_all_dir, "/ccf3.RData")
)
