library(writexl)
dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")
timeseries_all_dir <- paste0(dir_output, "/test_kendall")
load(paste0(dir_processing, "/Output/subject_id_all.RData"))
load( paste0(timeseries_all_dir, "/p_kendall.RData")
)
write_xlsx(data.frame(p_kendall), path = paste0(timeseries_all_dir, "/p_kendall.xlsx"))

indicator_test <- array(0, dim = dim(p_kendall))

indicator_test[p_kendall < 0.05] <- 1
indicator_row_sum <- apply(indicator_test, MARGIN = 1, FUN=sum)

subject_id_no_trend <- subject_id_all[indicator_row_sum<=4]
save(subject_id_no_trend, file = paste0(dir_output, "/subject_id_no_trend.RData"))
