library("XICOR")

library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

load(paste0(dir_output, "/yy_chatterjee_mat.RData"))

yy_chatterjee_full <- array(0, dim = c(dim(yy_chatterjee)[1], 86 * 86))

for (i in seq_len(dim(yy_chatterjee)[1])) {
    yy_chatterjee_full[i, ] <- c(yy_chatterjee[i, , ])
    cat(paste0(i, ","))
}

save(yy_chatterjee_full, file = paste0(dir_output, "/yy_chatterjee_full.RData"))
