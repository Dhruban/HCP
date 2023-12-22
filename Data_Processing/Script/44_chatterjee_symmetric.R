library("XICOR")

library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

load(paste0(dir_output, "/yy_chatterjee_mat.RData"))

sub_len <- dim(yy_chatterjee)[1]
yy_chatterjee_sym <- array(0, dim = c(sub_len, 86, 86))

for (sub in seq_len(sub_len)) {
    for (i in 1:86) {
        for (j in 1:86) {
            yy_chatterjee_sym[sub, i, j] <-
                max(yy_chatterjee[sub, i, j], yy_chatterjee[sub, j, i])
        }
    }
    cat(paste0(sub, ","))
}

yy_chatterjee_max <- array(0, dim = c(sub_len, 86 * 85 / 2))

for (i in seq_len(sub_len)) {
    yy_chatterjee_max[i, ] <- upperTriangle(yy_chatterjee_sym[i, , ])
    cat(paste0(i, ","))
}

save(yy_chatterjee_max, file = paste0(dir_output, "/yy_chatterjee_max.RData"))
