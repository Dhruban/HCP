library("XICOR")

library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

load(paste0(dir_output, "/yy_chatterjee_mat.RData"))

yy_chatterjee_ul <- array(0, dim = c(dim(yy_chatterjee)[1], 86 * 85))

for (i in seq_len(dim(yy_chatterjee)[1])) {
    yy_chatterjee_ul[i, ] <- c(
        upperTriangle(yy_chatterjee[i, , ]),
        lowerTriangle(yy_chatterjee[i, , ])
    )
    cat(paste0(i, ","))
}

save(yy_chatterjee_ul, file = paste0(dir_output, "/yy_chatterjee_ul.RData"))
