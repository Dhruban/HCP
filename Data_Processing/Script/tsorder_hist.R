library(gdata)
library(pracma)
library(dplyr)
# library(fable)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

load(paste0(dir_output, "/tsorder.RData"))
myhist <- hist(tsorder, breaks = c(-0.5, 0.5, 1.5 , 2.5 ,3.5, 4.5,5.5,6.5))

