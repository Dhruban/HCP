library(ROCR)
library(reshape2)
library(ggplot2)
library(matrixStats)
library(tidyverse)
library(writexl)
dir.data <- "./Data"
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"

dir.output <- paste0(dir.analysis,"/Cognition/output")
dir.temp <- paste0(dir.analysis,"/Cognition/temp")

load(paste0(dir.output,"/all_coefficients.RData"))
load(paste0(dir.output,"/test_scores.RData"))
load(paste0(dir.output,"/train_scores.RData"))

absolute_coef <- abs(all_coeffs)

median_abs <- apply(absolute_coef, 2, FUN=function(v){return(median(v))})
region_num= 86
median_abs=matrix(median_abs,nrow=region_num,byrow=FALSE)

iqr_abs <- apply(absolute_coef, 2, FUN=function(v){return(IQR(v))})
region_num= 86
iqr_abs=matrix(iqr_abs,nrow=region_num,byrow=FALSE)

median_abs_data_frame <- as.data.frame(median_abs)
median_abs_data_frame = median_abs_data_frame %>%
  mutate_all(funs(format(round(., 2), nsmall = 2)))
region_name <- read.table(paste0(dir.data,"/regions.txt"), header = FALSE)$V1
colnames(median_abs_data_frame) <- c("mean","sd","pacf1","pacf2","pacf3","acf2","acf3","acf4")
median_abs_data_frame = median_abs_data_frame %>% mutate(region = region_name, .before = everything())  %>% arrange(-dplyr::row_number())

iqr_abs_data_frame <- as.data.frame(iqr_abs)
iqr_abs_data_frame = iqr_abs_data_frame %>%
  mutate_all(funs(format(round(., 2), nsmall = 2)))
region_name <- read.table(paste0(dir.data,"/regions.txt"), header = FALSE)$V1
colnames(iqr_abs_data_frame) <- c("mean","sd","pacf1","pacf2","pacf3","acf2","acf3","acf4")
iqr_abs_data_frame = iqr_abs_data_frame %>% mutate(region = region_name, .before = everything())  %>% arrange(-dplyr::row_number())

combined_abs_data_frame <- map2_dfc(median_abs_data_frame,iqr_abs_data_frame, function(x,y){return( glue::glue("{x}({y})"))})
combined_abs_data_frame = combined_abs_data_frame %>% mutate(region = rev(region_name), .before = everything())
# rownames(combined_abs_data_frame) <- region_name

write_xlsx(list("combined"=combined_abs_data_frame,"median"=median_abs_data_frame,"iqr"=iqr_abs_data_frame), paste0(dir.output ,"/coefficient_excel/median_abs.xlsx"))



median <- apply(all_coeffs, 2, FUN=function(v){return(median(v))})
region_num= 86
median=matrix(median,nrow=region_num,byrow=FALSE)

iqr <- apply(all_coeffs, 2, FUN=function(v){return(IQR(v))})
region_num= 86
iqr=matrix(iqr,nrow=region_num,byrow=FALSE)

median_data_frame <- as.data.frame(median)
median_data_frame = median_data_frame %>% 
  mutate_all(funs(format(round(., 2), nsmall = 2)))
region_name <- read.table(paste0(dir.data,"/regions.txt"), header = FALSE)$V1
colnames(median_data_frame) <- c("mean","sd","pacf1","pacf2","pacf3","acf2","acf3","acf4")
median_data_frame = median_data_frame %>% mutate(region = region_name, .before = everything()) %>% arrange(-dplyr::row_number())

iqr_data_frame <- as.data.frame(iqr)
iqr_data_frame = iqr_data_frame %>% 
  mutate_all(funs(format(round(., 2), nsmall = 2)))
region_name <- read.table(paste0(dir.data,"/regions.txt"), header = FALSE)$V1
colnames(iqr_data_frame) <- c("mean","sd","pacf1","pacf2","pacf3","acf2","acf3","acf4")
iqr_data_frame = iqr_data_frame %>% mutate(region = region_name, .before = everything()) %>% arrange(-dplyr::row_number())

combined_data_frame <- map2_dfc(median_data_frame,iqr_data_frame, function(x,y){return( glue::glue("{x}({y})"))})
combined_data_frame = combined_data_frame %>% mutate(region = rev(region_name), .before = everything())
# rownames(combined_data_frame) <- region_name

write_xlsx(list("combined"=combined_data_frame,"median"=median_data_frame,"iqr"=iqr_data_frame), paste0(dir.output ,"/coefficient_excel/median.xlsx"))

