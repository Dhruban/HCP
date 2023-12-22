# import
library(tidyverse)
library(gbm)
library(caTools)
library(ROCR)



# for Sayan
dir.data <- "./Data"
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"
dir.output <- paste0(dir.analysis, "/Cognition/Output")

# Load Feature
load(paste0(dir.processing, "/Output/mean_all.RData"))
load(paste0(dir.processing, "/Output/sd_all.RData"))
load(paste0(dir.processing, "/Output/subject_id_all.RData"))
load(paste0(dir.processing, "/Output/pacf1_all.RData"))
load(paste0(dir.processing, "/Output/pacf2_all.RData"))
load(paste0(dir.processing, "/Output/pacf3_all.RData"))
load(paste0(dir.processing, "/Output/acf2_all.RData"))
load(paste0(dir.processing, "/Output/acf3_all.RData"))
load(paste0(dir.processing, "/Output/acf4_all.RData"))
# load(paste0(dir.processing,"/Output/hurst_he.RData"))
load(paste0(dir.processing, "/Output/yycor_all.RData"))
# load(paste0(dir.processing, "/Output/snr_all.RData"))
# load(paste0(dir.processing, "/Output/region_volume.RData"))
# load(paste0(dir.processing, "/Output/yy_cor_z.RData"))


# load(paste0(dir.processing, "/Output/yy_chatterjee_full.RData"))
# load(paste0(dir.processing, "/Output/yy_chatterjee_ul.RData"))
# load(paste0(dir.processing, "/Output/yy_chatterjee_max.RData"))
# 
# load(paste0(dir.processing, "/Output/chatterjee_acf1.RData"))
# load(paste0(dir.processing, "/Output/chatterjee_acf2.RData"))
# load(paste0(dir.processing, "/Output/chatterjee_acf3.RData"))
# 
# load(paste0(dir.processing, "/Output/acf3_diff4.RData"))
# load(paste0(dir.processing, "/Output/acf2_diff1.RData"))
# load(paste0(dir.processing, "/Output/acf3_diff1.RData"))
# load(paste0(dir.processing, "/Output/acf2_diff2.RData"))
# load(paste0(dir.processing, "/Output/acf3_diff2.RData"))
# load(paste0(dir.processing, "/Output/acf2_diff3.RData"))
# load(paste0(dir.processing, "/Output/acf3_diff3.RData"))
# load(paste0(dir.processing, "/Output/acf2_diff4.RData"))
# load(paste0(dir.processing, "/Output/acf3_diff4.RData"))
# # feature <- cbind(timeseries_hurst_he)
# 
# load(paste0(dir.processing, "/Output/timeseries_ccf/ccf0.RData"))
# load(paste0(dir.processing, "/Output/timeseries_ccf/ccf1.RData"))
# load(paste0(dir.processing, "/Output/timeseries_ccf/ccf2.RData"))
# load(paste0(dir.processing, "/Output/timeseries_ccf/ccf3.RData"))

# load(paste0(dir.processing, "/Output/ar_coeff_6.RData")) 

subject_id <- subject_id_all
feature <- c()

# if ("intercept" %in% xvars) {
#   print("intercept taken")
#   new_feature <- tsorder[,(6*86+1):(7*86)]
#   feature <- cbind(feature, new_feature)
# }
# if ("residual_sd" %in% xvars) {
#   print("residual_sd taken")
#   new_feature <- tsorder[,(7*86+1):(8*86)]
#   feature <- cbind(feature, new_feature)
# }
# if ("ar_coef" %in% xvars) {
#   print("ar_coef taken")
#   new_feature <- tsorder[,1:(6*86)]
#   feature <- cbind(feature, new_feature)
# }

if ("mean" %in% xvars) {
  print("mean taken")
  feature <- cbind(feature, timeseries_mean_arr[1,,])
}
if ("sd" %in% xvars) {
  print("sd taken")
  feature <- cbind(feature, timeseries_sd_arr[1,,])
}
if ("pacf" %in% xvars) {
  print("pacf taken")
  feature <- cbind(
    feature, timeseries_pacf1_arr[1,,], timeseries_pacf2_arr[1,,],
    timeseries_pacf3_arr[1,,]
  )
}

if ("acf" %in% xvars) {
  print("acf taken")
  feature <- cbind(feature, timeseries_acf2_arr[1,,], timeseries_acf3_arr[1,,], timeseries_acf4_arr[1,,])
}

if ("yycor" %in% xvars) {
  print("yycor taken")
  feature <- cbind(feature, yy_cor_arr[1,,])
}

if ("zyycor" %in% xvars) {
  print("zyycor taken")
  yy_cor <- 0.5 * log((1 + yy_cor_arr[1,,]) / (1 - yy_cor_arr[1,,]))
  feature <- cbind(feature, yy_cor)
}

# if ("snr" %in% xvars) {
#   print("snr taken")
#   feature <- cbind(feature, snr_arr[1,,])
# }

# if ("region_volume" %in% xvars) {
#   print("region_volume taken")
#   feature <- cbind(feature, region_volume)
# }

# if ("period" %in% xvars) {
#   print("period taken")
#   feature <- cbind(feature, timeseries_period_chunks)
# }
# if ("acf_diff" %in% xvars) {
#   print("acf diff taken")
#   feature <- cbind(
#     feature,
#     acf2_diff3, acf3_diff3, acf2_diff4, acf3_diff4
#   )
# }
# 
# if ("yy_chatterjee_full" %in% xvars) {
#   print("yy_chatterjee_full taken")
#   feature <- cbind(
#     feature,
#     yy_chatterjee_full
#   )
# }
# 
# if ("yy_chatterjee_ul" %in% xvars) {
#   print("yy_chatterjee_ul taken")
#   feature <- cbind(
#     feature,
#     yy_chatterjee_ul
#   )
# }
# 
# if ("yy_chatterjee_max" %in% xvars) {
#   print("yy_chatterjee_max taken")
#   feature <- cbind(
#     feature,
#     yy_chatterjee_max
#   )
# }
# 
# if ("chatterjee_acf" %in% xvars) {
#   print("chatterjee_acf taken")
#   feature <- cbind(feature, chatterjee_acf1, chatterjee_acf2, chatterjee_acf3)
# }
# 
# if ("ccf0" %in% xvars) {
#   print("ccf0 taken")
#   feature <- cbind(feature, ccf0)
# }
# 
# if ("ccf1" %in% xvars) {
#   print("ccf1 taken")
#   feature <- cbind(feature, ccf1)
# }
# 
# if ("ccf2" %in% xvars) {
#   print("ccf2 taken")
#   feature <- cbind(feature, ccf2)
# }

# if ("ccf3" %in% xvars) {
#   print("ccf3 taken")
#   feature <- cbind(feature, ccf3)
# }

# load dependent variable
unrestricted_elvisha <- read.csv(paste0(
  dir.data,
  "/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"
))

restricted_elvisha <- read.csv(paste0(
  dir.data,
  "/Elvisha/dem/RESTRICTED_elvisha9_3_9_2021_10_38_4.csv"
))





print(paste("variable", variable))

elvisa_cognition <- unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>% # only subjects in feature
  select(Subject, eval(variable), Age, Gender)

elvisa_cognition$Gender[elvisa_cognition$Gender == "M"] <- 1
elvisa_cognition$Gender[elvisa_cognition$Gender == "F"] <- 0
elvisa_cognition$Gender <- as.numeric(elvisa_cognition$Gender)

elvisa_cognition$Age <- as.factor(elvisa_cognition$Age)
elvisa_cognition$Age <- as.numeric(elvisa_cognition$Age)

elvisa_cognition <- elvisa_cognition %>% mutate(
  combined = (10 * Age) + Gender
)
elvisa_cognition$combined <- as.factor(elvisa_cognition$combined)
elvisa_cognition$combined <- as.numeric(elvisa_cognition$combined)

# removing subjects that do not have cognition scores
nonnanindex <- !is.na(elvisa_cognition[, 2])

transformed_elvisha <- restricted_elvisha %>%
  mutate(twin_status = (ZygositySR != "NotTwin")) %>%
  filter(Subject %in% subject_id_all) %>%
  select(Subject, twin_status, Mother_ID, Father_ID) %>%
  mutate(Parent_ID = Mother_ID*(10^5)+Father_ID) %>%
  select(Subject , Parent_ID)%>%
  slice(which(nonnanindex))

label <- as.numeric(elvisa_cognition[, 2])
new_subject_id <- as.numeric(elvisa_cognition$Subject)

Combined <- as.numeric(elvisa_cognition$Combined)
Combined <- Combined[nonnanindex]

Gender <- as.numeric(elvisa_cognition$Gender)
Gender <- Gender[nonnanindex]

Age <- as.numeric(elvisa_cognition$Age)
Age <- Gender[nonnanindex]

label <- label[nonnanindex]
new_subject_id <- new_subject_id[nonnanindex]
feature <- feature[nonnanindex, ]

# normlize feature (if we want try this in future)
# feature = t(t(feature)/colSums(feature))

# standardize feature

# feature <- data.frame(feature)
# feature <- scale(feature)
feature <- as.matrix(feature)

# print("mean and standart error for crosscheck")
# print(colMeans(feature)) # faster version of apply(scaled.dat, 2, mean)
# print(apply(feature, 2, sd))

print("number of variable")
print(dim(feature)[2] / 86)


mydata <- cbind(new_subject_id, label, feature)



getdata <- function(index, splitratio) {
  set.seed(seeds[1])
  if (splitBy == "Parent") {
    parent_split <- sample.split(unique(transformed_elvisha$Parent_ID), SplitRatio = splitratio)
    transformed_elvisha = transformed_elvisha%>%
      mutate(split = Parent_ID %in% unique(transformed_elvisha$Parent_ID)[parent_split==TRUE])
    split = transformed_elvisha$split
    print(split)
  }else if (splitBy == "Normal") {
    split <- sample.split(mydata[, 2], SplitRatio = splitratio)
  } else if (splitBy == "Combined") {
    split <- sample.split(Combined, SplitRatio = splitratio)
  } else if (splitBy == "Gender") {
    split <- sample.split(Gender, SplitRatio = splitratio)
  } else if (splitBy == "Age") {
    split <- sample.split(Age, SplitRatio = splitratio)
  }
  print(dim(mydata))
  print(length(split))
  train_data <- subset(mydata, split == "TRUE")
 
  test_data <- subset(mydata, split == "FALSE")
  
  set.seed(seeds[index])
  for(i in 1:j) {
    train_minus <- sample.int(dim(train_data)[1],4)
    train_data <- train_data[-train_minus]
    test_minus <- sample.int(dim(test_data)[1],1)
    print("test_minus")
    print(test_minus)
    print(test_data[test_minus, 1])
    test_data <- test_data[-test_minus]
  } 
  print("test train dims")
  print(dim(test_data))
  print(dim(train_data))
  
  print("split ratio")
  print(sum(as.numeric( split))/length(split))
  feature_train <- train_data[, 3:dim(train_data)[2]]
  label_train <- train_data[, 2]
  feature_test <- test_data[, 3:dim(train_data)[2]]
  label_test <- test_data[, 2]



  return(list("train_data" = train_data, "test_data" = test_data, "feature_train" = feature_train, "feature_test" = feature_test, "label_train" = label_train, "label_test" = label_test))
}
