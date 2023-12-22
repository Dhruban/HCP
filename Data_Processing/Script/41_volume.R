library(gdata)
library(pracma)
library(dplyr)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")
dir_temp <- paste0(dir_processing, "/Temp")

dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
load(paste0(dir_output, "/subject_id_all.RData"))

source(paste0(dir_processing, "/Script/01_library.R"))
source(paste0(dir_processing, "/Script/03_functions.R"))

subject_size <- length(subject_id_all)

unrestricted_elvisha <- read.csv(paste0(
  dir_data,
  "/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"
))

elvisa_gender <- unrestricted_elvisha %>%
  # filter(Subject %in% subject_id) %>%
  filter(Subject %in% subject_id_all) %>%
  select(Subject, Gender)

elvisa_gender$Gender[elvisa_gender$Gender == "M"] <- 1
elvisa_gender$Gender[elvisa_gender$Gender == "F"] <- 0

elvisa_vol <- array(0 , dim = subject_size)

count <- 0 
for (elvisa_id in elvisa_gender$Subject){
  count <- count +1
  fname_timeseries <- paste0(
    dir_timeseries, "/",
    subject_id_all[count], "_TS.RData"
  )
  load(fname_timeseries)
  elvisa_vol[count] <- sum(volume[,1])
  if(elvisa_vol[count] == 0){
    print("error")
  }
}

elvisa_gender$vol <- elvisa_vol


elvisa_male <- filter(elvisa_gender, Gender == 1)$Subject
elvisa_female <- filter(elvisa_gender, Gender == 0)$Subject

elvisa_male_filtered <- c()
elvisa_female_filtered <- c()



for (male_id in elvisa_male){
  if (sum(elvisa_male_filtered == male_id) != 0 ) {next}
  for(female_id in elvisa_female){
   male_vol <-  filter(elvisa_gender, Subject ==male_id )$vol[1]
   female_vol <-  filter(elvisa_gender, Subject ==female_id )$vol[1]
   diff <- abs(male_vol - female_vol)
   
   if (sum(elvisa_female_filtered == female_id) != 0 ) {next}
   else if (diff < (male_vol * 0.01)){
     elvisa_male_filtered <- c(elvisa_male_filtered, male_id)
     elvisa_female_filtered <- c(elvisa_female_filtered, female_id)
     break
   }
   
  }
  cat(paste0(length(elvisa_male_filtered), ", "))
  
}

# for (female_id in elvisa_male){
#   if (sum(elvisa_female_filtered == female_id) != 0 ) {next}
#   for(male_id in elvisa_female){
#     male_vol <-  filter(elvisa_gender, Subject ==male_id )$vol[1]
#     female_vol <-  filter(elvisa_gender, Subject ==female_id )$vol[1]
#     diff <- abs(male_vol - female_vol)
#     if (sum(elvisa_male_filtered == male_id) != 0 ) {next}
#     if (diff < female_vol * 0.01){
#       elvisa_male_filtered <- c(elvisa_male_filtered, male_id)
#       elvisa_female_filtered <- c(elvisa_female_filtered, female_id)
#       break
#     }
#     
#   }
#   print(length(elvisa_male_filtered))
# }

elvisa_male_vol <- filter(elvisa_gender, Subject %in% elvisa_male_filtered )$vol
elvisa_female_vol <- filter(elvisa_gender, Subject %in% elvisa_female_filtered)$vol


