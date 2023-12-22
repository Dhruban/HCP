library("dplyr")
dir.data <- "./Data" 
dir.processing <- "./Data_Processing"

load(paste0(dir.processing,"/Output/id.RData"))

unrestricted_elvisha <- read.csv(paste0(dir.data, 
                                        "/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"))

elvisa_volume <- unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>% # only subjects in feature
  select(Subject, FS_IntraCranial_Vol, Gender)

nonnanindex <- !is.na(elvisa_volume[, 2])


elvisa_volume$Gender <- as.numeric(elvisa_volume$Gender=="M")

gender <- elvisa_volume$Gender

elvisha_male <- elvisa_volume[gender==1,]
elvisha_female <- elvisa_volume[gender==0,]

load(paste0(dir.processing, "/Output/pacf1.RData"))
load(paste0(dir.processing, "/Output/pacf2.RData"))
load(paste0(dir.processing, "/Output/pacf3.RData"))

mean_abs_pacf1 <- apply(timeseries_pacf1, 1, FUN=function(v){return(mean(v))})
mean_abs_pacf2 <- apply(timeseries_pacf2, 1, FUN=function(v){return(mean(v))})
mean_abs_pacf3 <- apply(timeseries_pacf3, 1, FUN=function(v){return(mean(v))})

mean_abs_pacf1_male <- mean_abs_pacf1[gender==1]
mean_abs_pacf2_male <- mean_abs_pacf2[gender==1]
mean_abs_pacf3_male <- mean_abs_pacf3[gender==1]

mean_abs_pacf1_female <- mean_abs_pacf1[gender==0]
mean_abs_pacf2_female <- mean_abs_pacf2[gender==0]
mean_abs_pacf3_female <- mean_abs_pacf3[gender==0]


hist(mean_abs_pacf1_male)
hist(mean_abs_pacf1_female)
hist(elvisha_male$FS_IntraCranial_Vol)
hist((elvisha_female$FS_IntraCranial_Vol))

print("male_pacf_1")
print(cor(elvisha_male$FS_IntraCranial_Vol, mean_abs_pacf1_male))
plot(elvisha_male$FS_IntraCranial_Vol,mean_abs_pacf1_male )

print("male_pacf_2")
print(cor(elvisha_male$FS_IntraCranial_Vol, mean_abs_pacf2_male))

print("male_pacf_3")
print(cor(elvisha_male$FS_IntraCranial_Vol, mean_abs_pacf3_male))

print("female_pacf_1")
print(cor(elvisha_female$FS_IntraCranial_Vol, mean_abs_pacf1_female))
plot(elvisha_female$FS_IntraCranial_Vol, mean_abs_pacf1_female)

print("female_pacf_2")
print(cor(elvisha_female$FS_IntraCranial_Vol, mean_abs_pacf2_female))

print("female_pacf_3")
print(cor(elvisha_female$FS_IntraCranial_Vol, mean_abs_pacf3_female))

print("combined_pacf_1")
print(cor(elvisa_volume$FS_IntraCranial_Vol, mean_abs_pacf1))
plot(elvisa_volume$FS_IntraCranial_Vol, mean_abs_pacf1 , col= gender+1)

print("combined_pacf_2")
print(cor(elvisa_volume$FS_IntraCranial_Vol, mean_abs_pacf2))
plot(elvisa_volume$FS_IntraCranial_Vol, mean_abs_pacf2, col = gender+1)

print("combined_pacf_3")
print(cor(elvisa_volume$FS_IntraCranial_Vol, mean_abs_pacf3))