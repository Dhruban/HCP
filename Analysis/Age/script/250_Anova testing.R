library(tidyverse)
library(glmnet)
library(ROCR)
# library(caTools)
# library(reshape2)
# library(ggplot2)



load("./Output/id.RData")
load("./Output/pacf1.RData")
load("./Output/pacf2.RData")
load("./Output/pacf3.RData")
feature <- cbind(timeseries_pacf1, timeseries_pacf2, timeseries_pacf3)
unrestricted_elvisha <- read.csv("./Temp/unrestricted_elvisha9_3_9_2021_10_37_53.csv")
elvisa_age <-  unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>%
  select(Subject, Age, Gender)

elvisa_age$Age <- as.factor(elvisa_age$Age)
elvisa_age$Age <- as.numeric(elvisa_age$Age)

label <- as.numeric(elvisa_age$Age)
subject_id <- as.numeric(subject_id)
Gender <- as.numeric(elvisa_age$Gender=="M")
region_name <- read.table("./Temp/region_names.txt", header = FALSE)$V1
name <- c("subject_id","label","Gender",region_name)
Anova_p<- array(0,dim=c(86,9))
Anova_p<- as.matrix(Anova_p)
colnames(Anova_p) = c("Complete_lag_1","Complete_lag_2","Complete_lag_3","Male_lag_1","Male_lag_2","Male_lag_3","Female_lag_1","Female_lag_2","Female_lag_3")
for(k in 1:9){
  r=k %% 3
  if(r==0){r=3}
  mydata <- cbind(subject_id, label, Gender, feature[,((r-1)*86+1):(r*86)])
  mydata=mydata[mydata[,2]!=4,]
  colnames(mydata) =  name
  if(k>=4){
    if(k>=7){
      mydata= mydata[mydata[,3]==0,]
    }else{
      mydata= mydata[mydata[,3]==1,]
    }
  }
  for(i in 1:86){
    mydata=data.frame(mydata)
    res.aov <- aov(mydata[,(i+3)] ~ factor(label), data = mydata)
    Anova_p[i,k] <- summary(res.aov)[[1]][["Pr(>F)"]][1]
  }
}
library(MASS)
write.matrix(Anova_p,file="./Output/Anova_p_values.csv",sep=",")
