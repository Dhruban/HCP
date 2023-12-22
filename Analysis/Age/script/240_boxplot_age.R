library(tidyverse)
library(glmnet)
library(ROCR)

dir.data <- "./Data" 
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"
dir.output <- paste0(dir.analysis,"/Age/Output")
plot_file = paste0(dir.output,'/Age_boxplots.pdf')

# Load Feature
load(paste0(dir.processing,"/Output/id.RData"))
load(paste0(dir.processing,"/Output/pacf1.RData"))
load(paste0(dir.processing,"/Output/pacf2.RData"))
load(paste0(dir.processing,"/Output/pacf3.RData"))

feature <- cbind(timeseries_pacf1, timeseries_pacf2, timeseries_pacf3)

# load dependent variable
unrestricted_elvisha <- read.csv(paste0(dir.data,"/Elvisha/dem/unrestricted_elvisha9_3_9_2021_10_37_53.csv"))
elvisa_age <-  unrestricted_elvisha %>%
  filter(Subject %in% subject_id) %>%
  select(Subject, Age, Gender)

elvisa_age$Age <- as.factor(elvisa_age$Age)
elvisa_age$Age <- as.numeric(elvisa_age$Age)

label <- as.numeric(elvisa_age$Age)
subject_id <- as.numeric(subject_id)
Gender <- as.numeric(elvisa_age$Gender=="M") #1 for male, 0 for female
region_name <- read.table(paste0(dir.data,"/region_names.txt"), header = FALSE)$V1
region_size <- 86
name <- c("subject_id","label","Gender",region_name)
col_names <- c("male","female","combined")

#creating pdf of the boxplots
pdf(file=plot_file,width=9,height=10)
for(i in 1:region_size){
  par(mfrow = c(3,3))
  for(j in 1:3){
    for (k in 1:3) {
      mydata <- cbind(subject_id, label, Gender, feature[,((j-1)*region_size+1):(j*region_size)])
      mydata=mydata[mydata[,2]!=4,]
      mydata= mydata[mydata[,3]!=(k-1),]  
      colnames(mydata) =  name
      lab_y=paste0("Pacf values lag ",j)
      boxplot(mydata[,(i+3)] ~ label, data = mydata, xlab = "Age_labels",ylab = lab_y, main = paste0(name[(i+3)]," ",col_names[k]), names = c("22-25","26-30","31-35"))
    }
  }
}
#turn off PDF plotting
dev.off() 
