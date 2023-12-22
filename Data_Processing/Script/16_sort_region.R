dir.data<-"./Data"
dir.processing <- "./Data_Processing"
hcp.dir <- "./Data/Elvisha/"
yeo.names <- as.vector(readMat(paste0(hcp.dir, "yeo/map_fs86_to_yeo7_weighted_sym.mat"))$winner.vec)
region_name <- read.table(paste0(dir.data,"/regions.txt"), header = FALSE)$V1
left <- 1
right <- 10
lh <- 19
rh <- 53
permutation <- array(data = 0,dim = 86)

for(i in 1:9){
  permutation[2*i -1] = i
  permutation[2*i] = i+9 
}
for(i in 1:34){
  permutation[2*i -1 + 18] =i+18 
  permutation[2*i + 18] = i+18+34 
}



save(permutation, file = paste0(dir.processing,"/Output/symmetric_permutation.RData"))