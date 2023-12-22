dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"

dir.output <- paste0(dir.analysis,"/Cognition/Output")
dir.temp <- paste0(dir.analysis,"/Cognition/Temp")

# generate new seeds
seeds <- sample.int(99999,50)
write.csv(seeds,paste0(dir.temp,"/seed.txt"),row.names = FALSE,col.names = FALSE)

#read seed from temp (useful if r crashes)
seeds <- read.csv(paste0(dir.temp,"/seed.txt"))[,'x']

#read seed from output (useful to get the same result)
seeds <- read.csv(paste0(dir.output,"/seed.txt"))[,'x']
