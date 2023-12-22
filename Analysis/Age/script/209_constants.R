# variable <- "CogTotalComp_AgeAdj"
splitBy <- "Normal"

# xvars <- c("mean", "sd","snr","pacf","acf","yycor")
# xvars <- c("mean", "sd","snr","pacf","acf")
# xvars <- c("mean", "sd","acf","pacf","yycor")
# xvars <- c("mean", "sd","snr","pacf")
xvars <- c("mean", "sd","acf","pacf")
# xvars <- c("mean", "sd","yycor")
# xvars <- c("snr","pacf")
# xvars <- c("snr","acf")
# xvars <- c("mean", "sd","snr")
# xvars <- c("mean", "sd","pacf")
# xvars <- c("mean", "sd","acf")
# xvars <- c("mean", "sd")
# xvars <- c("pacf", "acf")
# xvars <- c("acf","yycor")
# xvars <- c("pacf","yycor")
# xvars <- "snr"
# xvars <- "mean"
# xvars <- "sd"
# xvars <- "yycor"
# xvars <-"acf"
# xvars <- "pacf"

mix <- 0.5 # 1 for lasso, 0 for ridge
splitratio <- 0.8 # train/test
x_var_name <- paste(xvars, sep = "_", collapse = "_")