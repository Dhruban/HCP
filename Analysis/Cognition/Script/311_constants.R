variable <- "CogTotalComp_AgeAdj"
splitBy <- "Parent"

# xvars <- c("mean", "sd","pacf", "acf","yycor","zyycor", "snr",
# "acf_diff","region_volume","yy_chatterjee_full", "yy_chatterjee_ul",
# "yy_chatterjee_max","chatterjee_acf","ccf0","ccf1","ccf2","ccf3",
# "intercept", "ar_coef","residual_sd")

xvars <- c("mean","sd","acf","pacf")

mix <- 0.5 # 1 for lasso, 0 for ridge
splitratio <- 0.8 # train/test
x_var_name <- paste(xvars, sep = "_", collapse = "_")

dry_run <- TRUE
