# variable <- "CogTotalComp_AgeAdj"
splitBy <- "Normal"

xvars <- c("intercept", "ar_coef","residual_sd","yycor")
# xvars <- c("intercept", "ar_coef","residual_sd")
# xvars <- c("intercept","residual_sd", "yycor")

# xvars <- c("ar_coef", "yycor")
# xvars <- c("residual_sd","ar_coef")
# xvars <- c("intercept", "ar_coef")
# xvars <- c("intercept", "residual_sd")

# xvars <- c("yycor")
# xvars <- c("residual_sd")
# xvars <- c("ar_coef")
# xvars <- c("intercept")

mix <- 0.5 # 1 for lasso, 0 for ridge
splitratio <- 0.8 # train/test
x_var_name <- paste(xvars, sep = "_", collapse = "_")