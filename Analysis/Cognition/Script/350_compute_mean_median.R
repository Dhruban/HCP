dir.data <- "./Data"
dir.processing <- "./Data_Processing"
dir.analysis <- "./Analysis"

dir.output <- paste0(dir.analysis, "/Cognition/output")
dir.temp <- paste0(dir.analysis, "/Cognition/temp")


folder_name <- glue::glue(
  "{variable}/splitBy_{splitBy}/elastic/split_{splitratio}/mix_{mix}/{x_var_name}"
)

load(paste0(dir.temp, "/", folder_name, "/all_coefficients.RData"))
absolute_coef <- abs(all_coeffs)
mean_abs <- apply(absolute_coef, 2, FUN = function(v) {
  return(mean(v))
})
mean_file_name <- paste0(dir.temp, "/", folder_name, "/abs.csv")
write.table(mean_abs,
  file = mean_file_name, sep = ",", row.names = FALSE, col.names = FALSE
)
