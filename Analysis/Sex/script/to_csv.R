dir.analysis <- "./Analysis"
dir.temp <- paste0(dir.analysis, "/Sex/Temp")
folder_name <- glue::glue(
  "splitBy_{splitBy}/elastic/split_{splitratio}/mix_{mix}/{x_var_name}"
)
folder_path <- paste0(dir.temp, "/family_split/", folder_name)
file_path <- paste0(folder_path, "/all_coefficients.RData")

abs_coeff <- abs(all_coeffs[3,])

write.table(abs_coeff, paste0(folder_path, "/abs.csv"),sep=",",col.names = FALSE, row.names = FALSE)

