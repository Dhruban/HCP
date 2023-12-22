mean_train <- mean(train_scores)
sd_train <- sd(train_scores)
mean_train_str <- format(round(mean_train, 2), nsmall = 2)
sd_train_str <- format(round(sd_train, 2), nsmall = 2)

result[glue::glue("{splitBy}_mean_sd")] <-
  add_entry(
    result[glue::glue("{splitBy}_mean_sd")],
    glue::glue("elastic(mix={mix})"),
    glue::glue("train_{splitratio}"),
    glue::glue("{mean_train_str} ({sd_train_str})")
  )

mean_test <- mean(test_scores)
sd_test <- sd(test_scores)
mean_test_str <- format(round(mean_test, 2), nsmall = 2)
sd_test_str <- format(round(sd_test, 2), nsmall = 2)
result[glue::glue("{splitBy}_mean_sd")] <-
  add_entry(
    result[glue::glue("{splitBy}_mean_sd")],
    glue::glue("elastic(mix={mix})"),
    glue::glue("test_{splitratio}"),
    glue::glue("{mean_test_str} ({sd_test_str})")
  )

median_train <- median(train_scores)
iqr_train <- IQR(train_scores)
median_train_str <- format(round(median_train, 2), nsmall = 2)
iqr_train_str <- format(round(iqr_train, 2), nsmall = 2)
result[glue::glue("{splitBy}_median_iqr")] <-
  add_entry(
    result[glue::glue("{splitBy}_median_iqr")],
    glue::glue("elastic(mix={mix})"),
    glue::glue("train_{splitratio}"),
    glue::glue("{median_train_str} ({iqr_train_str})")
  )

median_test <- median(test_scores)
iqr_test <- IQR(test_scores)
median_test_str <- format(round(median_test, 2), nsmall = 2)
iqr_test_str <- format(round(iqr_test, 2), nsmall = 2)
result[glue::glue("{splitBy}_median_iqr")] <-
  add_entry(
    result[glue::glue("{splitBy}_median_iqr")],
    glue::glue("elastic(mix={mix})"),
    glue::glue("test_{splitratio}"),
    glue::glue("{median_test_str} ({iqr_test_str})")
  )


# xvar

mean_train <- mean(train_scores)
sd_train <- sd(train_scores)
mean_train_str <- format(round(mean_train, 2), nsmall = 2)
sd_train_str <- format(round(sd_train, 2), nsmall = 2)

xvar_result[glue::glue("{splitBy}_mean_sd")] <-
  add_entry(
    xvar_result[glue::glue("{splitBy}_mean_sd")],
    glue::glue(x_var_name),
    glue::glue("train_{splitratio}"),
    glue::glue("{mean_train_str} ({sd_train_str})")
  )

mean_test <- mean(test_scores)
sd_test <- sd(test_scores)
mean_test_str <- format(round(mean_test, 2), nsmall = 2)
sd_test_str <- format(round(sd_test, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_mean_sd")] <-
  add_entry(
    xvar_result[glue::glue("{splitBy}_mean_sd")],
    glue::glue(x_var_name),
    glue::glue("test_{splitratio}"),
    glue::glue("{mean_test_str} ({sd_test_str})")
  )

median_train <- median(train_scores)
iqr_train <- IQR(train_scores)
median_train_str <- format(round(median_train, 2), nsmall = 2)
iqr_train_str <- format(round(iqr_train, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_median_iqr")] <-
  add_entry(
    xvar_result[glue::glue("{splitBy}_median_iqr")],
    glue::glue(x_var_name),
    glue::glue("train_{splitratio}"),
    glue::glue("{median_train_str} ({iqr_train_str})")
  )

median_test <- median(test_scores)
iqr_test <- IQR(test_scores)
median_test_str <- format(round(median_test, 2), nsmall = 2)
iqr_test_str <- format(round(iqr_test, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_median_iqr")] <-
  add_entry(
    xvar_result[glue::glue("{splitBy}_median_iqr")],
    glue::glue(x_var_name),
    glue::glue("test_{splitratio}"),
    glue::glue("{median_test_str} ({iqr_test_str})")
  )



# write temp files

# if we want to add timestamp
# nowtimestring =gsub(":","_",format(Sys.time(), "%Y_%b_%d_%X"))
# nowtimestring = glue::glue("{nowtimestring}_")
# otherwise
nowtimestring <- ""

train_file_name <- paste0(nowtimestring, glue::glue("train_result.csv"))
train_file_path <- paste0(folder_path, "/", train_file_name)


test_file_name <- paste0(nowtimestring, glue::glue("test_result.csv"))
test_file_path <- paste0(folder_path, "/", test_file_name)

write.csv(train_scores, train_file_path)
write.csv(test_scores, test_file_path)

# write result
# write_xlsx(result, path.result)
write_xlsx(xvar_result, path_xvar_result)
# write.csv(seeds,
#  paste0(dir.output, "/seed.txt"), row.names = FALSE, col.names = FALSE)
