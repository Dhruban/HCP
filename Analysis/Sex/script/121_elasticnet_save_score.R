mean_train_accu <- mean(train_scores_accu)
sd_train_accu <- sd(train_scores_accu)
mean_train_str_accu <- format(round(mean_train_accu, 2), nsmall = 2)
sd_train_str_accu <- format(round(sd_train_accu, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_accuracy_mean_sd")] <-
    add_entry(
        xvar_result[glue::glue("{splitBy}_{mix}_accuracy_mean_sd")],
        glue::glue(x_var_name),
        glue::glue("train_{splitratio}"),
        glue::glue("{mean_train_str_accu} ({sd_train_str_accu})")
    )

mean_test_accu <- mean(test_scores_accu)
sd_test_accu <- sd(test_scores_accu)
mean_test_str_accu <- format(round(mean_test_accu, 2), nsmall = 2)
sd_test_str_accu <- format(round(sd_test_accu, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_accuracy_mean_sd")] <-
    add_entry(
        xvar_result[glue::glue("{splitBy}_{mix}_accuracy_mean_sd")],
        glue::glue(x_var_name),
        glue::glue("test_{splitratio}"),
        glue::glue("{mean_test_str_accu} ({sd_test_str_accu})")
    )

mean_train_auc <- mean(train_scores_auc)
sd_train_auc <- sd(train_scores_auc)
mean_train_str_auc <- format(round(mean_train_auc, 2), nsmall = 2)
sd_train_str_auc <- format(round(sd_train_auc, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_auc_mean_sd")] <-
    add_entry(
        xvar_result[glue::glue("{splitBy}_{mix}_auc_mean_sd")],
        glue::glue(x_var_name),
        glue::glue("train_{splitratio}"),
        glue::glue("{mean_train_str_auc} ({sd_train_str_auc})")
    )


mean_test_auc <- mean(test_scores_auc)
sd_test_auc <- sd(test_scores_auc)
mean_test_str_auc <- format(round(mean_test_auc, 2), nsmall = 2)
sd_test_str_auc <- format(round(sd_test_auc, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_auc_mean_sd")] <-
    add_entry(
        xvar_result[glue::glue("{splitBy}_{mix}_auc_mean_sd")],
        glue::glue(x_var_name),
        glue::glue("test_{splitratio}"),
        glue::glue("{mean_test_str_auc} ({sd_test_str_auc})")
    )



median_train_accu <- median(train_scores_accu)
iqr_train_accu <- IQR(train_scores_accu)
median_train_str_accu <- format(round(median_train_accu, 2), nsmall = 2)
iqr_train_str_accu <- format(round(iqr_train_accu, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_accuracy_median_iqr")] <-
    add_entry(
        xvar_result[glue::glue("{splitBy}_{mix}_accuracy_median_iqr")],
        glue::glue(x_var_name),
        glue::glue("train_{splitratio}"),
        glue::glue("{median_train_str_accu} ({iqr_train_str_accu})")
    )

median_test_accu <- median(test_scores_accu)
iqr_test_accu <- IQR(test_scores_accu)
median_test_str_accu <- format(round(median_test_accu, 2), nsmall = 2)
iqr_test_str_accu <- format(round(iqr_test_accu, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_accuracy_median_iqr")] <-
    add_entry(
        xvar_result[glue::glue("{splitBy}_{mix}_accuracy_median_iqr")],
        glue::glue(x_var_name),
        glue::glue("test_{splitratio}"),
        glue::glue("{median_test_str_accu} ({iqr_test_str_accu})")
    )

median_train_auc <- median(train_scores_auc)
iqr_train_auc <- IQR(train_scores_auc)
median_train_str_auc <- format(round(median_train_auc, 2), nsmall = 2)
iqr_train_str_auc <- format(round(iqr_train_auc, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_auc_median_iqr")] <-
    add_entry(
        xvar_result[glue::glue("{splitBy}_{mix}_auc_median_iqr")],
        glue::glue(x_var_name),
        glue::glue("train_{splitratio}"),
        glue::glue("{median_train_str_auc} ({iqr_train_str_auc})")
    )

median_test_auc <- median(test_scores_auc)
iqr_test_auc <- IQR(test_scores_auc)
median_test_str_auc <- format(round(median_test_auc, 2), nsmall = 2)
iqr_test_str_auc <- format(round(iqr_test_auc, 2), nsmall = 2)
xvar_result[glue::glue("{splitBy}_{mix}_auc_median_iqr")] <-
    add_entry(
        xvar_result[glue::glue("{splitBy}_{mix}_auc_median_iqr")],
        glue::glue(x_var_name),
        glue::glue("test_{splitratio}"),
        glue::glue("{median_test_str_auc} ({iqr_test_str_auc})")
    )


# write temp files

# if we want to add timestamp
# nowtimestring =gsub(":","_",format(Sys.time(), "%Y_%b_%d_%X"))
# nowtimestring = glue::glue("{nowtimestring}_")
# otherwise
nowtimestring <- ""

train_accu_file_name <- paste0(nowtimestring, glue::glue("train_accu_result.csv"))
train_accu_file_path <- paste0(folder_path, "/", train_accu_file_name)
train_auc_file_name <- paste0(nowtimestring, glue::glue("train_auc_result.csv"))
train_auc_file_path <- paste0(folder_path, "/", train_auc_file_name)

test_accu_file_name <- paste0(nowtimestring, glue::glue("test_accu_result.csv"))
test_accu_file_path <- paste0(folder_path, "/", test_accu_file_name)
test_auc_file_name <- paste0(nowtimestring, glue::glue("test_auc_result.csv"))
test_auc_file_path <- paste0(folder_path, "/", test_auc_file_name)

write.csv(train_scores_accu, train_accu_file_path)
write.csv(test_scores_accu, test_accu_file_path)
write.csv(train_scores_auc, train_auc_file_path)
write.csv(test_scores_auc, test_auc_file_path)
# write result
# write.csv(result_sd_mean, path.result, row.names = FALSE)
write_xlsx(xvar_result, path_xvar_result)
write.csv(seeds, paste0(dir.output, "/seed.txt"), row.names = FALSE, col.names = FALSE)
