library(tidyverse)
dir.data <- "./Data"
dir_processing_output <- "./Data_Processing/Output"

load(paste0(dir_processing_output,"/subject_id_all.RData"))

load(paste0(dir_processing_output,"/id.RData"))
load(paste0(dir_processing_output,"/filtered_id.RData"))
load(paste0(dir_processing_output,"/subject_id_no_trend.RData"))

load(paste0(dir_processing_output, "/mean_all.RData"))
restricted_elvisha <- read.csv(paste0(
  dir.data,
  "/Elvisha/dem/RESTRICTED_elvisha9_3_9_2021_10_38_4.csv"
))

transformed_elvisha <- restricted_elvisha %>%
  mutate(twin_status = (ZygositySR != "NotTwin")) %>%
  filter(Subject %in% subject_id_all) %>%
  select(Subject, twin_status, Mother_ID, Father_ID) %>%
  mutate(Twin_ID = Mother_ID*(10^5)+Father_ID)

twin <- transformed_elvisha %>%
  filter(twin_status == TRUE)

filtered_twin <- twin %>%
  group_by(Twin_ID) %>%
  filter(n()>1)%>%
  ungroup()%>%
  select(Subject)
filtered_twin_array <- as.matrix(filtered_twin)[,1]

time_mean <- timeseries_mean_arr[1,,]

time_mean_df <- as_tibble(time_mean, co)

restricted_elvisha <- as.data.frame(restricted_elvisha) %>%
  select(Subject,Age_in_Yrs,ZygositySR, Family_ID, Mother_ID, Father_ID) %>%
  filter(Subject %in% subject_id_all) %>%
  mutate(Volume_Matched = Subject %in% filtered_id) %>%
  mutate(Trend = !Subject %in% subject_id_no_trend) %>%
  mutate(Twin = Subject %in% filtered_twin_array) %>%
  bind_cols(time_mean_df)

write.csv(restricted_elvisha,paste0(dir_processing_output, "/indicator_restricted.csv"))

