library(dplyr)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_output <- paste0(dir_processing, "/Output")

load(paste0(dir_output, "/subject_id_all.RData"))

restricted_elvisha <- read.csv(paste0(
  dir_data,
  "/Elvisha/dem/RESTRICTED_elvisha9_3_9_2021_10_38_4.csv"
))


transformed_elvisha <- restricted_elvisha %>%
  mutate(twin_status = (ZygositySR != "NotTwin")) %>%
  filter(Subject %in% subject_id_all) %>%
  select(Subject, twin_status, Mother_ID, Father_ID) %>%
  mutate(Twin_ID = Mother_ID*(10^5)+Father_ID)

# transformed_elvisha <- restricted_elvisha %>%
#   mutate(twin_status = (ZygositySR != "NotTwin")) %>%
#   select(Subject, twin_status, Mother_ID)

not_twin <- transformed_elvisha %>%
  filter(twin_status == FALSE)
twin <- transformed_elvisha %>%
  filter(twin_status == TRUE)

# filtered_twin <- twin %>%
#   group_by(Mother_ID) %>%
#   slice(1) %>%
#   ungroup()

filtered_not_twin <- twin %>%
  group_by(Twin_ID) %>%
  filter(n()==1)%>%
  ungroup()%>%
  select(Subject)

filtered_not_twin <- as.matrix(filtered_not_twin)[,1]

filtered_twin_first <- twin %>%
  group_by(Twin_ID) %>%
  filter(n()>1)%>%
  slice(1) %>%
  ungroup()%>%
  select(Subject)

filtered_twin_second <- twin %>%
  group_by(Twin_ID) %>%
  filter(n()>1)%>%
  slice(2) %>%
  ungroup()%>%
  select(Subject)

filtered_twin_first <- as.matrix(filtered_twin_first)[,1]
filtered_twin_second <- as.matrix(filtered_twin_second)[,1]

not_twin_id <- c(not_twin$Subject,filtered_not_twin)


twin_filtered_id_first <- c(filtered_twin_first, not_twin_id)
first_twin_subject_id <- intersect(twin_filtered_id_first, subject_id_all)
save(first_twin_subject_id,
  file = paste0(dir_output, "/first_twin_subject_id.RData")
)

twin_filtered_id_second <- c(filtered_twin_second, not_twin_id)
second_twin_subject_id <- intersect(twin_filtered_id_second, subject_id_all)
save(second_twin_subject_id,
     file = paste0(dir_output, "/second_twin_subject_id.RData")
)
