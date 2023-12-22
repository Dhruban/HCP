library(gdata)
library(pracma)

dir_data <- "./Data"
dir_processing <- "./Data_Processing"
dir_timeseries <- paste0(dir_data, "/Elvisha/timeseries/RData")
dir_temp <- paste0(dir_processing, "/Temp")

load(paste0(dir_processing, "/Output/subject_id_all.RData"))
subject_size <- length(subject_id_all)

for (i in 1:subject_size) { # length of subject_list
    fname_timeseries <- paste0(
        dir_timeseries,
        "/", subject_id_all[i], "_TS.RData"
    )

    if (file.exists(fname_timeseries)) {
        load(fname_timeseries)

        rest1_lr <- timeseries[1:4800, ]
        pdf(
            file = paste0(
                dir_temp, "/timeseries_full/",
                subject_id_all[i], ".pdf"
            ),
            width = 10, height = 5
        )
        for (region in seq_len(dim(rest1_lr)[2])) {
            plot(seq.int(from = 1, to = 4800), rest1_lr[, region],
                type = "l", ylab = paste0("region ", region), xlab = "time"
            )
        }
        dev.off()
    }
    print(fname_timeseries)
}
