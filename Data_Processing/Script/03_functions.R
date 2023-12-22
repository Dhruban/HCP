create_dir <- function(folder_path) {
    if (!file.exists(folder_path)) {
        dir.create(folder_path)
        print(paste0(folder_path, " created successfully."))
    } else {
        print(paste0(folder_path, " already exists."))
    }
}
