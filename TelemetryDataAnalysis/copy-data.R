#!/usr/bin/env Rscript

icloud_dir <- "~/Library/Mobile Documents/iCloud~TelemetryRecorder/Documents"
data_dir <- "data"

num_files_copied <- 0
for (file in list.files(icloud_dir, pattern = "json$", full.names = TRUE)) {
  new_file <- file.path(data_dir, basename(file))
  if (!file.exists(new_file)) {
    file.copy(file, new_file)
    num_files_copied <- num_files_copied + 1
  }
}

message(paste0("Copied ", num_files_copied, " data files."))
