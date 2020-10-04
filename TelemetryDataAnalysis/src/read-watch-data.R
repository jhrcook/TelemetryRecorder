

get_data_file_names <- function(dir) {
  list.files(dir, pattern = "json$", full.names = TRUE)
}


telemetry_json_to_dataframe <- function(j) {
  
  motion_type_labels <- tibble(
    motion = c(rep("acceleration", 3), rep("attitude", 3)),
    axis = c("x", "y", "z", "pitch", "roll", "yaw")
  )
  
  map_dfr(j$telemetryData, as_tibble) %.% {
    mutate(idx = row_number())
    janitor::clean_names()
    pivot_longer(
      -c(idx, date),
      names_to = "axis",
      values_to = "value"
    )
    mutate(axis = str_remove(axis, "^accel_"))
    left_join(motion_type_labels, by = "axis")
  }
}


workout_json_to_dataframe <- function(j) {
  
  d <- j$workoutData
  if (length(d) == 0) {
    return(tibble())
  }
  
  map_dfr(d, as_tibble) %.% {
    janitor::clean_names()
    mutate(
      date = as.numeric(date),
      value = as.numeric(value),
      quantity_type = str_remove(quantity_type, "HKQuantityTypeIdentifier")
    )
    group_by(quantity_type)
    mutate(scaled_value = scales::rescale_max(value))
    ungroup()
  }
}


read_telemetry_data_file <- function(f) {
  rjson::fromJSON(file = f)
}


parse_data_file_name <- function(fn) {
  fn_base <- tools::file_path_sans_ext(basename(fn))
  fn_data <- unlist(str_split_fixed(fn_base, "_", 4)[1, ])
  tibble(
    exercise = fn_data[[2]],
    reps = fn_data[[3]],
    date = lubridate::ymd_hms(fn_data[[4]]),
  )
}


# Read watch data in as a list of 3 data frames.
read_watch_data <- function(fn) {
  j <- read_telemetry_data_file(fn)
  
  telemetry_data <- telemetry_json_to_dataframe(j)
  workout_data <- workout_json_to_dataframe(j)
  
  start_date <- min(c(telemetry_data$date, workout_data$date))
  telemetry_data$date <- telemetry_data$date - start_date
  workout_data$date <- workout_data$date - start_date
  
  return(list(
    meta_data = parse_data_file_name(fn),
    telemetry_data = telemetry_data,
    workout_data = workout_data
  ))
}
