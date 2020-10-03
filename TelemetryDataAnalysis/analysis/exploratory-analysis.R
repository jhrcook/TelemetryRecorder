#!/usr/bin/env Rscript

source("start.R")


#### ---- Data prepration ---- ####

get_data_file_names <- function(dir) {
  list.files(dir, pattern = "json$", full.names = TRUE)
}


telemetry_json_to_dataframe <- function(j) {
  map_dfr(j$telemetryData, as_tibble) %.% {
    mutate(idx = row_number())
    janitor::clean_names()
    pivot_longer(
      -c(idx, date),
      names_to = "axis",
      values_to = "value"
    )
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


#### ---- Plotting ---- ####

line_plot_telemetry <- function(df) {
  df %>%
    ggplot(aes(idx, value)) +
    facet_wrap(~axis, ncol = 1, scales = "free_y") +
    geom_line(aes(color = axis))
}



all_data_files <- get_data_file_names(data_dir)
raw_pushup_data <- read_watch_data(all_data_files[2])

raw_pushup_telemetry_data <- raw_pushup_data$telemetry_data

line_plot_telemetry(raw_pushup_telemetry_data)


clean_pushup_data <- raw_pushup_telemetry_data %>%
  filter(between(idx, 250, 1400))
line_plot_telemetry(clean_pushup_data)


#### ---- Hidden Markov Modeling ---- ####

library(depmixS4)
set.seed(0)

pivot_telemetry_data <- function(telemetry_data) {
  telemetry_data %>%
    pivot_wider(
      c(date, idx),
      names_from = axis,
      values_from = value
    )
}

pushup_data_wide <- pivot_telemetry_data(clean_pushup_data)


construct_full_telemetry_hmm_model <- function(d, nstates) {
  depmix(
    list(
      accel_x ~ 1,
      accel_y ~ 1,
      accel_z ~ 1,
      pitch ~ 1,
      roll ~ 1,
      yaw ~ 1
    ),
    nstates = nstates,
    family = list(
      gaussian(), gaussian(), gaussian(),
      gaussian(), gaussian(), gaussian()
    ),
    data = d
  )
}


construct_accel_telemetry_hmm_model <- function(d, nstates) {
  depmix(
    list(
      accel_x ~ 1,
      accel_y ~ 1,
      accel_z ~ 1
    ),
    nstates = nstates,
    family = list(gaussian(), gaussian(), gaussian()),
    data = d
  )
}


construct_attitude_telemetry_hmm_model <- function(d, nstates) {
  depmix(
    list(
      pitch ~ 1,
      roll ~ 1,
      yaw ~ 1
    ),
    nstates = nstates,
    family = list(gaussian(), gaussian(), gaussian()),
    data = d
  )
}


m2_full <- construct_full_telemetry_hmm_model(pushup_data_wide, 2)
m3_full <- construct_full_telemetry_hmm_model(pushup_data_wide, 3)

m2_accel <- construct_accel_telemetry_hmm_model(pushup_data_wide, 2)
m3_accel <- construct_accel_telemetry_hmm_model(pushup_data_wide, 3)

m2_att <- construct_attitude_telemetry_hmm_model(pushup_data_wide, 2)
m3_att <- construct_attitude_telemetry_hmm_model(pushup_data_wide, 3)


m2_full_fit <- fit(m2_full)
m3_full_fit <- fit(m3_full)
m2_accel_fit <- fit(m2_accel)
m3_accel_fit <- fit(m3_accel)
m2_att_fit <- fit(m2_att)
m3_att_fit <- fit(m3_att)

AIC(
  m2_full_fit, m3_full_fit,
  m2_accel_fit, m3_accel_fit,
  m2_att_fit, m3_att_fit
)
BIC(
  m2_full_fit, m3_full_fit,
  m2_accel_fit, m3_accel_fit,
  m2_att_fit, m3_att_fit
)

hmm_states_p <- posterior(m2_full_fit) %>%
  as_tibble() %>%
  mutate(idx = row_number()) %>%
  pivot_longer(-c(idx, state)) %>%
  ggplot(aes(x = idx, y = value, color = name)) +
  geom_line(size = 2, alpha = 0.8) +
  scale_color_manual(values = c("purple", "orange")) +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
  ) +
  labs(
    x = "data point",
    y = "probability of state",
    color = "state",
    title = "Hidden Markov Model"
  )

data_p <- clean_pushup_data %>%
  mutate(motion_type = str_to_title(axis)) %>%
  line_plot_telemetry() +
  labs(
    x = NULL,
    y = "value"
  ) +
  theme(
    strip.text = element_text(size = 11, face = "bold")
  )

patch <- data_p / hmm_states_p +
  plot_layout(heights = c(4, 1))

ggsave(
  file.path("graphs", "pushup-hmm.png"),
  patch,
  width = 8, height = 5, unit = "in",
  dpi = 500
)






raw_pushup_wide <- pivot_telemetry_data(raw_pushup_telemetry_data)

m2_full_raw <- construct_full_telemetry_hmm_model(
  d = raw_pushup_wide,
  nstates = 2
)
m2_full_raw_fit <- fit(m2_full_raw)

m3_full_raw <- construct_full_telemetry_hmm_model(
  d = raw_pushup_wide,
  nstates = 3
)
m3_full_raw_fit <- fit(m3_full_raw)

m4_full_raw <- construct_full_telemetry_hmm_model(
  d = raw_pushup_wide,
  nstates = 4
)
m4_full_raw_fit <- fit(m4_full_raw)

m5_full_raw <- construct_full_telemetry_hmm_model(
  d = raw_pushup_wide,
  nstates = 5
)
m5_full_raw_fit <- fit(m5_full_raw)

AIC(
  m2_full_raw_fit, m3_full_raw_fit, 
  m4_full_raw_fit, m5_full_raw_fit
)

BIC(
  m2_full_raw_fit, m3_full_raw_fit, 
  m4_full_raw_fit, m5_full_raw_fit
)



hmm_states_p <- posterior(m4_full_raw_fit) %>%
  as_tibble() %>%
  mutate(idx = row_number()) %>%
  pivot_longer(-c(idx, state)) %>%
  ggplot(aes(x = idx, y = value, color = name)) +
  facet_wrap(~ name, ncol = 1) +
  geom_line(size = 2, alpha = 0.8) +
  scale_color_brewer(type = "qual", palette = "Set1") +
  theme(
    plot.title = element_text(hjust = 0.5, size = 11, face = "bold")
  ) +
  labs(
    x = "data point",
    y = "probability of state",
    color = "state",
    title = "Hidden Markov Model states"
  )

data_p <- raw_pushup_telemetry_data %>%
  mutate(motion_type = str_to_title(axis)) %>%
  line_plot_telemetry() +
  labs(
    x = NULL,
    y = "value"
  ) +
  theme(
    strip.text = element_text(size = 11, face = "bold")
  )

patch <- data_p / hmm_states_p +
  plot_layout(heights = c(6, 4))
patch




# TO TRY: 
# - smooth data
# - scale data to be Gaussian


# TODO:
# - tidy up this script
# - move to a R Markdown so that the results can be seen on GitHub.
