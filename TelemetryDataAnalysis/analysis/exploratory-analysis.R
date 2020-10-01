#!/usr/bin/env Rscript

source("start.R")


#### ---- Data preparation ---- ####

telemetry_json_to_tibble <- function(j) {
  enframe(j, name = "motion_type", value = "data") %.% {
    mutate(data = map(data, ~ enframe(.x, name = "axis", value = "data")))
    unnest(data)
    unnest(data)
    group_by(motion_type, axis)
    mutate(idx = row_number())
    ungroup()
    arrange(motion_type, axis)
    mutate(axis = fct_inorder(axis))
  }
}

read_telemetry_data_file <- function(f) {
  j <- rjson::fromJSON(file = f)
  telemetry_json_to_tibble(j)
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

prep_telemtry_data_file <- function(f) {
  meta_data <- parse_data_file_name(f)
  meta_data$data <- list(read_telemetry_data_file(f))
  return(meta_data)
}

get_data_file_names <- function(dir) {
  list.files(dir, pattern = "json$", full.names = TRUE)
}

#### ---- Plotting ---- ####

line_plot_telemetry <- function(df) {
  df %>%
    ggplot(aes(idx, data)) +
    facet_wrap(~ motion_type, ncol = 1, scales = "free_y") +
    geom_line(aes(color = axis))
}



all_data_files <- get_data_file_names(data_dir)
raw_pushup_data <- prep_telemtry_data_file(all_data_files[2]) %>%
  unnest(data)

line_plot_telemetry(raw_pushup_data)
  

pushup_data <- raw_pushup_data %>%
  filter(between(idx, 350, 625))
line_plot_telemetry(pushup_data)


#### ---- HMM ---- ####

library(depmixS4)
set.seed(0)

pushup_data_wide <- pushup_data %.% {
  mutate(motion = glue("{motion_type}_{axis}"))
  pivot_wider(
    c(exercise, reps, date, idx), 
    names_from = motion,
    values_from = data
  )
}


contruct_full_telemetry_hmm_model <- function(d, nstates) {
  depmix(
    list(
      acceleration_x ~ 1,
      acceleration_y ~ 1,
      acceleration_z ~ 1,
      attitude_pitch ~ 1,
      attitude_roll ~ 1,
      attitude_yaw ~ 1
    ),
    nstates = nstates,
    family = list(
      gaussian(), gaussian(), gaussian(), 
      gaussian(), gaussian(), gaussian()
    ),
    data = d
  )
}


contruct_accell_telemetry_hmm_model <- function(d, nstates) {
  depmix(
    list(
      acceleration_x ~ 1,
      acceleration_y ~ 1,
      acceleration_z ~ 1
    ),
    nstates = nstates,
    family = list(gaussian(), gaussian(), gaussian()),
    data = d
  )
}


contruct_attitude_telemetry_hmm_model <- function(d, nstates) {
  depmix(
    list(
      attitude_pitch ~ 1,
      attitude_roll ~ 1,
      attitude_yaw ~ 1
    ),
    nstates = nstates,
    family = list(gaussian(), gaussian(), gaussian()),
    data = d
  )
}


m2_full <- contruct_full_telemetry_hmm_model(pushup_data_wide, 2)
m3_full <- contruct_full_telemetry_hmm_model(pushup_data_wide, 3)

m2_accel <- contruct_accell_telemetry_hmm_model(pushup_data_wide, 2)
m3_accel <- contruct_accell_telemetry_hmm_model(pushup_data_wide, 3)

m2_att <- contruct_attitude_telemetry_hmm_model(pushup_data_wide, 2)
m3_att <- contruct_attitude_telemetry_hmm_model(pushup_data_wide, 3)


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
  labs(x = "data point",
       y = "probability of state",
       color = "state",
       title = "Hidden Markov Model")

data_p <- pushup_data %>%
  mutate(motion_type = str_to_title(motion_type)) %>%
  line_plot_telemetry() +
  labs(x = NULL,
       y = "value") +
  theme(
    strip.text = element_text(size = 11, face = "bold")
  )

patch <- data_p / hmm_states_p +
  plot_layout(heights = c(2, 1))

ggsave(
  file.path("graphs", "pushup-hmm.png"),
  patch,
  width = 8, height = 5, unit = "in",
  dpi = 500
)
