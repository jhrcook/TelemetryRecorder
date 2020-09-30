
library(jhcutils)
library(mustashe)
library(rjson)
library(nakedpipe)
library(magrittr)
library(tidyverse)

theme_set(theme_minimal())

data_dir <- "~/Library/Mobile Documents/iCloud~TelemetryRecorder/Documents"
f <- list.files(data_dir, pattern = "^workout-data", full.names = TRUE)[5]
message(basename(f))

j <- fromJSON(file = f)
  
d <- enframe(j, name = "motion_type", value = "data") %.% {
  mutate(data = map(data, ~ enframe(.x, name = "axis", value = "data")))
  unnest(data)
  ~~ print(.)
  unnest(data)
  group_by(motion_type, axis)
  mutate(
    i = row_number()
  )
  ungroup()
  mutate(
    axis = fct_inorder(axis)
  )
} 


d %>%
  ggplot(aes(i, data)) +
  facet_wrap(~ motion_type, ncol = 1, scales = "free_y") +
  geom_line(aes(color = axis))


d %.% {
  filter(motion_type == "attitude")
  group_by(i)
  summarise(total_motion = abs(prod(data)))
  ungroup()
  mutate(smooth_motion = ksmooth(unlist(i), unlist(total_motion), bandwidth = 10)$y)
} %>%
  ggplot(aes(i)) +
  geom_line(aes(y = total_motion), color = "grey25", size = 3, alpha = 0.5) +
  geom_line(aes(y = smooth_motion), color = "purple", size = 1)
