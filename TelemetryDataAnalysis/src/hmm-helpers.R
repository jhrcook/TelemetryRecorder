
#### ---- Plotting ---- ####


plot_telmetry_data <- function(df, x = value) {
  df %>%
    mutate(motion = str_to_title(motion)) %>%
    ggplot(aes(idx, {{ x }})) +
    facet_wrap(~motion, ncol = 1, scales = "free_y") +
    geom_line(aes(color = axis), alpha = 0.7) +
    scale_color_brewer(type = "qual", palette = "Dark2") +
    theme(
      strip.text = element_text(hjust = 0.5, size = 11)
    )
}

plot_hmm_results <- function(hmm_fit) {
  posterior(hmm_fit) %>%
    as_tibble() %>%
    mutate(idx = row_number()) %>%
    pivot_longer(-c(idx, state)) %>%
    ggplot(aes(x = idx, y = value, color = name)) +
    facet_grid(name ~ .) +
    geom_line(size = 1, alpha = 0.8) +
    scale_color_brewer(type = "qual", palette = "Set1") +
    scale_y_continuous(breaks = c(0, 0.5, 1)) +
    theme(
      plot.title = element_text(hjust = 0.5, size = 11),
    ) +
    labs(
      x = "data point",
      y = "probability of state",
      color = "state",
      title = "Hidden Markov Model states"
    )
}


plot_hmm_fit <- function(data, hmm_fit, data_x) {
  data_plot <- plot_telmetry_data(data, x = {{ data_x }}) +
    theme(axis.title.x = element_blank())
  hmm_plot <- plot_hmm_results(hmm_fit)
  patch <- data_plot / hmm_plot + plot_layout(heights = c(3, 1))
  plot(patch)
  return(NULL)
}
