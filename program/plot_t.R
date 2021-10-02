# function to create timeline basic plot
plot_t_basic <- function(df){
  df %>% 
    ggplot(aes(x = date_time, y = weight, group = exercise, color = exercise)) +
    geom_line() +
    geom_point(
      # To-Do: point shape conditional to whether completed workout reps or not
      # aes(shape = factor(ifelse(reps == 5, 1, 2), levels = 1:2, labels = c("Complete", "Incomplete")))
    ) +
    theme_cowplot() +
    scale_color_manual(values = color_exercise) +
    ylab("Weight (lbs)") +
    xlab("Date") +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
}

# function to plot timeline by facets of exercise
plot_t_facet <- function(p_t_basic) {
  p_t_basic +
    facet_wrap(facets = "exercise", ncol = 1, scales = "free_y")
}
