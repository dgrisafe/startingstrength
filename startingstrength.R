library(tidyverse)
library(gsheet) # https://cran.r-project.org/web/packages/gsheet/gsheet.pdf
library(cowplot)
source("fun_save_png.R")


# exercises lifts
exercise_names <- c("Squat", "Bench Press", "Press", "Deadlift", "Power Clean", "Chin Ups", "Back Extension")

# color palate for lift exercises
color_exercise <- c(
  "Squat" = "#cc0000",
  "Bench Press" = "#674ea7",
  "Press" = "#6aa84f",
  "Deadlift" = "#3c78d8",
  "Power Clean" = "#000000",
  "Chin Ups" = "#000000",
  "Back Extension" = "#000000"
)


# load url
url <- "https://docs.google.com/spreadsheets/d/1F2IPfClwYT3qm4VbRAwbtanyX0AFURBW-uvPVcoMV-8/edit?usp=sharing"

# supply encoding
construct_download_url(url, format = "csv")

# load data from google sheet
df_data <- read.csv(text=gsheet2text(url,format='csv'), stringsAsFactors = FALSE)
colnames(df_data) <- c("date_time", "exercise", "weight", "reps", "sets", "notes")

# format data types
df_format <- df_data %>% 
  dplyr::mutate(
    exercise = factor(exercise, levels = exercise_names),
    date_time = lubridate::mdy_hms(date_time),
    date = lubridate::date(date_time)
  )


# function to create basic plot
plot_t_basic <- function(df){
  df %>% 
    ggplot(aes(x = date_time, y = weight, group = exercise, color = exercise)) +
    geom_line() +
    theme_cowplot() +
    scale_color_manual(values = color_exercise) +
    ylab("Weight (lbs)") +
    xlab("Date") +
    theme(
      legend.position = "top",
      legend.title = element_blank()
    )
}

# function to plot by facets
plot_t_facet <- function(p_t_basic) {
  p_t_basic +
    facet_wrap(facets = "exercise", ncol = 1, scales = "free_y")
}


# Plot w/Warm Ups

  ## create basic plot
  p_str_time <- df_format %>% plot_t_basic()
  save_png("./strength_timeline.png", p_str_time)
  
  ## create facet plot
  p_str_time_facet <- p_str_time %>% plot_t_facet()
  save_png("./strength_timeline_facet.png", p_str_time_facet, dim_h = 10)
  

# Plot Max
  
  ## create basic plot
  p_str_time_max <- df_format %>% dplyr::group_by(date, exercise) %>% top_n(n = 1, wt = weight) %>% plot_t_basic()
  save_png("./strength_timeline_max.png", p_str_time_max)
  
  ## create facet plot
  p_str_time_max_facet <- p_str_time_max %>% plot_t_facet()
  save_png("./strength_timeline_max_facet.png", p_str_time_max_facet, dim_h = 10)
