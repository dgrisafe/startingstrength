library(tidyverse)
library(gsheet) # https://cran.r-project.org/web/packages/gsheet/gsheet.pdf
library(cowplot)
source("program/fun_save_png.R")
source("program/plot_t.R")

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

# programming reps
reps <- list(
  "general" = c(5, 5, 3, 2, 5),
  "power" = c(5, 5, 3, 2, 3)
)

# programming sets
sets <- list(
  "general" = c(2, 1, 1, 1, 3),
  "deadlift" = c(2, 1, 1, 1, 1),
  "power" = c(2, 1, 1, 1, 5)
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
    date = lubridate::date(date_time),
    labels = str_replace_all(str_extract(notes, pattern = "^(.*?)[.!?:,]") , "[[:punct:]]", "")
  ) %>% 
  # remove first set with empty barbell (45 lb)
  dplyr::filter(weight != 45)


# Plot w/Warm Ups

  ## create basic plot
  p_str_time <- df_format %>% plot_t_basic()
  save_png("./visualization/strength_timeline.png", p_str_time)
  
  ## create facet plot
  p_str_time_facet <- p_str_time %>% plot_t_facet() + 
    theme(legend.position = "none") + 
    ggrepel::geom_label_repel(aes(label = labels), size = 2)
  save_png("./visualization/strength_timeline_facet.png", p_str_time_facet, dim_h = 10)
  

# Plot Max
  
  ## create basic plot max
  p_str_time_max <- df_format %>% dplyr::group_by(date, exercise) %>% top_n(n = 1, wt = weight) %>% plot_t_basic() 
  save_png("./visualization/strength_timeline_max.png", p_str_time_max)
  
  ## create facet plot max
  p_str_time_max_facet <- p_str_time_max %>% 
    plot_t_facet() + 
    theme(legend.position = "none") + 
    ggrepel::geom_label_repel(aes(label = labels), size = 2)
  save_png("./visualization/strength_timeline_max_facet.png", p_str_time_max_facet, dim_h = 10)
