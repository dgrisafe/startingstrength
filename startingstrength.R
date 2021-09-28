library(tidyverse)
library(gsheet) # https://cran.r-project.org/web/packages/gsheet/gsheet.pdf
library(cowplot)
source("fun_save_png.R")

# load url
url <- "https://docs.google.com/spreadsheets/d/1F2IPfClwYT3qm4VbRAwbtanyX0AFURBW-uvPVcoMV-8/edit?usp=sharing"

# supply encoding
construct_download_url(url, format = "csv")

# load data from google sheet
df_data <- read.csv(text=gsheet2text(url,format='csv'), stringsAsFactors = FALSE)
colnames(df_data) <- c("time", "exercise", "weight", "reps", "sets")

# format data types
df_format <- df_data %>% 
  dplyr::mutate(
    exercise = factor(exercise, levels = unique(exercise)),
    time = lubridate::mdy_hms(time)
  )

# create basic plot
p_str_time <- df_format %>% 
  ggplot(aes(x = time, y = weight, group = exercise, color = exercise)) +
  geom_line() +
  theme_cowplot() +
  ylab("Weight (lbs)") +
  xlab("Date") +
  theme(
    legend.position = "top",
    legend.title = element_blank()
  )
p_str_time
save_png("./strength_timeline.png", p_str_time)

# plot by facets
p_str_time_facet <- p_str_time + facet_wrap(facets = "exercise", ncol = 1, scales="free_y")
save_png("./strength_timeline_facet.png", p_str_time_facet, dim_h = 10)
