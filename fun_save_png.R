# save as png files in consistent format
save_png <- function(file_name, plot, dim_res = 300, dim_w = 7, dim_h = 5, dim_units = "in"){
  png(file_name, res = dim_res, width = dim_w, height = dim_h, units = dim_units)
  print(plot)
  dev.off()
}
