## code to prepare height adjustment dataset is included here

height_data <- tibble::tribble(
  ~N, ~Actual, ~str, ~size,
  1, 0.27, stringHeight(paste(rep("M", 1), collapse = "\n"), font_size = 5), 5,
  2, 0.49, stringHeight(paste(rep("M", 2), collapse = "\n"), font_size = 5), 5,
  3, 0.74, stringHeight(paste(rep("M", 3), collapse = "\n"), font_size = 5), 5,
  4, 1.00, stringHeight(paste(rep("M", 4), collapse = "\n"), font_size = 5), 5,
  1, 0.42, stringHeight(paste(rep("M", 1), collapse = "\n"), font_size = 8), 8,
  2, 0.81, stringHeight(paste(rep("M", 2), collapse = "\n"), font_size = 8), 8,
  3, 1.20, stringHeight(paste(rep("M", 3), collapse = "\n"), font_size = 8), 8,
  4, 1.57, stringHeight(paste(rep("M", 4), collapse = "\n"), font_size = 8), 8,
  1, 0.56, stringHeight(paste(rep("M", 1), collapse = "\n"), font_size = 11), 11,
  2, 1.09, stringHeight(paste(rep("M", 2), collapse = "\n"), font_size = 11), 11,
  3, 1.62, stringHeight(paste(rep("M", 3), collapse = "\n"), font_size = 11), 11,
  4, 2.15, stringHeight(paste(rep("M", 4), collapse = "\n"), font_size = 11), 11,
  1, 0.77, stringHeight(paste(rep("M", 1), collapse = "\n"), font_size = 16), 16,
  2, 1.54, stringHeight(paste(rep("M", 2), collapse = "\n"), font_size = 16), 16,
  3, 2.31, stringHeight(paste(rep("M", 3), collapse = "\n"), font_size = 16), 16,
  4, 3.09, stringHeight(paste(rep("M", 4), collapse = "\n"), font_size = 16), 16,
  1, 1.00, stringHeight(paste(rep("M", 1), collapse = "\n"), font_size = 20), 20,
  2, 1.94, stringHeight(paste(rep("M", 2), collapse = "\n"), font_size = 20), 20,
  3, 2.94, stringHeight(paste(rep("M", 3), collapse = "\n"), font_size = 20), 20,
  4, 3.87, stringHeight(paste(rep("M", 4), collapse = "\n"), font_size = 20), 20,
  1, 1.10, stringHeight(paste(rep("M", 1), collapse = "\n"), font_size = 22), 22,
  2, 2.15, stringHeight(paste(rep("M", 2), collapse = "\n"), font_size = 22), 22,
  3, 3.21, stringHeight(paste(rep("M", 3), collapse = "\n"), font_size = 22), 22,
  4, 4.27, stringHeight(paste(rep("M", 4), collapse = "\n"), font_size = 22), 22,
  1, 1.35, stringHeight(paste(rep("M", 1), collapse = "\n"), font_size = 28), 28,
  2, 2.72, stringHeight(paste(rep("M", 2), collapse = "\n"), font_size = 28), 28,
  3, 4.07, stringHeight(paste(rep("M", 3), collapse = "\n"), font_size = 28), 28,
  4, 5.43, stringHeight(paste(rep("M", 4), collapse = "\n"), font_size = 28), 28,
  1, 2.34, stringHeight(paste(rep("M", 1), collapse = "\n"), font_size = 48), 48,
  2, 4.67, stringHeight(paste(rep("M", 2), collapse = "\n"), font_size = 48), 48,
  3, 6.99, stringHeight(paste(rep("M", 3), collapse = "\n"), font_size = 48), 48,
  4, 9.27, stringHeight(paste(rep("M", 4), collapse = "\n"), font_size = 48), 48
)


height_adj <- stats::lm(Actual ~ str + size, data = height_data)



# ggplot2::ggplot(data = height_data, ggplot2::aes(str, Actual, color = as.factor(size))) +
#   ggplot2::geom_point() +
#   ggplot2::geom_smooth(method='lm')


saveRDS(height_adj, "inst/apis/height_adj.rds")
