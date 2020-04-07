# as.count is TRUE for integers if style = pretty, fixed, or log10_pretty

# N (natural numbers, with 0)
World$x <- sample(0:20, size = 177, replace = TRUE)
tm_shape(World) + tm_polygons("x")

# N (natural numbers, positive)
World$x <- sample(1:20, size = 177, replace = TRUE)
tm_shape(World) + tm_polygons("x")

# Z (integers)
World$x <- sample(-10:10, size = 177, replace = TRUE)
tm_shape(World) + tm_polygons("x")

# show as continuous (old way)
World$x <- sample(1:20, size = 177, replace = TRUE)
tm_shape(World) + tm_polygons("x", as.count = FALSE)

# style: fixed
tm_shape(World) + tm_polygons("x", breaks = c(1, 5, 10, 20))

# scientific notation
tm_shape(World) + tm_polygons("x", breaks = c(0, 1, 3, 5, 10, 20), legend.format = list(scientific = TRUE))

# style: log10pretty (continuous)
tm_shape(World) + tm_polygons("pop_est", style = "log10_pretty")

# style: log10pretty (count)
tm_shape(World) + tm_polygons("pop_est", as.count = TRUE, style = "log10_pretty")
