tm_shape(World) +
  tm_polygons(
  	fill = "HPI",
    fill.scale = tm_scale_continuous(values = "scico.roma", midpoint = 30))

tm_shape(metro) +
  tm_bubbles(
  	size = "pop1950", 
    size.scale = tm_scale_continuous(
      values.scale = 1),
    size.legend = tm_legend("Population in 1950", frame = FALSE))

# Note that for this type of legend, we recommend tm_scale_intervals()
tm_shape(metro) +
  tm_bubbles(
    size = "pop1950", 
    size.scale = tm_scale_continuous(
      values.scale = 2, 
      limits = c(0, 12e6), 
      ticks = c(1e5, 3e5, 8e5, 4e6, 1e7), 
      labels = c("0 - 200,000", "200,000 - 500,000", "500,000 - 1,000,000", "1,000,000 - 10,000,000", "10,000,000 or more"), 
      outliers.trunc = c(TRUE, TRUE)),
    size.legend = tm_legend("Population in 1950", frame = FALSE))
