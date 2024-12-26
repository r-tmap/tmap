test_that("It is possible to combine two visual elements into single legend", {
	break_values <- c(-Inf, 2, 4, 6, 8, Inf)
	color_values <- c("#B4D79E", "#98E600", "#FFAA00", "#FF5500", "#A80000")
	size_values <- c(0.5, 1, 2, 3, 5)
  expect_no_error({
  	tm_shape(World_rivers) +
  		tm_lines(
  			col = "strokelwd",
  			col.scale = tm_scale_intervals(values = color_values, breaks = break_values),
  			lwd = "strokelwd",
  			lwd.scale = tm_scale_intervals(values = size_values, breaks = break_values),
  			lwd.legend = tm_legend_combine("col"),
  			plot.order = tm_plot_order("lwd", reverse = FALSE, na.order = "bottom")
  		) +
  		tm_layout(legend.show = TRUE)
  })
})
