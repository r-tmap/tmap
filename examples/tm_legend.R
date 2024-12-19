# Example using different settings from tm_legend()

World |>
	tm_shape() +
	tm_polygons(
		fill = "HPI",
		fill.legend = tm_legend(
			title = "Home Price Index",
			design = "standard",
			title.color = "orange",
			bg.color = "purple",
			show = TRUE
		),
		id = "name",
		# Format the labels using dollar sign
		fill.scale = tm_scale_intervals(label.format = function(x) format(x, big.marj = " ")),
	)
