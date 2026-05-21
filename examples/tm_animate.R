\donttest{
if (interactive()) {
	tm_shape(NLD_prov) +
		tm_polygons("yellow") +
		tm_animate(frames = "name")

	tm_shape(metro) +
		tm_symbols(size = paste0("pop", seq(1950, 2030, by=10)),
				   size.free = FALSE,
				   size.legend = tm_legend("Population")) +
		tm_layout(panel.labels = seq(1970, 2030, by=10)) +
		tm_animate()
}
}
