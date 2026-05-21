\donttest{
	if (interactive()) {
		m1 <- tm_shape(NLD_prov) +
			tm_polygons("yellow") +
			tm_animate(frames = "name")

		tmap_animation(m1, filename = "countries.gif")

		m2 <- tm_shape(metro) +
			tm_symbols(size = paste0("pop", seq(1950, 2030, by=10)),
					   size.free = FALSE,
					   size.legend = tm_legend("Population")) +
			tm_layout(panel.labels = seq(1970, 2030, by=10)) +
			tm_animate()

		tmap_animation(m2, filename = "cities.gif")
	}
}
