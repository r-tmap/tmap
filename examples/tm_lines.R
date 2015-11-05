data(World, Europe, rivers)

qtm(rivers, line.col = "navy")

tm_shape(Europe) +
  tm_fill("MAP_COLORS", palette = "Pastel2") +
tm_shape(rivers) +
  tm_lines(col="black", lwd="scalerank", scale=2, legend.lwd.show = FALSE) +
tm_layout("Rivers of Europe") +
	tm_style_cobalt()
