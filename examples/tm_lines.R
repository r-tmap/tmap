data(World, Europe, rivers)

qtm(rivers, line.col = "navy")

tm_shape(Europe) +
  tm_fill("MAP_COLORS", palette = "Pastel2") +
tm_shape(rivers) +
  tm_lines(col="navy", lwd="scalerank", scale=2, legend.lwd.show = FALSE) +
tm_layout("Rivers of Europe")
