data(rivers)
data(World)
data(Europe)

qtm(rivers, line.col = "navy")

tm_shape(Europe) +
  tm_fill("darkolivegreen3") +
  tm_borders("white") +
tm_shape(rivers) +
  tm_lines(col="navy", lwd="scalerank", scale=2) +
tm_layout("Rivers of Europe", legend.show=FALSE)
