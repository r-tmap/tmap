data(rivers)
data(World)
data(Europe)

tm_shape(World) +
	tm_fill("darkolivegreen3") +
	tm_shape(rivers) +
	tm_lines(col="navy") +
	tm_layout_World("Rivers of the World", inner.margins = c(0,0,.1,0))

tm_shape(Europe) +
    tm_fill("darkolivegreen3") +
    tm_borders("white") +
tm_shape(rivers) +
    tm_lines(col="navy", lwd="scalerank", scale=2) +
tm_layout("Rivers of Europe", legend.show=FALSE)
