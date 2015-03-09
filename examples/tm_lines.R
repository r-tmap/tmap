data(rivers)

data(Europe)
tm_shape(Europe) +
    tm_fill("darkolivegreen3") +
    tm_borders("white") +
tm_shape(rivers) +
    tm_lines(col="navy", lwd="scalerank", scale=2) +
tm_layout("Rivers of Europe", legend.show=FALSE)

data(World)
tm_shape(World) +
    tm_fill("darkolivegreen3") +
tm_shape(rivers) +
    tm_lines(col="navy") +
tm_layout_World("Rivers of the World")
