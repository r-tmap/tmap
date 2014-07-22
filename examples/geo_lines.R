data(rivers)

data(Europe)
geo_shape(Europe) +
	geo_fill("darkolivegreen3") +
	geo_borders("white") +
geo_shape(rivers) +
	geo_lines(col="navy", lwd="scalerank", scale=2) +
geo_theme("Rivers in Europe", legend.show=FALSE)

data(World)
geo_shape(World) +
	geo_fill("darkolivegreen3") +
geo_shape(rivers) +
	geo_lines(col="navy") +
	geo_theme_World("Rivers in the World")

