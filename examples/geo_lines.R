data(rivers)

data(Europe)
geo_shape(Europe) +
	geo_fill("darkolivegreen3") +
	geo_borders("white") +
	geo_shape(rivers) +
	geo_lines(col="navy", lwd="scalerank", scale=2) +
geo_theme("Rivers in Europe", legend.profile="hide")

data(World)
geo_shape(World) +
	geo_fill("darkolivegreen3") +
	geo_shape(rivers) +
	geo_lines(col="navy") +
	geo_theme("Rivers in the World")





## all in one example

geo_shape(Europe) +
	geo_fill(c("pop_est_dens", "gdp_cap_est")) +
	geo_borders("white") +
geo_shape(rivers) +
	geo_lines(col="type", lwd="scalerank", scale=2, palette="Set1") +
geo_shape(Europe) +
	geo_bubbles("pop_est", col="income_grp") +
geo_theme_Europe()