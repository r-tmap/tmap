data(Europe)

geo(Europe, choro.fill="gdp_cap_est", borders="gray50", theme="Europe", style="kmeans", title="Estimated GDP per capita")

geo_shape(Europe) +
	geo_choropleth("gdp_cap_est", style="kmeans") +
	geo_borders("gray50") +
	geo_theme_Europe("Estimated GDP per capita")

geo_shape(Europe) +
	geo_choropleth("gdp_cap_est", style="kmeans") +
	geo_borders("gray50") + #geo_theme()
	geo_theme_Europe("Estimated GDP per capita", draw.frame=TRUE, crop=TRUE,
					 margins = rep(0, 4), frame.margins=c(0, 0, 0, 0))


geo_shape(Europe, projection="longlat") +
	geo_choropleth("gdp_cap_est", style="kmeans") +
	geo_borders("gray50") + #geo_theme()
	geo_theme_Europe("Estimated GDP per capita", draw.frame=TRUE, crop=TRUE,
					 margins = rep(0, 4), frame.margins=c(0, 0, 0, 0))


geo_theme()