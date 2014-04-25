# Europe example
data(Europe)
geo_shape(Europe) +
	geo_choropleth("gdp_cap_est", style="kmeans") +
	geo_borders() +
	geo_text("iso_a3", cex="AREA3", scale=2, bg.alpha=0) +
	geo_theme_Europe("GDP per capita")
