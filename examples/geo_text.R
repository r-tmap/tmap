# Europe example
data(Europe)
geo_shape(Europe) +
	geo_fill("gdp_cap_est", style="kmeans", textNA = "Non-European countries") +
	geo_borders() +
	geo_text("iso_a3", cex="AREA", scale=2, bg.alpha=0) +
	geo_theme_Europe("GDP per capita")


data(rivers)
data(cities)

geo_shape(Europe) +
	geo_fill("pop_est_dens", style="kmeans", textNA="Non-European countries") +
	geo_borders() +
	geo_shape(rivers) +
	geo_lines("dodgerblue3") +
	geo_shape(cities) +
	geo_text("name", cex="pop_max", scale=1, ymod=-.02, root=4, cex.lowerbound = .60, bg.color="yellow", bg.alpha = 150) + 
	geo_bubbles("pop_max", "red", border.col = "black", border.lwd=1, size.lim = c(0, 2e7)) +
	geo_shape(Europe) +
	geo_text("name", cex="area", scale=1.5, root=8, cex.lowerbound = .40, fontface="bold", case=NA, fontcolor = "gray35") + 
	geo_theme_Europe("Map of Europe", legend.titles = c(fill="Country population density (people per km2)", bubble.size="City Population"))
