## Europe examples
data(Europe)
data(cities)

tm_shape(Europe) +
	tm_fill("gdp_cap_est", style="kmeans", textNA = "Non-European countries") +
	tm_borders() +
	tm_text("iso_a3", cex="AREA", root=4, scale=2, bg.alpha=0) +
	tm_layout_Europe("GDP per capita")

tm_shape(Europe) +
	tm_borders() +
	tm_fill() +
tm_shape(cities) +
	tm_text("name", cex="pop_max", scale=2, root=3, ymod=-.015, bg.alpha=0) +
	tm_bubbles(size="pop_max", col="capital", size.lim=c(0, 2e7)) +
	tm_layout_Europe("Metropolitan population", legend.titles=c(bubble.col="Capital"))

## Netherlands example
data(NLD_muni)
data(NLD_prov)

tm_shape(NLD_muni) +
	tm_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"),
			convert2density=TRUE, style="kmeans") +
tm_shape(NLD_prov) +
	tm_borders() +
	tm_facets(free.scales=FALSE) +
	tm_layout_NLD(c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44",
		"Population 45 to 64", "Population 65 and older"), scale=4, draw.frame = TRUE)
