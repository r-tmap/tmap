data(Europe)
data(NLD_muni)
data(NLD_prov)
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


