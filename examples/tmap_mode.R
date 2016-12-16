# world choropleth/bubble map of the world
data(World, metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

map1 <- tm_shape(World) +
	tm_polygons("income_grp", palette="-Blues", contrast=.7, id="name", title="Income group") +
	tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", 
		border.col = "black", border.alpha = .5, 
		style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
		palette="-RdYlBu", contrast=1, 
		title.size="Metro population", 
		title.col="Growth rate (%)", id="name",
		popup.vars = c("pop2010", "pop2020", "growth")) + 
	tm_layout(legend.bg.color = "grey90", legend.bg.alpha=.5, legend.frame=TRUE)

# initial mode: "plot"
current.mode <- tmap_mode("plot")

# plot map
map1

# switch to other mode: "view"
ttm()

# view map
map1

# choropleth of the Dutch population in interactive mode:
require(tmaptools)
data(NLD_muni, NLD_prov)
NLD_muni$pop_dens <- calc_densities(NLD_muni, var = "population")

tm_shape(NLD_muni) +
	tm_fill(col="pop_dens", 
		style="kmeans", 
		title = "Population (per km^2)", id = "name") +
	tm_borders("grey25", alpha=.5) + 
	tm_shape(NLD_prov) +
	tm_borders("grey40", lwd=2)

# restore current mode
tmap_mode(current.mode)
