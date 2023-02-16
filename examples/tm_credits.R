## tmap v3 example:

## expression("Population (per " * km^2 * ")") not working yet
## no backwards compatibility for position=c("left", "bottom") yet

current.mode <- tmap_mode("plot")

data(NLD_muni, NLD_prov)

tm_shape(NLD_muni) +
	tm_fill(col="population", convert2density=TRUE, 
			style="kmeans", title = "Population (per km2") +
	tm_borders("grey25", alpha=.5) + 
	tm_shape(NLD_prov) +
	tm_borders("grey40", lwd=2) +
	tm_format("NLD", bg.color="white", frame = TRUE) +
	tm_credits("(c) Statistics Netherlands (CBS) and\nKadaster Nederland", position=tm_pos_in("left", "bottom"))

# restore current mode
tmap_mode(current.mode)
