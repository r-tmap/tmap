## Europe example
data(Europe)
tm_shape(Europe) + tm_borders()

## Netherlands example
data(NLD_prov)
data(NLD_muni)

tm_shape(NLD_muni) +
	tm_fill(col="population", convert2density=TRUE, style="kmeans") +
	tm_borders("grey25", alpha=.5) + 
tm_shape(NLD_prov) +
	tm_borders("grey25", lwd=2, alpha=.5) +
	tm_layout_NLD(title="Population (per km2)", bg.color="white", draw.frame = FALSE)
