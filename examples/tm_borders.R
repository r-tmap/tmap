## Europe example
data(Europe)
tm_shape(Europe) + tm_borders()

## Netherlands example
data(NLD_prov)
data(NLD_muni)

tm_shape(NLD_muni) + 
	tm_borders("grey25", alpha=.5) + 
	tm_fill("population", style="kmeans") +
tm_shape(NLD_prov) + 
	tm_borders("grey25", lwd=2, alpha=.5) +
tm_layout(bg.color="white", draw.frame = FALSE)
	