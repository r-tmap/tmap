data(NLD_muni)
data(NLD_prov)


tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", drop.shapes=FALSE, free.coords=FALSE) +
	tm_layout(legend.show = FALSE)

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", drop.shapes=TRUE, free.coords=FALSE) +
	tm_layout(legend.show = FALSE)

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", drop.shapes=FALSE, free.coords=TRUE) +
	tm_layout(legend.show = FALSE)

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
	tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", free.coords=TRUE, drop.shapes=TRUE) +
	tm_layout(legend.show = FALSE)