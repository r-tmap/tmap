data(NLD_muni)

NLD_muni_pop_per_km2 <- calc_densities(NLD_muni, var = c("pop_men", "pop_women"), suffix = "_km2")
NLD_muni <- append_data(NLD_muni_pop_per_km2, NLD_muni, fixed=TRUE)

qtm(NLD_muni, fill=c("pop_men_km2", "pop_women_km2"), nrow=1, scale=2)

tm_shape(NLD_muni) +
	tm_fill(c("pop_men_km2", "pop_women_km2")) +
	tm_borders() +
	tm_facets(nrow=1, free.scales = FALSE) +
	tm_layout(scale=2)
