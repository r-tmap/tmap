data(NLD_muni)

NLD_muni_pop_per_km2 <- calc_densities(NLD_muni, 
    var = c("pop_men", "pop_women"), suffix = "_km2")
NLD_muni <- append_data(NLD_muni, NLD_muni_pop_per_km2, fixed=TRUE)

tm_shape(NLD_muni) +
	tm_polygons(c("pop_men_km2", "pop_women_km2"), 
        title=expression(paste("Population per ", km^2)), style="quantile") +
tm_facets(free.scales = FALSE) +
tm_layout(panel.show = TRUE, panel.labels=c("Men", "Women"))
	
