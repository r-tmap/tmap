\dontrun{
data(land)

# original map
qtm(land, raster="cover_cls")

# map decreased by factor 4 for each dimension
land4 <- aggregate_map(land, fact=4)
qtm(land4, raster="cover_cls")

# map decreased by factor 8, where the variable trees is aggregated with mean, min, and max
land_trees <- aggregate_map(land, fact=8, 
							agg.fun=list(trees="mean", trees="min", trees="max"))
tm_shape(land_trees) +
	tm_raster(c("trees.1", "trees.2", "trees.3"), title="Trees (%)") +
	tm_facets(free.scales=FALSE) +
	tm_layout(panel.labels = c("mean", "min", "max"))

data(NLD_muni, NLD_prov)

# aggregate Dutch municipalities to provinces
NLD_prov2 <- aggregate_map(NLD_muni, by="province", 
	agg.fun = list(population="sum", origin_native="mean", origin_west="mean", 
				   origin_non_west="mean", name="modal"), weights = "population")

# see original provinces data
NLD_prov@data[, c("name", "population", "origin_native", "origin_west", "origin_non_west")]

# see aggregates data (the last column corresponds to the most populated municipalities)
NLD_prov2@data

# largest municipalities in area per province
aggregate_map(NLD_muni, by="province", agg.fun = list(name="modal"), weights = "AREA")@data
}
