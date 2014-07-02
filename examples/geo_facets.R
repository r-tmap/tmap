## World example
data(World)
geo_shape(World) +
	geo_fill(c("green", "blue")) +
	geo_theme(c("A green world", "A blue world"))

## Europe example
data(Europe)
geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "pop_est_dens")) +
	geo_theme_Europe(scale=2)



## Netherlands example
data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)

NLD_muni <- append_data(NLD_muni, NLD_ageGroups, key.data="muni", key.shp="name")

geo_shape(NLD_muni) +
	geo_fill(c("pop_0_20", "pop_20_65", "pop_65p"), convert2density=TRUE) + geo_shape(NLD_prov) +
	geo_borders() +
	geo_facets(free.scales=TRUE) +
	geo_theme_NLD(c("Population 0 to 20", "Population 20 to 65", "Population 65 and older"),
				  scale=3,
				  inner.margins=c(.05, .2, .7, .05), legend.width=.8, 
				  legend.height=.5, legend.position=c("left", "top"))
