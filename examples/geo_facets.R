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

geo_shape(NLD_muni) +
	geo_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"), convert2density=TRUE, style="kmeans") +
	geo_shape(NLD_prov) +
	geo_borders() +
	geo_facets(free.scales=FALSE) +
	geo_theme(c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44", "Population 45 to 64", "Population 65 and older"), scale=4)
