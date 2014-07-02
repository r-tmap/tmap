# \\dontrun{
data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)

## append age group data to municipalities and plot it
NLD_muni <- append_data(NLD_muni, NLD_ageGroups, key.data="muni", key.shp="name")
geo_shape(NLD_muni) +
	geo_fill(c("pop_0_20", "pop_20_65", "pop_65p"), convert2density=TRUE) +
	geo_shape(NLD_prov) +
	geo_borders() +
	geo_facets(free.scales=TRUE) +
	geo_theme_NLD(c("Population 0 to 20", "Population 20 to 65", "Population 65 and older"),
				  scale=3,
				  inner.margins=c(.05, .2, .7, .05), legend.width=.8, 
				  legend.height=.5, legend.position=c("left", "top"))


## aggregate age group data to provinces
NLD_prov <- convert_shape_data(NLD_muni, NLD_prov, c("pop_0_20", "pop_20_65", "pop_65p"))
geo_shape(NLD_prov) +
	geo_fill(c("pop_0_20", "pop_20_65", "pop_65p"), convert2density=TRUE) +
	geo_borders() +
	geo_facets(free.scales=TRUE) +
	geo_theme_NLD(c("Population 0 to 20", "Population 20 to 65", "Population 65 and older"),
				  scale=3,
				  inner.margins=c(.05, .2, .7, .05), legend.width=.8, 
				  legend.height=.5, legend.position=c("left", "top"))

## convert age group data to municipalities again
NLD_muni <- convert_shape_data(NLD_prov, NLD_muni, c("pop_0_20", "pop_20_65", "pop_65p"), 
						  variables.to=c("pop_0_20_new", "pop_20_65_new", "pop_65p_new"))
geo_shape(NLD_muni) +
	geo_fill(c("pop_0_20_new", "pop_20_65_new", "pop_65p_new"), convert2density=TRUE) +
	geo_borders() +
	geo_facets(free.scales=TRUE) +
	geo_theme_NLD(c("Population 0 to 20", "Population 20 to 65", "Population 65 and older"),
				  scale=3,
				  inner.margins=c(.05, .2, .7, .05), legend.width=.8, 
				  legend.height=.5, legend.position=c("left", "top"))
# }
