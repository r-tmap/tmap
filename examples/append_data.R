data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)

NLD_muni <- append_data(NLD_muni, NLD_ageGroups, key.data="muni", key.shp="name")

geo_shape(NLD_muni) +
	geo_fill(c("pop_0_20", "pop_20_65", "pop_65p"), convert2density=TRUE, total.area.km2=41543) +
	geo_shape(NLD_prov) +
	geo_borders() +
	geo_facets(free.scales=TRUE) +
	geo_theme_NLD(c("Population 0 to 20", "Population 20 to 65", "Population 65 and older"),inner.margins=c(.05, .2, .7, .05), legend.width=.8, legend.max.height=.5, legend.position=c("left", "top"))
