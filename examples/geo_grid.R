data(World)

geo_shape(World) +
	geo_fill(c("economy"), style="kmeans") +
	geo_facets(by="income_grp") +
	geo_theme(inner.margins=c(0, .4, .02, .02),
			  legend.profile="text",
			  legend.height=c(choro=.6),
			  legend.max.height=.8)


data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)

NLD_muni <- append_data(NLD_muni, NLD_ageGroups, key.data="muni", key.shp="name")

geo_shape(NLD_muni) +
	geo_fill(c("pop_0_20", "pop_20_65", "pop_65p"), convert2density=TRUE, total.area.km2=41543) +
geo_shape(NLD_prov) +
	geo_borders() +
	geo_grid(free.scales=TRUE) +
	geo_theme_NLD(c("Population 0 to 20", "Population 20 to 65", "Population 65 and older"),inner.margins=c(.05, .2, .7, .05), legend.width=.8, legend.max.height=.5, legend.position=c("left", "top"))
