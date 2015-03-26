data(World)
data(Europe)
data(NLD_muni)
data(NLD_prov)


# Facets defined by constant values
tm_shape(World) +
    tm_fill(c("forestgreen", "goldenrod")) +
tm_layout_World(c("A green world", "A dry world"), bg.color="lightskyblue2", 
	title.position=c("left", "bottom"))

# Facets defined by multiple variables
tm_shape(Europe) +
	tm_borders() +
    tm_fill(c("gdp_cap_est", "pop_est_dens"), style="kmeans") +
tm_layout_Europe(c("GDP per capita", "Population density"))

tm_shape(NLD_muni) +
    tm_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"),
            style="kmeans") +
tm_shape(NLD_prov) +
    tm_borders() +
	tm_layout_NLD(c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44",
		"Population 45 to 64", "Population 65 and older"), draw.frame = TRUE, asp=0)


# Facets defined by groupings
tm_shape(NLD_prov) +
	tm_borders() +
	tm_fill("gold2") +
	tm_facets(by="name") +
	tm_layout()

tm_shape(NLD_prov) +
	tm_fill("gold2") + tm_borders() +
	tm_facets(by="name", free.coords = TRUE, split=TRUE) +
tm_layout()

tm_shape(NLD_muni) +
	tm_borders() +
	tm_facets(by="province") +
	tm_fill("population", style="kmeans", convert2density = TRUE) +
tm_shape(NLD_prov) +
	tm_borders(lwd=4) +
	tm_facets(by="name", free.coords=TRUE, split=TRUE) +
	tm_layout(legend.show = FALSE)
	
