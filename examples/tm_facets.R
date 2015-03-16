data(World)
data(Europe)
data(NLD_muni)
data(NLD_prov)


# Facets defined by constant values
tm_shape(World) +
    tm_fill(c("green", "blue")) +
	tm_facets(nrow = 2) +
tm_layout_World(c("A green world", "A blue world"))

# Facets defined by multiple variables
tm_shape(Europe) +
    tm_fill(c("gdp_cap_est", "pop_est_dens"), style="kmeans") +
tm_layout_Europe(c("GDP per capita", "Population density"))

tm_shape(NLD_muni) +
    tm_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"),
            style="kmeans") +
tm_shape(NLD_prov) +
    tm_borders() +
	tm_layout_NLD(c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44",
		"Population 45 to 64", "Population 65 and older"), scale=1.5, draw.frame = TRUE)



# Facets defined by multiple variables
tm_shape(NLD_prov) +
	tm_borders() +
	tm_facets(by="name")

tm_shape(NLD_prov) +
	tm_fill() +
	tm_facets(by="name")


tm_shape(NLD_muni) +
	tm_borders() +
	tm_fill("population") +
tm_shape(NLD_prov) +
	tm_borders(lwd=2) +
	tm_facets(by="name") +
	tm_layout(asp=0)



tm_shape(Europe) +
	tm_fill("pop_est_dens") +
	tm_bubbles("pop_est") +
tm_facets(by="part") +
tm_layout_Europe()

data(rivers)
tm_shape(Europe) +
	tm_fill("pop_est_dens") +
	tm_bubbles("pop_est") +
tm_shape(rivers) +
	tm_lines() +
	tm_facets(by="type") +
	tm_layout_Europe()


tm_shape(Europe) +
	tm_fill("pop_est_dens") +
	tm_bubbles("pop_est") +
	tm_layout_Europe()


tm_shape(Europe) +
	tm_fill("pop_est_dens") +
	tm_bubbles("pop_est") +
tm_facets(by="part") +
	tm_layout_Europe()

## todos:
# fix asp ratios
# data_by == NA ?
data(rivers)

system.time({
	print(tm_shape(rivers) +
	tm_lines() +
tm_shape(NLD_prov) +
	tm_fill("green") +
	tm_facets(by="name") +
tm_layout(asp=0))
})

tm_shape(NLD_prov) +
	tm_fill() +
	tm_facets(by="name") +
tm_shape(rivers) +
	tm_lines() +
	tm_layout(asp=0)

system.time({
print(tm_shape(NLD_prov) +
	tm_fill() +
	tm_facets(by="name") +
	tm_shape(rivers) +
	tm_lines())
})







tm_shape(NLD_prov) +
	tm_fill() +
	tm_facets(by="name")
