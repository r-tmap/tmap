## World example
data(World)
tm_shape(World) +
    tm_fill(c("green", "blue")) +
tm_layout_World(c("A green world", "A blue world"))

## Europe example
data(Europe)
tm_shape(Europe) +
    tm_fill(c("gdp_cap_est", "pop_est_dens"), style="kmeans") +
tm_layout_Europe(c("GDP per capita", "Population density"), scale=2)



## Netherlands example
data(NLD_muni)
data(NLD_prov)

tm_shape(NLD_muni) +
    tm_fill(c("pop_0_14", "pop_15_24", "pop_25_44", "pop_45_64", "pop_65plus"),
            convert2density=TRUE, style="kmeans") +
tm_shape(NLD_prov) +
    tm_borders() +
    tm_facets(free.scales=FALSE) +
tm_layout_NLD(c("Population 0 to 14", "Population 15 to 24", "Population 25 to 44", 
    "Population 45 to 64", "Population 65 and older"), scale=4, draw.frame = TRUE)
