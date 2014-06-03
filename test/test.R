data(Europe)
# names(Europe)
# [1] "iso_a3"       "name"         "sovereignt"   "continent"    "part"         "area"         "pop_est"      "pop_est_dens" "gdp_md_est"   "gdp_cap_est" 
# [11] "economy"      "income_grp" 


## fill tests

# constant
geo_shape(Europe) +
	geo_fill()

# color variable
Europe$color <- ifelse(Europe$iso_a3=="NLD", "orange", "steelblue")
geo_shape(Europe) +
	geo_fill("color")

# numeric variables
geo_shape(Europe) +
	geo_fill("gdp_cap_est")

geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "pop_est_dens"))

geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "pop_est_dens")) +
	geo_facets(free.scales.fill=FALSE)

# categorical variables
geo_shape(Europe) +
	geo_fill("continent")

geo_shape(Europe) +
	geo_fill(c("continent", "economy"))

geo_shape(Europe) +
	geo_fill(c("continent", "economy")) +
	geo_facets(free.scales=FALSE)

# mixed variables
geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "economy"))

geo_shape(Europe) +
	geo_fill(c("gdp_cap_est", "economy")) +
	geo_facets(free.scales.fill=FALSE)

# group by
geo_shape(Europe) +
	geo_fill("red") +
	geo_facets(by="part")

geo_shape(Europe) +
	geo_fill("color") +
	geo_facets(by="part")

geo_shape(Europe) +
	geo_fill("pop_est_dens") +
	geo_facets(by="part")

geo_shape(Europe) +
	geo_fill(c("pop_est_dens", "gdp_cap_est")) +
	geo_facets(by="part")

geo_shape(Europe) +
	geo_fill("pop_est_dens") +
	geo_facets(by="part", free.scales=TRUE)

geo_shape(Europe) +
	geo_fill("economy") +
	geo_facets(by="part")

geo_shape(Europe) +
	geo_fill("economy") +
	geo_facets(by="part", free.scales=FALSE)

geo_shape(Europe) +
	geo_fill("economy") +
	geo_facets(by="part", free.scales=TRUE)

