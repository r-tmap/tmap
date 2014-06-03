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


## bubble tests
# names(Europe)
# [1] "iso_a3"       "name"         "sovereignt"   "continent"    "part"         "area"         "pop_est"      "pop_est_dens" "gdp_md_est"   "gdp_cap_est" 
# [11] "economy"      "income_grp" 


# constant
geo_shape(Europe) + geo_borders() +
	geo_bubbles()

geo_shape(Europe) + geo_borders() +
	geo_bubbles(col="red", scale=2)

geo_shape(Europe) + geo_borders() +
	geo_bubbles(size=c(.5, 1), col=c("red", "purple"), scale=1)

geo_shape(Europe) + geo_borders() +
	geo_bubbles(size=c(.5, 1, 2), col=c("red", "purple"), scale=1)

# size
geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", scale=2)

geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col=c("red", "purple"), scale=2)

geo_shape(Europe) + geo_borders() +
	geo_bubbles(c("pop_est", "gdp_md_est"), scale=2)

geo_shape(Europe) + geo_borders() +
	geo_bubbles(c("pop_est", "gdp_md_est"), scale=2) +
	geo_facets(free.scales=FALSE)

# color
geo_shape(Europe) + geo_borders() +
	geo_bubbles(col="income_grp", scale=1)

geo_shape(Europe) + geo_borders() +
	geo_bubbles(col=c("income_grp", "economy"), scale=1)

geo_shape(Europe) + geo_borders() +
	geo_bubbles(col=c("income_grp", "economy"), scale=1) +
	geo_facets(free.scales=FALSE)

# size and color
geo_shape(Europe) + geo_borders() +
	geo_bubbles(c("pop_est", "gdp_md_est"), col="income_grp", scale=2)

geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col=c("income_grp", "economy"), scale=2)

geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col=c("income_grp", "economy"), scale=2) +
	geo_facets(free.scales=FALSE)

# by
geo_shape(Europe) + geo_borders() +
	geo_bubbles() +
	geo_facets("part")

geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", scale=2) +
	geo_facets("part")

geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col="economy", scale=2) +
	geo_facets("part")

geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col="economy", scale=2) +
	geo_facets("part", free.scales.bubble.size=TRUE)

geo_shape(Europe) + geo_borders() +
	geo_bubbles("pop_est", col="economy", scale=2) +
	geo_facets("part", free.scales.bubble.col=TRUE)