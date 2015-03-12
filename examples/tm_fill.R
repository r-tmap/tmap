data(World)
data(Europe)
data(NLD_muni)
data(NLD_prov)


# Constant fill
tm_shape(World) + tm_fill("green3") + tm_layout_World(title="A green World")

# Data variable containing colours values
Europe$isNLD <- ifelse(Europe$name=="Netherlands", "darkorange", "forestgreen")
tm_shape(Europe) +
	tm_fill("isNLD") +
	tm_layout("Find the Netherlands!")

# Data variable containing numeric values
tm_shape(Europe) +
	tm_fill("gdp_cap_est", style="kmeans", textNA = "Non-European countries") +
	tm_borders() +
	tm_text("iso_a3", cex="AREA", root=4, scale=2) +
	tm_layout_Europe("GDP per capita")

tm_shape(World) +
    tm_fill("pop_est_dens", style="kmeans", palette="YlOrRd") +
    tm_borders() +
    tm_text("iso_a3", cex="AREA", cex.lowerbound=.4) +
tm_layout_World(title="Population density per km2")


# Data variable containing categorical values
tm_shape(World) +
    tm_fill("income_grp", palette="-Blues") +
    tm_borders() +
    tm_text("iso_a3", cex="AREA", scale=1.5) +
tm_layout_World("Income classification")

tm_shape(NLD_prov) + 
    tm_fill("name") + 
tm_shape(NLD_muni) + 
    tm_borders() + 
tm_shape(NLD_prov) + 
    tm_borders(lwd=2) +
    tm_text("name", shadow=TRUE) +
tm_layout_NLD("Provinces and municipalities", legend.show=FALSE, bg.color="white")

