# World examples
data(World)

tm_shape(World) + tm_fill("green3") + tm_layout_World(title="A green World")

World$highlighted <- ifelse(World$iso_a3 %in% c("GRL", "AUS"), "gold", "gray75")
tm_shape(World, projection="merc") + 
	tm_fill("highlighted") + 
	tm_borders() + 
	tm_layout("Mercator projection. Although used in Google Maps, it is discouraged for
statistical purposes. In reality, Australia is 3 times larger than Greenland!", 
			  inner.margins=c(0,0,.1,0), title.cex=.6)

tm_shape(World) +
    tm_fill("pop_est_dens", style="kmeans", palette="YlOrRd") +
    tm_borders() +
    tm_text("iso_a3", cex="AREA", cex.lowerbound=.4, bg.alpha=0) +
tm_layout_World(title="Population density per km2")

tm_shape(World) +
    tm_fill("income_grp", palette="-Blues") +
    tm_borders() +
    tm_text("iso_a3", cex="AREA", scale=1.5, bg.alpha=0) +
tm_layout_World("Income classification")

# Europe example
data(Europe)
tm_shape(Europe) +
    tm_fill("gdp_cap_est", style="kmeans", textNA = "Non-European countries") +
    tm_borders() +
    tm_text("iso_a3", cex="AREA", scale=2, bg.alpha=0) +
tm_layout_Europe("GDP per capita")

# Netherlands examples
data(NLD_muni)
data(NLD_prov)

tm_shape(NLD_prov) + 
    tm_fill("name") + 
tm_shape(NLD_muni) + 
    tm_borders() + 
tm_shape(NLD_prov) + 
    tm_borders(lwd=2) +
    tm_text("name") +
tm_layout_NLD("Provinces and municipalities", legend.show=FALSE)

tm_shape(NLD_muni) +
    tm_fill(col="population", convert2density=TRUE, style="kmeans") +
    tm_borders() +
tm_shape(NLD_prov) +
    tm_borders(, lwd=2) +
tm_layout_NLD(title="Population (per km2)", legend.digits=0)
