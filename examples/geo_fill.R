# World examples
data(World)

geo_shape(World) + geo_fill("green3") + geo_theme_World(title="A green World")

World$highlighted <- ifelse(World$iso_a3 %in% c("GRL", "AUS"), "gold", "gray75")
geo_shape(World, projection="merc") + 
	geo_fill("highlighted") + 
	geo_borders() + 
	geo_theme("Mercator projection. Although used in Google Maps, it is discouraged for
statistical purposes. In reality, Australia is 3 times larger than Greenland!", 
			  inner.margins=c(0,0,.1,0), title.cex=.6)

geo_shape(World) +
    geo_fill("pop_est_dens", style="kmeans", palette="YlOrRd") +
    geo_borders() +
    geo_text("iso_a3", cex="AREA", cex.lowerbound=.4, bg.alpha=0) +
geo_theme_World(title="Population density per km2")

geo_shape(World) +
    geo_fill("income_grp", palette="-Blues") +
    geo_borders() +
    geo_text("iso_a3", cex="AREA", scale=1.5, bg.alpha=0) +
geo_theme_World("Income classification")

# Europe example
data(Europe)
geo_shape(Europe) +
    geo_fill("gdp_cap_est", style="kmeans", textNA = "Non-European countries") +
    geo_borders() +
    geo_text("iso_a3", cex="AREA", scale=2, bg.alpha=0) +
geo_theme_Europe("GDP per capita")

# Netherlands examples
data(NLD_muni)
data(NLD_prov)

geo_shape(NLD_prov) + 
    geo_fill("name") + 
geo_shape(NLD_muni) + 
    geo_borders() + 
geo_shape(NLD_prov) + 
    geo_borders(lwd=2) +
    geo_text("name") +
geo_theme_NLD("Provinces and municipalities", legend.show=FALSE)

geo_shape(NLD_muni) +
    geo_fill(col="population", convert2density=TRUE, style="kmeans") +
    geo_borders() +
geo_shape(NLD_prov) +
    geo_borders(, lwd=2) +
geo_theme_NLD(title="Population (per km2)", legend.digits=0)
