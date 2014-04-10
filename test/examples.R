data(Europe)
data(World)
data(NLD_muni)
data(NLD_prov)
data(NLD_ageGroups)

NLD_muni$gender <- (NLD_muni$men/NLD_muni$pop -.5) / sqrt(NLD_muni$pop) * 10000
World$pop_est_km <- calc_densities(World, "pop_est", total.area.km2=148940000)

# head(World[order(World$pop_est_km, decreasing=TRUE), c("name", "pop_est", "pop_est_km")], 100)
# areas <- getAreas(World)
# World$area <- areas * (148940000 / sum(areas))
# head(World[order(World$area, decreasing=TRUE), c("name", "area")], 100)


### Pretty example for the Netherlands
geo_shape(NLD_muni) +
geo_choropleth(col="pop", convert2density=TRUE, style="kmeans", total.area.km2=41543) +
geo_borders() +
geo_shape(NLD_prov) +
geo_borders(lwd=2) +
geo_text("name", bg.color="white", bg.alpha=150) +
geo_theme_NLD(title="Population (per km2)", bg.color="gray80", legend.digits=0, margins=rep(0,4))


## Pretty example World
(g <- geo_shape(World) +
 	geo_choropleth("pop_est_km", style="fixed", breaks=c(0, 5, 20, 100, 250, 1000, 30000), palette="YlOrRd", auto.palette.mapping=FALSE) +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3") +
 	geo_theme_World(legend.plot.type="none", title="Population density per km2"))

(g <- geo_shape(World) +
 	geo_choropleth("income_grp", palette="-Greens") +
 	geo_borders() +
 	geo_bubblemap("pop_est") +
 	geo_text("iso_a3", cex="AREA3") +
 	geo_theme_World("Income classification", legend.plot.cex=.2))

(g <- geo_shape(Europe) +
 	geo_choropleth("gdp_cap_est", style="kmeans") +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", scale=2) +
 	geo_theme_Europe("GDP per capita"))



(g <- geo_shape(NLD_muni) +
 	geo_choropleth(col="pop", convert2density=TRUE, style="kmeans", total.area.km2=41543)+
 	geo_borders(col="gray", lwd=1) +
 	#geo_text(NLD_muni, "code", cex=.3) +
 	geo_shape(NLD_prov, projection="robin") +
 	geo_borders(lwd=2) +
 	geo_text("name", cex=.5) +
 	geo_grid(free.scales=TRUE) +
 	geo_theme_NLD(title="Population"))

(g <- geo_shape(NLD_muni) +
 	geo_choropleth(col="gender", convert2density=FALSE, style="kmeans")+
 	geo_borders(col="gray", lwd=1) +
 	#geo_text(NLD_muni, "code", cex=.3) +
 	geo_shape(NLD_prov) +
 	geo_borders(lwd=2) +
 	geo_text("name", cex=.5) +
 	geo_grid(free.scales=TRUE) +
 	geo_theme_NLD(title="Gender"))

## small multiples

Rprof("../rprof.out", memory.profiling=TRUE)

pdf("../test.pdf")
(g <- geo_shape(NLD_muni) +
	geo_borders() +
	geo_choropleth(c("pop", "men", "women", "pop", "men", "women",
					 "pop", "men", "women", "pop", "men", "women")))
dev.off()


Rprof(NULL)

summaryRprof("../rprof.out", memory="both")

## legend

(g <- geo_shape(Europe) +
 	geo_choropleth("gdp_cap_est", style="kmeans") +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", scale=2) +
 	geo_theme_Europe("GDP per capita"))

(g <- geo_shape(Europe) +
 	geo_bubblemap("gdp_cap_est", style="kmeans") +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", scale=2) +
 	geo_theme_Europe("GDP per capita"))

(g <- geo_shape(Europe) +
 	geo_bubblemap(col="income_grp", style="kmeans") +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", scale=2) +
 	geo_theme_Europe("GDP per capita"))


(g <- geo_shape(Europe) +
 	geo_bubblemap("gdp_md_est", "income_grp", style="kmeans", scale=3) +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", scale=2) +
 	geo_theme_Europe("GDP per capita"))


(g <- geo_shape(Europe) +
 	geo_choropleth("gdp_cap_est", style="kmeans") +
 	geo_bubblemap("gdp_md_est", "income_grp", style="kmeans", scale=3) +
 	geo_borders() +
 	geo_text("iso_a3", cex="AREA3", scale=2) +
 	geo_theme_Europe("GDP per capita"))
