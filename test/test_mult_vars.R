data(World)
data(Europe)
data(NLD_muni)
data(NLD_prov)
data(metro)

tm_shape(World) +
	tm_polygons(c("pop_est_dens", "income_grp"), style="kmeans", palette="YlOrRd", title=c("Population per km2", "test")) +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_layout_World()

tm_shape(World) +
	tm_polygons(c("pop_est_dens", "pop_est_dens"), style="kmeans", palette=list("YlOrRd", "-Blues"), title="Population per km2", n=c(10,3)) +
	tm_text("iso_a3", size="AREA", scale=1.5) +
	tm_layout_World()

tm_shape(World) + tm_polygons() +
tm_shape(metro) +
	tm_bubbles(size = c("pop1950", "pop2030"), col = c("pop1950", "pop2030"), palette=list("Reds", "Set3"), scale = c(1,2), sizes.legend = list(seq(5,30,by=10), NULL))
