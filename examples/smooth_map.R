\dontrun{
	####################################
	## Smooth polygons
	####################################
	data(NLD_muni)
	
	NLD_muni$population_dens <- calc_densities(NLD_muni, "population")
	
	qtm(NLD_muni, fill="population_dens") + tm_grid()
	
	# NLD_muni_smooth <- smooth_raster(NLD_muni, var = "population_dens")
	# qtm(NLD_muni_smooth, layout.bg.color="grey80")
	
	randstad <- bb(xlim = c(50000, 150000), ylim=c(400000, 500000))
	randstad2 <- bb(xlim = c(50000, 100000), ylim=c(425000, 475000))
	
	NLD_muni_list <- smooth_map(NLD_muni, var = "population_dens")

	NLD_muni_list$iso$lev <- as.numeric(NLD_muni_list$iso$level)
	
	tm_shape(NLD_muni_list$iso, bbox = randstad2) + tm_iso(size = .5, along.lines = T)
	
	
	tm_shape(NLD_muni_list$iso, bbox = randstad2) + tm_lines() + tm_text("level", size = .5, along.lines = TRUE, overwrite.lines = T, auto.placement = F, remove.overlap = T, bg.color="green", bg.alpha=.25)
	
	qtm(NLD_muni_list$iso, line.col = "level", text="level")#, bubble.size="lev", bubble.col="level")
	qtm(NLD_muni_list$dasy, fill = "level", fill.palette="Blues")
	qtm(NLD_muni_list$raster, layout.bg.color="grey80", raster.n=20)
	

	####################################
	## Smooth points
	####################################
	data(World, metro)
	metro_eck <- set_projection(metro, projection = "eck4")
	
	# aggregate metropolitan population per country
	metro_per_country <- tapply(metro_eck$pop2010, INDEX = list(metro_eck$iso_a3), FUN=sum)
	metro_per_country_in_World <- metro_per_country[names(metro_per_country) %in% World$iso_a3]
	
	# assign to World shape
	World$pop_metro <- 0
	World$pop_metro[match(names(metro_per_country_in_World), World$iso_a3)] <- metro_per_country_in_World
	
	# define population density other than metropolitan areas
	World$pop_est_dens_non_metro <- (World$pop_est - World$pop_metro) / World$area

	# sample population dots from non-metropolitan areas (1 dot = 1mln people)
	World_pop <- sample_dots(World, vars="pop_est_dens_non_metro", w = 1e6, npop = 7.3e9)
	
	# generate dots for metropolitan areas (1 dot = 1mln people)
	metro_dots <- do.call("sbind", lapply(1:length(metro_eck), function(i) {
		m <- metro_eck[i,]
		m[rep(1, max(1, m$pop2010 %/% 1e6)),]
	}))
	
	# combine 
	World_1mln_dots <- sbind(as(World_pop, "SpatialPoints"), as(metro_dots, "SpatialPoints"))

	tm_shape(World_1mln_dots) + tm_dots()

	World_list <- smooth_map(World_1mln_dots, cover = World)
	qtm(World_list$raster, layout.bg.color="grey80")
	qtm(World, borders=NA) + qtm(World_list$iso)
	qtm(World_list$dasy, fill="level", fill.palette="Blues", layout.bg.color="grey80")
	
	####################################
	## Smooth raster
	####################################
	data(land)

	land_smooth <- smooth_map(land, var="trees", cover.type = "smooth")
	
	qtm(land, raster="trees", layout.bg.color="grey80")
	qtm(land_smooth$raster, layout.bg.color="grey80")
	qtm(land_smooth$iso)
	qtm(land_smooth$dasy, fill="level", fill.palette="Greens")
}
