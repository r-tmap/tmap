\dontrun{
	## smooth polygons
	data(NLD_muni)
	
	NLD_muni$population_dens <- calc_densities(NLD_muni, "population")
	
	qtm(NLD_muni, fill="population_dens")
	
	NLD_muni_smooth <- smooth_raster(NLD_muni, var = "population_dens")
	
	qtm(NLD_muni_smooth, layout.bg.color="grey80")
	
	
	## smooth points
	data(metro)
	
	metro_smooth <- smooth_raster(metro)
	
	qtm(metro_smooth)
	
	
	## smooth raster
	data(land)
	
	land_smooth <- smooth_raster(land, var="trees", cover.type = "smooth")
	
	qtm(land_smooth, layout.bg.color="grey80")
}