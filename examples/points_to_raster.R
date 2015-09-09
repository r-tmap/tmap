\dontrun{
	data(NLD_muni)
	
	# sample points (each point represents 1000 people)
	NLD_muni_points <- sample_dots(NLD_muni, vars = "population", w=1000, convert2density = TRUE)
	
	# dot map
	tm_shape(NLD_muni_points) + tm_dots()
	
	# convert points to raster
	NLD_rst <- points_to_raster(NLD_muni_points, N = 1e4)
	
	# plot raster
	qtm(NLD_muni) + qtm(NLD_rst, raster="count")
}