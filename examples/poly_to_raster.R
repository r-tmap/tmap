\dontrun{
	data(NLD_muni)
	
	# choropleth of 65+ population percentages
	qtm(NLD_muni, fill="pop_65plus", format="NLD")
	
	# rasterized version
	NLD_rst <- poly_to_raster(NLD_muni, copy.data = TRUE)
	qtm(NLD_rst, raster="pop_65plus", format="NLD")
}
