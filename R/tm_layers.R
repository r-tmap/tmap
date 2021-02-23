tm_polygons = function(fill = "blue", 
					   fill.setup = tm_aes_color_discrete(),
					   fill.free = NA,
					   color = "gray30",
					   color.setup = tm_aes_color_discrete(),
					   color.free = NA) {

	tm_element_list(tm_element(
		trans.fun = tmapTransPolygons,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(fill = tmapAes(value = fill,
										  setup = fill.setup,
										  free = fill.free),
						   color = tmapAes(value = color,
						   				setup = color.setup,
						   				free = color.free)),
		mapping.fun = "Polygons",
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_borders = function(color = "gray30",
					  color.setup = tm_aes_color_discrete(),
					  color.free = NA) {
	
	tm_element_list(tm_element(
		trans.fun = tmapTransPolygons,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(fill = NULL,
						   color = tmapAes(value = color,
						   				setup = color.setup,
						   				free = color.free)),
		mapping.fun = "Polygons",
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_cartogram = function(...,
						size = 1,
						size.setup = tm_aes_2d_size(),
						size.free = NA) {
	
	tmp = do.call(tm_polygons, list(...))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.aes = list(size = tmapAes(value = size,
										setup = size.setup,
										free = size.free))
		trans.isglobal = TRUE
	})
	tmp
}

tm_raster = function(color = "blue", 
					 color.setup = tm_aes_color_discrete(),
					 color.free = NA) {
	
	tm_element_list(tm_element(
		trans.fun = tmapTransRaster,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(color = tmapAes(value = color,
						   				setup = color.setup,
										free = color.free)),
		mapping.fun = "Raster",
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_rgb = function(color = MV(1:3),
				  color.setup = tm_aes_color_rgb(),
				  color.free = NA) {
	do.call(tm_raster, color = color, color.setup = color.setup, color.free = color.free)
}

tm_symbols = function(color = "blue", 
					  size = 1, 
					  shape = 19, 
					  color.setup = tm_aes_color_discrete(), 
					  size.setup = tm_aes_2d_size(),
					  shape.setup = tm_aes_shape(),
					  color.free = NA,
					  size.free = NA,
					  shape.free = NA) {
	
	tm_element_list(tm_element(
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(color = tmapAes(value = color,
						   				setup = color.setup,
										free = color.free),
						   size = tmapAes(value = size,
						   			   setup = size.setup,
						   			   free = size.free),
						   shape = tmapAes(value = shape,
						   				setup = shape.setup,
						   				free = shape.free)),
		
		mapping.fun = "Symbol",
		subclass = c("tm_aes_layer", "tm_layer")))
}
