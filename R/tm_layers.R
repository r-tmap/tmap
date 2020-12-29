tm_polygons = function(fill = "blue", 
					   fill.setup = tm_aes_color_discrete(),
					   color = "gray30",
					   color.setup = tm_aes_color_discrete()) {

	tm_element_list(tm_element(trans.fun = tmapTransPolygon,
		trans.isglobal = FALSE,
		trans.aes = list(),
		trans.aes.setup = list(),
		mapping.fun = tmapMapPolygon,
		mapping.aes = list(fill = fill,
		   			      color = color),
		mapping.aes.setup = list(fill = fill.setup,
		   					    color = color.setup),
		plotting.fun = "Polygon",
		subclass = c("tm_aes_layer", "tm_layer")))
}


tm_borders = function(color = "gray30",
					  color.setup = tm_aes_color_discrete()) {
	
	tm_element_list(tm_element(trans.fun = tmapTransPolygon,
		trans.isglobal = FALSE,
		trans.aes = list(),
		trans.aes.setup = list(),
		mapping.fun = tmapMapPolygon,
		mapping.aes = list(fill = NULL,
		   		      color = color),
		mapping.aes.setup = list(fill = list(),
		   				    color = color.setup),
		plotting.fun = "Polygon",
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_cartogram = function(...,
						size = 1,
						size.setup = tm_aes_2d_size) {
	
	tmp = do.call(tm_polygons, list(...))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.isglobal = TRUE
		trans.aes = list(size = size)
		trans.aes.setup = list(size.setup = size.setup)
	})
	tmp
}

tm_raster = function(color = "blue", 
					 color.setup = tm_aes_color_discrete()) {
	
	tm_element_list(tm_element(trans.fun = tmapTransRaster,
							   trans.isglobal = FALSE,
							   trans.aes = list(),
							   trans.aes.setup = list(),
							   mapping.fun = tmapMapRaster,
							   mapping.aes = list(color = color),
							   mapping.aes.setup = list(color = color.setup),
							   plotting.fun = "Raster",
							   subclass = c("tm_aes_layer", "tm_layer")))
}

tm_rgb = function(color = MV(1:3),
				  color.setup = tm_aes_color_rgb()) {
	do.call(tm_raster, color = color, color.setup = color.setup)
}

tm_symbols = function(color = "blue", 
					  size = 1, 
					  shape = 19, 
					  color.setup = tm_aes_color_discrete(), 
					  size.setup = tm_aes_2d_size(),
					  shape.setup = tm_aes_shape()) {
	aes.trans = character()
	aes.mapping = c("color", "size", "shape")
	aes.trans.fun = tmapTransCentroid
	aes.trans.isglobal = FALSE
	aes.mapping.fun = tmapMapPoint
	tm_element_list(tm_element(as.list(environment()), 
							   subclass = c("tm_symbols", "tm_layer")))
	
	tm_element_list(tm_element(trans.fun = tmapTransCentroid,
							   trans.isglobal = FALSE,
							   trans.aes = list(),
							   trans.aes.setup = list(),
							   mapping.fun = tmapMapPoint,
							   mapping.aes = list(color = color,
							   				   size = size,
							   				   shape = shape),
							   mapping.aes.setup = list(color = color.setup,
							   						 size = size.setup,
							   						 shape = shape.setup),
							   plotting.fun = "Symbol",
							   subclass = c("tm_aes_layer", "tm_layer")))
}






