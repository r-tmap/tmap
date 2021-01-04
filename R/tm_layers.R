tm_polygons = function(fill = "blue", 
					   fill.setup = tm_aes_color_discrete(),
					   color = "gray30",
					   color.setup = tm_aes_color_discrete()) {

	tm_element_list(tm_element(
		trans.fun = tmapTransPolygon,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(fill = tmapAes(fun = tmapAesColorDiscrete,
										  value = fill,
										  setup = fill.setup),
						   color = tmapAes(fun = tmapAesColorDiscrete,
						   				value = color,
						   				setup = color.setup)),
		plotting.fun = "Polygon",
		subclass = c("tm_aes_layer", "tm_layer")))
}


tm_borders = function(color = "gray30",
					  color.setup = tm_aes_color_discrete()) {
	
	tm_element_list(tm_element(
		trans.fun = tmapTransPolygon,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(fill = NULL,
						   color = tmapAes(fun = tmapAesColorDiscrete,
						   				value = color,
						   				setup = color.setup)),
		plotting.fun = "Polygon",
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_cartogram = function(...,
						size = 1,
						size.setup = tm_aes_2d_size) {
	
	tmp = do.call(tm_polygons, list(...))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.aes = list(size = tmapAes(fun = tmapAes2dSize,
										value = size,
										setup = size.setup))
		trans.isglobal = TRUE
	})
	tmp
}

tm_raster = function(color = "blue", 
					 color.setup = tm_aes_color_discrete()) {
	
	tm_element_list(tm_element(
		trans.fun = tmapTransRaster,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(color = tmapAes(fun = tmapAesColorDiscrete,
						   				value = color,
						   				setup = color.setup)),
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
	
	tm_element_list(tm_element(
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(color = tmapAes(fun = tmapAesColorDiscrete,
						   				value = color,
						   				setup = color.setup),
						   size = tmapAes(fun = tmapAes2dSize,
						   			   value = size,
						   			   setup = size.setup),
						   shape = tmapAes(fun = tmapAesShape,
						   				value = shape,
						   				setup = shape.setup)),
		
		plotting.fun = "Symbol",
		subclass = c("tm_aes_layer", "tm_layer")))
}

