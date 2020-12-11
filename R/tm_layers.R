tm_polygons = function(fill = "blue", 
					   fill.setup = tm_aes_color_discrete()) {
	aes.trans = character()
	aes.mapping = "fill"
	aes.trans.fun = tmapTransPolygon
	aes.trans.isglobal = FALSE
	aes.mapping.fun = tmapMapPolygon
	tm_element_list(tm_element(as.list(environment()), 
							   subclass = c("tm_layer", "tm_polygons")))
}

tm_cartogram = function(fill = "blue", 
					   fill.setup = tm_aes_color_discrete(),
					   size = 1,
					   size.setup = tm_aes_2d_size) {
	aes.trans = "size"
	aes.mapping = "fill"
	aes.trans.fun = tmapTransCartogram
	aes.trans.isglobal = TRUE
	aes.mapping.fun = tmapMapPolygon
	tm_element_list(tm_element(as.list(environment()), 
							   subclass = c("tm_layer", "tm_polygons")))
}

tm_raster = function(color = "blue", 
					 color.setup = tm_aes_color_discrete()) {
	aes.trans = character()
	aes.mapping = "color"
	aes.trans.fun = tmapTransRaster
	aes.trans.isglobal = TRUE
	aes.mapping.fun = tmapMapRaster
	tm_element_list(tm_element(as.list(environment()), 
							   subclass = c("tm_layer", "tm_raster")))
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
							   subclass = c("tm_layer", "tm_symbols")))
}

tm_compass = function( x = 1) {
	tm_element_list(tm_element(as.list(environment()), 
							   subclass = c("tm_layer", "tm_compass")))
}

