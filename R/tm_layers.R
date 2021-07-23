tm_polygons = function(fill = "blue", 
					   fill.setup = tm_aes_color(),
					   fill.legend = tm_legend_portrait(),
					   fill.free = NA,
					   col = "gray30",
					   col.setup = tm_aes_color(),
					   col.legend = tm_legend_portrait(),
					   col.free = NA,
					   zindex = NA,
					   group = NA) {

	tm_element_list(tm_element(
		trans.fun = tmapTransPolygons,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(fill = tmapAes(value = fill,
										  setup = fill.setup,
										  legend = fill.legend,
										  free = fill.free),
						   col = tmapAes(value = col,
						   				setup = col.setup,
						   				legend = col.legend,
						   				free = col.free)),
		gpar = tmapGpar(fill = "__fill",
						col = "__col",
						shape = FALSE,
						size = NA,
						fill_alpha = 1,
						col_alpha = 1,
						pattern = "fill",
						lty = "solid",
						lwd = 1,
						linejoin = "round",
						lineend = "round"),
		mapping.fun = "Polygons",
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_borders = function(col = "gray30",
					  col.setup = tm_aes_color(),
					  col.legend = tm_legend_portrait(),
					  col.free = NA) {
	
	tm_element_list(tm_element(
		trans.fun = tmapTransPolygons,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(fill = NULL,
						   color = tmapAes(value = col,
						   				setup = col.setup,
						   				legend = col.legend,
						   				free = col.free)),
		mapping.fun = "Polygons",
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_cartogram = function(...,
						size = 1,
						size.setup = tm_aes_2d_size(),
						size.legend = tm_legend_hide(),
						size.free = NA) {
	
	tmp = do.call(tm_polygons, list(...))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.aes = list(size = tmapAes(value = size,
										setup = size.setup,
										legend = size.legend,
										free = size.free))
		trans.isglobal = TRUE
	})
	tmp
}

tm_raster = function(col = "blue", 
					 col.setup = tm_aes_color(),
					 col.legend = tm_legend_portrait(),
					 col.free = NA) {
	
	tm_element_list(tm_element(
		trans.fun = tmapTransRaster,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(color = tmapAes(value = col,
										   setup = col.setup,
										   legend = col.legend,
										   free = col.free)),
		gpar = tmapGpar(fill = "__col",
						col = "__col",
						shape = FALSE,
						size = NA,
						fill_alpha = 1,
						col_alpha = 1,
						pattern = "fill",
						lty = "blank",
						lwd = 0,
						linejoin = "round",
						lineend = "round"),
		mapping.fun = "Raster",
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_rgb = function(color = MV(1:3),
				  color.setup = tm_aes_color_rgb(),
				  color.legend = tm_legend_portrait(),
				  color.free = NA) {
	do.call(tm_raster, color = color, color.setup = color.setup, color.legend = color.legend, color.free = color.free)
}

tm_symbols = function(col = "blue", 
					  size = 1, 
					  shape = 21,
					  fill = "red",
					  col.setup = tm_aes_color(), 
					  size.setup = tm_aes_2d_size(),
					  shape.setup = tm_aes_shape(),
					  fill.setup = tm_aes_color(), 
					  col.legend = tm_legend_portrait(),
					  size.legend = tm_legend_portrait(),
					  shape.legend = tm_legend_portrait(),
					  fill.legend = tm_legend_portrait(),
					  col.free = NA,
					  size.free = NA,
					  shape.free = NA,
					  fill.free = NA) {
	
	tm_element_list(tm_element(
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapAes(value = col,
										   setup = col.setup,
										   legend = col.legend,
										   free = col.free),
						   size = tmapAes(value = size,
						   			   setup = size.setup,
						   			   legend = size.legend,
						   			   free = size.free),
						   shape = tmapAes(value = shape,
						   				setup = shape.setup,
						   				legend = shape.legend,
						   				free = shape.free),
						   fill = tmapAes(value = fill,
						   			  setup = fill.setup,
						   			  legend = fill.legend,
						   			  free = fill.free)),
		gpar = tmapGpar(fill = "__fill",
						col = "__col",
						shape = "__shape",
						size = "__size",
						fill_alpha = 1,
						col_alpha = 1,
						pattern = "fill",
						lty = "solid",
						lwd = 1,
						linejoin = "round",
						lineend = "round"),
		mapping.fun = "Symbols",
		subclass = c("tm_aes_layer", "tm_layer")))
}
