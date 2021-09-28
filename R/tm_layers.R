#' export
tm_plot_order = function(aes, descending = TRUE) {
	structure(list(aes = aes, descending = descending), class = "tm_plot_order")
}

#' @export
tm_polygons = function(fill = tm_const(), 
					   fill.scale = tm_scale(),
					   fill.legend = tm_legend_portrait(),
					   fill.free = NA,
					   col = tm_const(),
					   col.scale = tm_scale(),
					   col.legend = tm_legend_portrait(),
					   col.free = NA,
					   lwd = tm_const(),
					   lwd.scale = tm_scale(),
					   lwd.legend = tm_legend_portrait(),
					   lwd.free = NA,
					   lty = tm_const(),
					   lty.scale = tm_scale(),
					   lty.legend = tm_legend_portrait(),
					   lty.free = NA,
					   fill_alpha = tm_const(),
					   fill_alpha.scale = tm_scale(),
					   fill_alpha.legend = tm_legend_portrait(),
					   fill_alpha.free = NA,
					   col_alpha = tm_const(),
					   col_alpha.scale = tm_scale(),
					   col_alpha.legend = tm_legend_portrait(),
					   col_alpha.free = NA,
					   linejoin = "round",
					   lineend = "round",
					   plot.order = tm_plot_order("AREA", descending = TRUE),
					   zindex = NA,
					   group = NA) {
	
	tm_element_list(tm_element(
		layer = "polygons",
		trans.fun = tmapTransPolygons,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(fill = tmapScale(aes = "fill",
											value = fill,
											scale = fill.scale,
											legend = fill.legend,
											free = fill.free),
						   col = tmapScale(aes = "col",
						   				value = col,
						   				scale = col.scale,
						   				legend = col.legend,
						   				free = col.free),
						   lwd = tmapScale(aes = "lwd",
						   				value = lwd,
						   				scale = lwd.scale,
						   				legend = lwd.legend,
						   				free = lwd.free),
						   lty = tmapScale(aes = "lty",
						   				value = lty,
						   				scale = lty.scale,
						   				legend = lty.legend,
						   				free = lty.free),
						   fill_alpha = tmapScale(aes = "fill_alpha",
						   					   value = fill_alpha,
						   					   scale = fill_alpha.scale,
						   					   legend = fill_alpha.legend,
						   					   free = fill_alpha.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
						   					  free = col_alpha.free)),
		
		gpar = tmapGpar(fill = "__fill",
						col = "__col",
						shape = NA,
						size = NA,
						fill_alpha = "__fill_alpha",
						col_alpha = "__col_alpha",
						pattern = "fill",
						lty = "__lty",
						lwd = "__lwd",
						linejoin = linejoin,
						lineend = lineend),
		tpar = tmapTpar(area = "AREA"),
		plot.order = plot.order,
		mapping.fun = "Polygons",
		subclass = c("tm_aes_layer", "tm_layer")))
}

#' @export
tm_borders = function(col = tm_const(),
					  col.scale = tm_scale(),
					  col.legend = tm_legend_portrait(),
					  col.free = NA,
					  lwd = tm_const(),
					  lwd.scale = tm_scale(),
					  lwd.legend = tm_legend_portrait(),
					  lwd.free = NA,
					  lty = tm_const(),
					  lty.scale = tm_scale(),
					  lty.legend = tm_legend_portrait(),
					  lty.free = NA,
					  col_alpha = tm_const(),
					  col_alpha.scale = tm_scale(),
					  col_alpha.legend = tm_legend_portrait(),
					  col_alpha.free = NA,
					  linejoin = "round",
					  lineend = "round",
					  zindex = NA,
					  group = NA) {
	
	tm_element_list(tm_element(
		layer = "polygons",
		trans.fun = tmapTransPolygons,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free),
						   lwd = tmapScale(aes = "lwd",
						   				value = lwd,
						   				scale = lwd.scale,
						   				legend = lwd.legend,
						   				free = lwd.free),
						   lty = tmapScale(aes = "lty",
						   				value = lty,
						   				scale = lty.scale,
						   				legend = lty.legend,
						   				free = lty.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
						   					  free = col_alpha.free)),
		
		gpar = tmapGpar(fill = NA,
						col = "__col",
						shape = NA,
						size = NA,
						fill_alpha = NA,
						col_alpha = "__col_alpha",
						pattern = NA,
						lty = "__lty",
						lwd = "__lwd",
						linejoin = linejoin,
						lineend = lineend),
		tpar = tmapTpar(),
		mapping.fun = "Polygons",
		subclass = c("tm_aes_layer", "tm_layer")))
}







#' @export
tm_cartogram = function(...,
						size = 1,
						size.scale = tm_scale(),
						size.legend = tm_legend_hide(),
						size.free = NA,
						plot.order = tm_plot_order("size", descending = TRUE)) {
	po = plot.order
	tmp = do.call(tm_polygons, list(...))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.aes = list(size = tmapScale(aes = "area",
										  value = size,
										  scale = size.scale,
										  legend = size.legend,
										  free = size.free))
		tpar = tmapTpar(area = "__area")
		trans.isglobal = TRUE
		plot.order = po
	})
	tmp
}


#' @export
tm_balloons = function(col = tm_const(),
					   col.scale = tm_scale(),
					   col.legend = tm_legend_portrait(),
					   col.free = NA,
					   size = 1,
					   size.scale = tm_scale(),
					   size.legend = tm_legend_hide(),
					   size.free = NA,
					   alpha = 0.5,
					   lwd = 2,
					   zindex = NA,
					   group = NA) {
	
	tm_element_list(tm_element(
		layer = "balloons",
		trans.fun = tmapTransCartogram,
		trans.aes = list(size = tmapScale(aes = "area",
										  value = size,
										  scale = size.scale,
										  legend = size.legend,
										  free = size.free)),
		trans.isglobal = TRUE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free)),
		
		gpar = tmapGpar(fill = "__col",
						col = "__col",
						shape = NA,
						size = NA,
						fill_alpha = alpha,
						col_alpha = 1,
						pattern = "fill",
						lty = NA,
						lwd = lwd,
						linejoin = NA,
						lineend = NA),
		tpar = tmapTpar(area = "__area"),
		mapping.fun = "Polygons",
		subclass = c("tm_aes_layer", "tm_layer")))
}


#' @export
tm_raster = function(col = tm_const(),
					 col.scale = tm_scale(),
					 col.legend = tm_legend_portrait(),
					 col.free = NA,
					 col_alpha = tm_const(),
					 col_alpha.scale = tm_scale(),
					 col_alpha.legend = tm_legend_portrait(),
					 col_alpha.free = NA,
					 zindex = NA,
					 group = NA) {
	
	tm_element_list(tm_element(
		layer = "raster",
		trans.fun = tmapTransRaster,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   				value = col_alpha,
						   				scale = col_alpha.scale,
						   				legend = col_alpha.legend,
						   				free = col_alpha.free)),
		
		gpar = tmapGpar(fill = "__col",
						col = NA,
						shape = NA,
						size = NA,
						fill_alpha = "__col_alpha",
						col_alpha = NA,
						pattern = "fill",
						lty = NA,
						lwd = NA,
						linejoin = NA,
						lineend = NA),
		tpar = tmapTpar(),
		mapping.fun = "Raster",
		subclass = c("tm_aes_layer", "tm_layer")))
}



#' @export
tm_rgb = function(col = tm_mv(1:3),
				  col.scale = tm_scale_rgb(),
				  col.legend = tm_legend_portrait(),
				  col.free = NA) {
	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.free = col.free))
}






#' @export
tm_symbols = function(fill = tm_const(),
					  fill.scale = tm_scale(),
					  fill.legend = tm_legend_portrait(),
					  fill.free = NA,
					  col = tm_const(),
					  col.scale = tm_scale(),
					  col.legend = tm_legend_portrait(),
					  col.free = NA,
					  size = tm_const(),
					  size.scale = tm_scale(),
					  size.legend = tm_legend_portrait(),
					  size.free = NA,
					  shape = tm_const(),
					  shape.scale = tm_scale(),
					  shape.legend = tm_legend_portrait(),
					  shape.free = NA,
					  lwd = tm_const(),
					  lwd.scale = tm_scale(),
					  lwd.legend = tm_legend_portrait(),
					  lwd.free = NA,
					  lty = tm_const(),
					  lty.scale = tm_scale(),
					  lty.legend = tm_legend_portrait(),
					  lty.free = NA,
					  fill_alpha = tm_const(),
					  fill_alpha.scale = tm_scale(),
					  fill_alpha.legend = tm_legend_portrait(),
					  fill_alpha.free = NA,
					  col_alpha = tm_const(),
					  col_alpha.scale = tm_scale(),
					  col_alpha.legend = tm_legend_portrait(),
					  col_alpha.free = NA,
					  zindex = NA,
					  group = NA) {
	
	tm_element_list(tm_element(
		layer = "symbols",
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free),
						   fill = tmapScale(aes = "fill",
											value = fill,
											scale = fill.scale,
											legend = fill.legend,
											free = fill.free),
						   size = tmapScale(aes = "size",
						   				value = size,
						   				scale = size.scale,
						   				legend = size.legend,
						   				free = size.free),
						   shape = tmapScale(aes = "shape",
						   				value = shape,
						   				scale = shape.scale,
						   				legend = shape.legend,
						   				free = shape.free),
						   lwd = tmapScale(aes = "lwd",
						   				value = lwd,
						   				scale = lwd.scale,
						   				legend = lwd.legend,
						   				free = lwd.free),
						   lty = tmapScale(aes = "lty",
						   				value = lty,
						   				scale = lty.scale,
						   				legend = lty.legend,
						   				free = lty.free),
						   fill_alpha = tmapScale(aes = "fill_alpha",
						   					   value = fill_alpha,
						   					   scale = fill_alpha.scale,
						   					   legend = fill_alpha.legend,
						   					   free = fill_alpha.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
						   					  free = col_alpha.free)),
		
		gpar = tmapGpar(fill = "__fill",
						col = "__col",
						shape = "__shape",
						size = "__size",
						fill_alpha = "__fill_alpha",
						col_alpha = "__col_alpha",
						pattern = "fill",
						lty = "__lty",
						lwd = "__lwd",
						linejoin = NA,
						lineend = NA),
		tpar = tmapTpar(),
		mapping.fun = "Symbols",
		subclass = c("tm_aes_layer", "tm_layer")))
}
