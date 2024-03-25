#' @rdname tm_cartogram
#' @name opt_tm_cartogram
#' @export
opt_tm_cartogram = function(type = "cont",
							itermax = 15,
							...) {
	list(cartogram = list(mapping.args = list(),
						 trans.args = list(type = type, itermax = itermax)),
		  polygons = do.call(opt_tm_polygons, list(...)))
}

#' @rdname tm_cartogram
#' @name opt_tm_cartogram
#' @export
opt_tm_cartogram_ncont = function(type = "ncont",
								  expansion = 1,
								  inplace = FALSE,
								  ...) {
	
	list(cartogram = list(mapping.args = list(),
						  trans.args = list(type = type, expansion = expansion, inplace = inplace)),
		 polygons = do.call(opt_tm_polygons, list(...)))
}


#' @rdname tm_cartogram
#' @name opt_tm_cartogram
#' @export
opt_tm_cartogram_dorling = function(type = "dorling",
									share = 5,
									itermax = 1000,
									...) {
	list(cartogram = list(mapping.args = list(),
						  trans.args = list(type = type, share = share, itermax = itermax)),
		 polygons = do.call(opt_tm_polygons, list(...)))
}


#' Map layer: cartogram
#' 
#' Map layer that draws a cartogram
#' 
#' @param size,size.scale,size.legend,size.chart,size.free Transformation variable that
#'   determines the size of the polygons.
#' @param plot.order Specification in which order the spatial features are drawn.
#'   See [tm_plot_order()] for details.
#' @param options options passed on to the corresponding `opt_<layer_function>` function 
#' @inheritDotParams tm_polygons
#' @export
tm_cartogram = function(size = 1,
						size.scale = tm_scale(),
						size.legend = tm_legend_hide(),
						size.chart = tm_chart_none(),
						size.free = NA,
						plot.order = tm_plot_order("size", reverse = FALSE),
						options = opt_tm_cartogram(),
						...) {
	po = plot.order
	#trans.args$type = match.arg(trans.args$type)
	
	# types: "cont", "ncont", "dorling"
	
	tmp = do.call(tm_polygons, c(list(...), list(options = options$polygons)))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.args = options$cartogram$trans.args
		trans.aes = list(size = tmapScale(aes = "area",
										  value = size,
										  scale = size.scale,
										  legend = size.legend,
										  chart = size.chart,
										  free = size.free))
		tpar = tmapTpar(area = "__area")
		trans.isglobal = FALSE
		plot.order = po
	})
	tmp
}


tm_cartogram_ncont = function(size = 1,
							  size.scale = tm_scale(),
							  size.legend = tm_legend_hide(),
							  size.chart = tm_chart_none(),
							  size.free = NA,
							  plot.order = tm_plot_order("size", reverse = FALSE),
							  options = opt_tm_cartogram_ncont(),
							  ...) {
	args = list(...)
	do.call(tm_cartogram, c(list(size = size,
								 size.scale = size.scale,
								 size.legend = size.legend,
								 size.chart = size.chart,
								 size.free = size.free,
								 plot.order = plot.order,
								 options = options), args))
}



tm_cartogram_dorling = function(size = 1,
								size.scale = tm_scale(),
								size.legend = tm_legend_hide(),
								size.chart = tm_chart_none(),
								size.free = NA,
								plot.order = tm_plot_order("size", reverse = FALSE),
								options = opt_tm_cartogram_dorling(),
								...) {
	args = list(...)
	do.call(tm_cartogram, c(list(size = size,
								 size.scale = size.scale,
								 size.legend = size.legend,
								 size.chart = size.chart,
								 size.free = size.free,
								 plot.order = plot.order,
								 options = options), args))
}

