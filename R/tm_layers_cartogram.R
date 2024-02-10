#' Map layer: cartogram
#' 
#' Map layer that draws a cartogram
#' 
#' @param size,size.scale,size.legend,size.free Transformation variable that
#'   determines the size of the polygons.
#' @param plot.order Specification in which order the spatial features are drawn.
#'   See [tm_plot_order()] for details.
#' @param trans.args lists that are passed on to internal transformation function
#' @inheritDotParams tm_polygons
#' @export
tm_cartogram = function(size = 1,
						size.scale = tm_scale(),
						size.legend = tm_legend_hide(),
						size.chart = tm_chart_none(),
						size.free = NA,
						plot.order = tm_plot_order("size", reverse = FALSE),
						trans.args = list(type = "cont", itermax = 15),
						...) {
	po = plot.order
	#trans.args$type = match.arg(trans.args$type)
	
	# types: "cont", "ncont", "dorling"
	
	tmp = do.call(tm_polygons, list(...))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.args = get("trans.args", envir = parent.env(environment()))
		trans.aes = list(size = tmapScale(aes = "area",
										  value = size,
										  scale = size.scale,
										  legend = size.legend,
										  chart = size.chart,
										  free = size.free))
		tpar = tmapTpar(area = "__area")
		trans.isglobal = TRUE
		plot.order = po
	})
	tmp
}
