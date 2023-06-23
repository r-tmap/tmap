#' @export
tm_cartogram = function(size = 1,
						size.scale = tm_scale(),
						size.legend = tm_legend_hide(),
						size.free = NA,
						plot.order = tm_plot_order("size", reverse = FALSE),
						trans.args = list(type = c("cont", "ncont", "dorling"), itermax = 15),
						...) {
	po = plot.order
	type = match.arg(type)
	tmp = do.call(tm_polygons, list(...))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.args = trans.args
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
