#' @rdname tm_raster
#' @name opt_tm_rgb
#' @export
opt_tm_rgb = function(interpolate = FALSE) {
	list(trans.args = list(),
		 mapping.args = list(interpolate = interpolate))
}

#' Map layer: RGB
#' 
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines
#'   the col color.
#' @param options options passed on to the corresponding `opt_<layer_function>` function 
#' @export
tm_rgb = function(col = tm_mv(1:3),
				  col.scale = tm_scale_rgb(),
				  col.legend = tm_legend(),
				  col.chart = tm_chart_none(),
				  col.free = NA,
				  options = opt_tm_rgb()) {
	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.chart = col.chart, col.free = col.free, options = options))
}
