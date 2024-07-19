#' @rdname tm_rgb
#' @name opt_tm_rgb
#' @param interpolate Should the raster image be interpolated? Currently only applicable in view mode (passed on to [`grid`][grid::rasterGrob()])
opt_tm_rgb = function(interpolate = FALSE) {
	list(trans.args = list(),
		 mapping.args = list(interpolate = interpolate))
}

#' Map layer: rgb images
#' 
#' Map layer that an rgb image.. The used (multivariate) visual variable is `col`, which should be specified with 3 or 4 variables for `tm_rgb` and `tm_rgba` respectively. The first three correspond to the red, green, and blue channels. The optional fourth is the alpha transparency channel.
#' 
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines
#'   the col color. `col` is a multivariate variable, with 3 (`tm_rgb`) or 4 (`tm_rgba`) numeric data variables. These can be specified via [tm_mv()] or [tm_mv_dim()]
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

#' @rdname tm_rgb
#' @export
tm_rgba = function(col = tm_mv(1:4),
				  col.scale = tm_scale_rgba(),
				  col.legend = tm_legend(),
				  col.chart = tm_chart_none(),
				  col.free = NA,
				  options = opt_tm_rgb()) {
	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.chart = col.chart, col.free = col.free, options = options))
}