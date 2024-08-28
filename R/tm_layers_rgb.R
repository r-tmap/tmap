#' Map layer: rgb images
#' 
#' Map layer that an rgb image.. The used (multivariate) visual variable is `col`,
#' which should be specified with 3 or 4 variables for `tm_rgb()` and `tm_rgba()` respectively.
#' The first three correspond to the red, green, and blue channels. The optional
#' fourth is the alpha transparency channel.
#' 
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines
#'   the col color. `col` is a multivariate variable, with 3 (`tm_rgb`) or 4 (`tm_rgba`) numeric data variables. These can be specified via [tm_vars()] with `multivariate = TRUE`
#' @param options options passed on to the corresponding `opt_<layer_function>` function 
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_rgb.R 
#' @export
tm_rgb = function(col = tm_vars(n = 3, multivariate = TRUE),
				  col.scale = tm_scale_rgb(),
				  col.legend = tm_legend(),
				  col.chart = tm_chart_none(),
				  col.free = NA,
				  options = opt_tm_rgb(),
				  ...) {
	args = list(...)
	args_called = as.list(match.call()[-1])
	
	if (any(v3_only("tm_rgb") %in% names(args)) || is.numeric(col.scale)) {
		v3_start_message()
		
		# second condition needed to catch tm_rgb(1, 2, 3)
		layer_fun = if ("called_from" %in% names(args)) {
			args$called_from
		} else {
			"tm_rgb"
		}
		
		if (all(c("r", "g", "b") %in% names(args))) {
			v3_tm_rgb(args$r, args$g, args$b)
			col = tm_vars(c(args$r, args$g, args$b), multivariate = TRUE)
		}
		if (is.numeric(col) && is.numeric(col.scale) && is.numeric(col.legend)) {
			col = tm_vars(c(col, col.scale, col.legend), multivariate = TRUE)
			col.scale = tm_scale_rgb()
			col.legend = tm_legend()
		}
		
		v3_start_message()
		
	}
	
	
	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.chart = col.chart, col.free = col.free, options = options))
}

#' @rdname tm_rgb
#' @export
tm_rgba = function(col = tm_vars(n = 4, multivariate = TRUE),
				  col.scale = tm_scale_rgba(),
				  col.legend = tm_legend(),
				  col.chart = tm_chart_none(),
				  col.free = NA,
				  options = opt_tm_rgb()) {
	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.chart = col.chart, col.free = col.free, options = options))
}

#' @name opt_tm
#' @param interpolate Should the raster image be interpolated? Currently only applicable in view mode (passed on to [`grid`][grid::rasterGrob()])
opt_tm_rgb = function(interpolate = FALSE) {
	list(trans.args = list(),
		 mapping.args = list(interpolate = interpolate))
}