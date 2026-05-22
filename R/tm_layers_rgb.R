#' Map layer: rgb images
#'
#' Map layer that an rgb image.. The used (multivariate) visual variable is `col`,
#' which should be specified with 3 or 4 variables for `tm_rgb()` and `tm_rgba()` respectively.
#' The first three correspond to the red, green, and blue channels. The optional
#' fourth is the alpha transparency channel.
#'
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines
#'   the color. `col` is a multivariate variable, with 3 (`tm_rgb`) or 4 (`tm_rgba`) numeric data variables. These can be specified via [tm_vars()] with `multivariate = TRUE`
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free  `r .doc_vv("col_alpha")`
#' @param options options passed on to the corresponding `opt_<layer_function>` function
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_rgb.R
#' @export
tm_rgb = function(col = tm_vars(n = 3, multivariate = TRUE),
				  col.scale = tm_scale_rgb(),
				  col.legend = tm_legend(),
				  col.chart = tm_chart_none(),
				  col.free = NA,
				  col_alpha = tm_const(),
				  col_alpha.scale = tm_scale(),
				  col_alpha.legend = tm_legend(),
				  col_alpha.chart = tm_chart_none(),
				  col_alpha.free = NA,
				  options = opt_tm_rgb(),
				  ...) {
	args = list(...)

	layer_fun = if ("called_from" %in% names(args)) {
		args$called_from
	} else {
		"tm_rgb"
	}

	if (any(v3_only("tm_rgb") %in% names(args)) || is.numeric(col.scale)) {
		v3_start_message()

		# second condition needed to catch tm_rgb(1, 2, 3)


		if (all(c("r", "g", "b") %in% names(args))) {
			# v3_tm_rgb(args$r, args$g, args$b) moved to step1_helper_facets
			col = tm_vars(c(args$r, args$g, args$b), n = -1, multivariate = TRUE)
		}
		if (is.numeric(col) && is.numeric(col.scale) && is.numeric(col.legend)) {
			col = tm_vars(c(col, col.scale, col.legend), n = -1, multivariate = TRUE)
			col.scale = tm_scale_rgb()
			col.legend = tm_legend()
		}


		if ("alpha" %in% names(args) && layer_fun == "tm_rgb") {
			col_alpha = args$alpha
			v3_message_rgb_alpha(layer_fun = layer_fun)
		}
		if ("saturation" %in% names(args)) {
			options$saturation = args$saturation
			v3_message_rgb_opt(layer_fun = layer_fun, opt = "saturation", args$saturation)
		}
		if ("interpolate" %in% names(args)) {
			options$interpolate = args$interpolate
			v3_message_rgb_opt(layer_fun = layer_fun, opt = "interpolate", args$interpolate)
		}
		if ("max.value" %in% names(args)) {
			col.scale$max_color_value = args$max.value
			v3_message_rgb_maxV(layer_fun = layer_fun, opt = "interpolate", args$max.value)
		}

		v3_start_message()

	}

	# unused arguments: typos?
	unused = setdiff(names(args), c(v3_only("tm_rgb"), "called_from"))

	if (length(unused)) {
		message_layer_unused_args(layer_fun, unused)
	}



	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.chart = col.chart, col.free = col.free,
								   col_alpha = col_alpha, col_alpha.scale = col_alpha.scale, col_alpha.legend = col_alpha.legend, col_alpha.chart = col_alpha.chart, col_alpha.free = col_alpha.free,
								   options = options))
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

#' @rdname tm_rgb
#' @param interpolate Should the raster image be interpolated? Currently only applicable in view mode (passed on to [`grid`][grid::rasterGrob()])
#' @param saturation `r .doc_opt("rgb.saturation")`
#' @param blend (Experimental) see the argument `op` in [`groupGrob`][grid::groupGrob()]
#' @details
#' ## Layer blending (`blend`)
#'
#' Blend modes control how a layer's pixels are combined with the pixels
#' beneath it. For each pixel, let \eqn{S} be the source (top layer) RGB
#' value and \eqn{D} be the destination (bottom layer) RGB value, both
#' normalised to \eqn{[0, 1]}.
#'
#' | `col_blend` | Formula | Use case |
#' | --- | --- | --- |
#' | `"over"` | \eqn{S \cdot \alpha + D \cdot (1 - \alpha)} | Standard alpha compositing (default) |
#' | `"multiply"` | \eqn{S \times D} | Hillshading over colour raster; both layers darken each other |
#' | `"screen"` | \eqn{1 - (1 - S)(1 - D)} | Inverse of multiply; brightens |
#' | `"overlay"` | multiply if \eqn{D < 0.5}, screen if \eqn{D \geq 0.5} | Boosts contrast; preserves midtones |
#'
#' **Example**: with `"multiply"`, a grey hillshade pixel \eqn{S = (0.8, 0.8,
#' 0.8)} multiplied by a blue elevation pixel \eqn{D = (0.2, 0.4, 0.9)} gives
#' \eqn{(0.16, 0.32, 0.72)} — the colour is preserved but darkened where the
#' hillshade is dark, producing natural-looking terrain shading without
#' transparency.
#'
#' Requires R >= 4.2 and a compatible graphics device (e.g.
#' `png(type = "cairo")`, `svg()`). See [`grid::groupGrob()`] for the full
#' list of supported operators.
#' @export
opt_tm_rgb = function(interpolate = FALSE, saturation = 1, blend = "over") {
	list(trans.args = list(),
		 mapping.args = list(interpolate = interpolate, saturation = saturation, blend = blend))
}
