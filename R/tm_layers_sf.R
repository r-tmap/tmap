#' Map layer: simple features
#'
#' Map layer that draws simple features as they are. Supported visual variables
#' are: `fill` (the fill color), `col` (the border color), `size` the point size,
#' `shape` the symbol shape, `lwd` (line width), `lty` (line type), `fill_alpha` (fill color alpha transparency)
#' and `col_alpha` (border color alpha transparency).
#'
#' @param fill,fill.scale,fill.legend,fill.free  `r .doc_vv("fill")`
#' @param col,col.scale,col.legend,col.free  `r .doc_vv("col")`
#' @param size,size.scale,size.legend,size.free  `r .doc_vv("size")`
#' @param shape,shape.scale,shape.legend,shape.free  `r .doc_vv("shape")`
#' @param lwd,lwd.scale,lwd.legend,lwd.free  `r .doc_vv("lwd")`
#' @param lty,lty.scale,lty.legend,lty.free  `r .doc_vv("lty")`
#' @param fill_alpha,fill_alpha.scale,fill_alpha.legend,fill_alpha.free  `r .doc_vv("fill_alpha")`
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free  `r .doc_vv("col_alpha")`
#' @param linejoin,lineend line join and line end. See [gpar()][grid::gpar()] for details.
#' @param plot.order.list Specification in which order the spatial features are drawn.
#'   This consists of a list of three elementary geometry types: for polygons, lines and, points.
#'   For each of these types, which are drawn in that order, a [tm_plot_order()] is required.
#' @param options options passed on to the corresponding `opt_<layer_function>` function
#' @inherit tm_polygons details
#' @inheritParams tm_polygons
#' @param ... passed on to [tm_polygons()], [tm_lines()], and [tm_dots()]
#' @example ./examples/tm_sf.R
#' @export
tm_sf = function(fill = tm_const(),
                 fill.scale = tm_scale(),
                 fill.legend = tm_legend(),
                 fill.free = NA,
                 col = tm_const(),
                 col.scale = tm_scale(),
                 col.legend = tm_legend(),
                 col.free = NA,
                 size = tm_const(),
                 size.scale = tm_scale(),
                 size.legend = tm_legend(),
                 size.free = NA,
                 shape = tm_const(),
                 shape.scale = tm_scale(),
                 shape.legend = tm_legend(),
                 shape.free = NA,
                 lwd = tm_const(),
                 lwd.scale = tm_scale(),
                 lwd.legend = tm_legend(),
                 lwd.free = NA,
                 lty = tm_const(),
                 lty.scale = tm_scale(),
                 lty.legend = tm_legend(),
                 lty.free = NA,
                 fill_alpha = tm_const(),
                 fill_alpha.scale = tm_scale(),
                 fill_alpha.legend = tm_legend(),
                 fill_alpha.free = NA,
                 col_alpha = tm_const(),
                 col_alpha.scale = tm_scale(),
                 col_alpha.legend = tm_legend(),
                 col_alpha.free = NA,
                 linejoin = "round",
                 lineend = "round",
                 plot.order.list = list(
                 	polygons = tm_plot_order("AREA"), lines = tm_plot_order("LENGTH"), points = tm_plot_order("size")
                 	),
				 options = opt_tm_sf(),
                  zindex = NA,
                  group = NA,
                  group.control = "check",
                  ...) {
	args = list(...)
	args_called = names(rlang::call_match()[-1])

	#paste(paste(names(formals(tm_polygons)), names(formals(tm_polygons)), sep = " = "), collapse = ", ")
	tm = tm_polygons(
		fill = fill, fill.scale = fill.scale, fill.legend = fill.legend,
		fill.free = fill.free, col = col, col.scale = col.scale, col.legend = col.legend,
		col.free = col.free, lwd = lwd, lwd.scale = lwd.scale, lwd.legend = lwd.legend,
		lwd.free = lwd.free, lty = lty, lty.scale = lty.scale, lty.legend = lty.legend,
		lty.free = lty.free, fill_alpha = fill_alpha, fill_alpha.scale = fill_alpha.scale,
		fill_alpha.legend = fill_alpha.legend, fill_alpha.free = fill_alpha.free,
		col_alpha = col_alpha, col_alpha.scale = col_alpha.scale,
		col_alpha.legend = col_alpha.legend, col_alpha.free = col_alpha.free,
		linejoin = linejoin, lineend = lineend, plot.order = plot.order.list$polygons,
		options = options$polygons) +
	tm_lines(
		col = col, col.scale = col.scale, col.legend = col.legend,
		col.free = col.free, lwd = lwd, lwd.scale = lwd.scale,
		lwd.legend = lwd.legend, lwd.free = lwd.free, lty = lty, lty.scale = lty.scale,
		lty.legend = lty.legend, lty.free = lty.free, col_alpha = col_alpha,
		col_alpha.scale = col_alpha.scale, col_alpha.legend = col_alpha.legend,
		col_alpha.free = col_alpha.free, linejoin = linejoin, lineend = lineend,
		plot.order = plot.order.list$lines, options = options$lines)


	if ("shape" %in% args_called || "col" %in% args_called) {
		tm + tm_symbols(size = size, size.scale = size.scale, size.legend = size.legend,
						size.free = size.free, fill = fill, fill.scale = fill.scale,
						fill.legend = fill.legend, fill.free = fill.free, col = col,
						col.scale = col.scale, col.legend = col.legend, col.free = col.free,
						shape = shape, shape.scale = shape.scale, shape.legend = shape.legend,
						shape.free = shape.free, lwd = lwd, lwd.scale = lwd.scale, lwd.legend = lwd.legend,
						lwd.free = lwd.free, lty = lty, lty.scale = lty.scale, lty.legend = lty.legend,
						lty.free = lty.free, fill_alpha = fill_alpha, fill_alpha.scale = fill_alpha.scale,
						fill_alpha.legend = fill_alpha.legend, fill_alpha.free = fill_alpha.free,
						col_alpha = col_alpha, col_alpha.scale = col_alpha.scale,
						col_alpha.legend = col_alpha.legend, col_alpha.free = col_alpha.free,
						plot.order = plot.order.list$points, options = options$points)
	} else {
		tm + tm_dots(fill = fill, fill.scale = fill.scale, fill.legend = fill.legend,
					 fill.free = fill.free, size = size, size.scale = size.scale,
					 size.legend = size.legend, size.free = size.free, lwd = lwd,
					 lwd.scale = lwd.scale, lwd.legend = lwd.legend, lwd.free = lwd.free,
					 lty = lty, lty.scale = lty.scale, lty.legend = lty.legend, lty.free = lty.free,
					 fill_alpha = fill_alpha, fill_alpha.scale = fill_alpha.scale,
					 fill_alpha.legend = fill_alpha.legend, fill_alpha.free = fill_alpha.free,
					 plot.order = plot.order.list$points, options = options$points)
	}

}

#' @param points.icon.scale scaling number that determines how large the icons (or grobs) are in plot mode in comparison to proportional symbols (such as bubbles). For view mode, use the argument `grob.dim`
#' @param points.grob.dim vector of four values that determine how grob objects (see details) are shown in view mode. The first and second value are the width and height of the displayed icon. The third and fourth value are the width and height of the rendered png image that is used for the icon. Generally, the third and fourth value should be large enough to render a ggplot2 graphic successfully. Only needed for the view mode.
#' @param points.just justification of the points relative to the point coordinates. Either one of the following values: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}, or a vector of two values where first value specifies horizontal and the second value vertical justification. Besides the mentioned values, also numeric values between 0 and 1 can be used. 0 means left justification for the first value and bottom justification for the second value. Note that in view mode, only one value is used.
#' @rdname tm_sf
#' @inheritParams tm_symbols
#' @inheritParams tm_polygons
#' @inheritParams tm_lines
#' @export
opt_tm_sf = function(polygons.only = "yes",
					 lines.only = "yes",
					 points_only = "yes",
					 point_per = "feature",
					 points.icon.scale = 3,
					 points.just = NA,
					 points.grob.dim = c(width = 48, height = 48, render.width = 256, render.height = 256)) {

	list(points = list(trans.args = list(points_only = points_only, point_per = point_per, on_surface = FALSE, along_lines = FALSE),
					   mapping.args = list(icon.scale = points.icon.scale,
					   					just = points.just,
					   					grob.dim = points.grob.dim)),
		 lines = list(trans.args = list(lines.only = lines.only),
		 			 mapping.args = list()),
		 polygons = list(trans.args = list(polygons.only = polygons.only),
		 				mapping.args = list()))

}
