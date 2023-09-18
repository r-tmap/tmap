#' Map layer: simple features
#' 
#' Map layer that draws simple features as they are. Supported visual variables are: `fill` (the fill color), `col` (the border color), `size` the point size, `shape` the symbol shape, `lwd` (line width), `lty` (line type), `fill_alpha` (fill color alpha transparency) and `col_alpha` (border color alpha transparency).
#' 
#' The visual variable arguments (e.g. `col`) can be specified with either a data variable name (of the object specified in [tm_shape()]), or with a visual value (for `col`, a color is expected). Multiple values can be specified: in that case facets are created. These facets can be combined with other faceting data variables, specified with [tm_facets()].
#' 
#' The `.scale` arguments determine the used scale to map the data values to visual variable values. These can be specified with one of the available `tm_scale_` functions. The default scale that is used is specified by the tmap option `scales.var`.
#' 
#' The `.legend` arguments determine the used legend, specified with [tm_legend()]. The default legend and its settings are determined by the tmap options `legend.`.
#' 
#' The `.free` arguments determine whether scales are applied freely across facets, or shared. A logical value is required. They can also be specified with a vector of three logical values; these determine whether scales are applied freely per facet dimension. This is only useful when facets are applied (see [tm_facets()]). There are maximally three facet dimensions: rows, columns, and pages. This only applies for a facet grid ([tm_facets_grid()]). For instance, `col.free = c(TRUE, FALSE, FALSE)` means that for the visual variable `col`, each row of facets will have its own scale, and therefore its own legend. For facet wraps and stacks ([tm_facets_wrap()] and [tm_facets_stack()]) there is only one facet dimension, so the `.free` argument requires only one logical value.
#' 
#' @param fill,fill.scale,fill.legend,fill.free Visual variable that determines the fill color. See details.
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color. See details.
#' @param size,size.scale,size.legend,size.free Visual variable that determines the size. See details.
#' @param shape,shape.scale,shape.legend,shape.free Visual variable that determines the shape. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.free Visual variable that determines the line type. See details.
#' @param fill_alpha,fill_alpha.scale,fill_alpha.legend,fill_alpha.free Visual variable that determines the fill color alpha transparency See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free Visual variable that determines the border color alpha transparency. See details.
#' @param linejoin,lineend line join and line end. See [grid::gpar()] for details.
#' @param plot.order.list Specification in which order the spatial features are drawn. This consists of a list of three elementary geometry types: for polygons, lines and, points. For each of these types, which are drawn in that order, a [tm_plot_order()] is required.
#' @param trans.args.list,mapping.args.list lists that are passed on to internal transformation and mapping functions respectively. Each is a list of three items, named `polygons`, `lines`, and `points`. See [tm_polygons()], [tm_lines()], and [tm_dots()].
#' @param zindex Map layers are drawn on top of each other. The `zindex` numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups can be switched on and off. Options: `"radio"` for radio buttons (meaning only one group can be shown), `"check"` for check boxes (so multiple groups can be shown), and `"none"` for no control (the group cannot be (de)selected).
#' @param ... passed on to [tm_polygons()], [tm_lines()], and [tm_dots()]
#' @example ./examples/tm_sf.R 
#' @export
#' @name tm_sf
#' @rdname tm_sf
tm_sf = function(	  fill = tm_const(),
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
					  plot.order.list = list(polygons = tm_plot_order("AREA"), lines = tm_plot_order("LENGTH"), points = tm_plot_order("size")),
					  trans.args.list = list(polygons = list(polygons.only = "yes"), lines = list(lines.only = "yes"), points = list(points.only = "yes")),
					  mapping.args.list = list(polygons = list(), lines = list(), points = list(icon.scale = 3, just = NA, grob.dim = c(width=48, height=48, render.width=256, render.height=256))),
					  zindex = NA,
					  group = NA,
					  group.control = "check",
					  ...) {
	
	args = list(...)
	calls = names(match.call(expand.dots = TRUE)[-1])
	
	
	
	#paste(paste(names(formals(tm_polygons)), names(formals(tm_polygons)), sep = " = "), collapse = ", ")
	tm = tm_polygons(fill = fill, fill.scale = fill.scale, fill.legend = fill.legend, fill.free = fill.free, col = col, col.scale = col.scale, col.legend = col.legend, col.free = col.free, lwd = lwd, lwd.scale = lwd.scale, lwd.legend = lwd.legend, lwd.free = lwd.free, lty = lty, lty.scale = lty.scale, lty.legend = lty.legend, lty.free = lty.free, fill_alpha = fill_alpha, fill_alpha.scale = fill_alpha.scale, fill_alpha.legend = fill_alpha.legend, fill_alpha.free = fill_alpha.free, col_alpha = col_alpha, col_alpha.scale = col_alpha.scale, col_alpha.legend = col_alpha.legend, col_alpha.free = col_alpha.free, linejoin = linejoin, lineend = lineend, plot.order = plot.order.list$polygons, trans.args = trans.args.list$polygons, mapping.args = mapping.args.list$polygons) +
	tm_lines(col = col, col.scale = col.scale, col.legend = col.legend, col.free = col.free, lwd = lwd, lwd.scale = lwd.scale, lwd.legend = lwd.legend, lwd.free = lwd.free, lty = lty, lty.scale = lty.scale, lty.legend = lty.legend, lty.free = lty.free, col_alpha = col_alpha, col_alpha.scale = col_alpha.scale, col_alpha.legend = col_alpha.legend, col_alpha.free = col_alpha.free, linejoin = linejoin, lineend = lineend, plot.order = plot.order.list$lines, trans.args = trans.args.list$lines, mapping.args = mapping.args.list$lines)
	
	
	if ("shape" %in% calls) {
		tm + tm_symbols(size = size, size.scale = size.scale, size.legend = size.legend, size.free = size.free, fill = fill, fill.scale = fill.scale, fill.legend = fill.legend, fill.free = fill.free, col = col, col.scale = col.scale, col.legend = col.legend, col.free = col.free, shape = shape, shape.scale = shape.scale, shape.legend = shape.legend, shape.free = shape.free, lwd = lwd, lwd.scale = lwd.scale, lwd.legend = lwd.legend, lwd.free = lwd.free, lty = lty, lty.scale = lty.scale, lty.legend = lty.legend, lty.free = lty.free, fill_alpha = fill_alpha, fill_alpha.scale = fill_alpha.scale, fill_alpha.legend = fill_alpha.legend, fill_alpha.free = fill_alpha.free, col_alpha = col_alpha, col_alpha.scale = col_alpha.scale, col_alpha.legend = col_alpha.legend, col_alpha.free = col_alpha.free, plot.order = plot.order.list$points, trans.args = trans.args.list$points, mapping.args = mapping.args.list$points)
	} else {
		tm + tm_dots(fill = fill, fill.scale = fill.scale, fill.legend = fill.legend, fill.free = fill.free, size = size, size.scale = size.scale, size.legend = size.legend, size.free = size.free, lwd = lwd, lwd.scale = lwd.scale, lwd.legend = lwd.legend, lwd.free = lwd.free, lty = lty, lty.scale = lty.scale, lty.legend = lty.legend, lty.free = lty.free, fill_alpha = fill_alpha, fill_alpha.scale = fill_alpha.scale, fill_alpha.legend = fill_alpha.legend, fill_alpha.free = fill_alpha.free, plot.order = plot.order.list$points, trans.args = trans.args.list$points, mapping.args = mapping.args.list$points)
	}
	
}
