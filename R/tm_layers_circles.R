#' Map layer: geographic circles
#'
#' Map layer that draws circles with geographically fixed radii — i.e. the
#' radius is expressed in meters and the circles scale with zoom in interactive
#' (view) mode. This is in contrast to [tm_bubbles()], where the symbol size
#' is a fixed number of screen pixels.
#'
#' Supported map variables: `fill` (fill color), `col` (border color),
#' `size` (radius in meters), `lwd` (line width), `lty` (line type),
#' `fill_alpha` (fill transparency), `col_alpha` (border transparency).
#'
#' @param fill,fill.scale,fill.legend,fill.chart,fill.free `r .doc_vv("fill")` `r .doc_unit("fill")`
#' @param col,col.scale,col.legend,col.chart,col.free `r .doc_vv("col")` `r .doc_unit("col")`
#' @param size,size.scale,size.legend,size.chart,size.free `r .doc_vv("size")` `r .doc_unit("size.circles")`
#'   `r .doc_unit("size.circles")`
#' @param lwd,lwd.scale,lwd.legend,lwd.chart,lwd.free `r .doc_vv("lwd")` `r .doc_unit("lwd")`
#' @param lty,lty.scale,lty.legend,lty.chart,lty.free `r .doc_vv("lty")` `r .doc_unit("lty")`
#' @param fill_alpha,fill_alpha.scale,fill_alpha.legend,fill_alpha.chart,fill_alpha.free `r .doc_vv("fill_alpha")` `r .doc_unit("fill_alpha")`
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free `r .doc_vv("col_alpha")` `r .doc_unit("col_alpha")`
#' @param plot.order Specification in which order the spatial features are
#'   drawn. See [tm_plot_order()] for details.
#' @param options Options passed on to [opt_tm_circles()].
#' @param points_only Should only point geometries of the shape object be
#'   plotted? Default `"ifany"` means `TRUE` whenever the geometry collection
#'   contains points.
#' @param point_per How spatial points are generated for non-point geometries.
#'   One of `"feature"` (one point per feature, default), `"segment"` (one per
#'   sub-feature), or `"largest"` (only the largest sub-feature).
#' @param on_surface For polygon inputs, should the centroid be guaranteed to
#'   lie on the surface? If `TRUE` (slower), centroids outside the polygon are
#'   replaced via [sf::st_point_on_surface()].
#' @inheritParams tm_polygons
#' @inherit tm_polygons details
#' @param ... To catch deprecated arguments from version < 4.0.
#' @seealso
#'   * [tm_bubbles()] for screen-fixed proportional circles (pixel radius).
#'   * [tm_symbols()] for the general symbol layer with configurable shapes.
#'   * [tm_scale_asis()] to pass data values directly as metre radii.
#' @example examples/tm_circles.R
#' @export
tm_circles = function(size = tm_const(),
					  size.scale = tm_scale(),
					  size.legend = tm_legend(),
					  size.chart = tm_chart_none(),
					  size.free = NA,
					  fill = tm_const(),
					  fill.scale = tm_scale(),
					  fill.legend = tm_legend(),
					  fill.chart = tm_chart_none(),
					  fill.free = NA,
					  col = tm_const(),
					  col.scale = tm_scale(),
					  col.legend = tm_legend(),
					  col.chart = tm_chart_none(),
					  col.free = NA,
					  lwd = tm_const(),
					  lwd.scale = tm_scale(),
					  lwd.legend = tm_legend(),
					  lwd.chart = tm_chart_none(),
					  lwd.free = NA,
					  lty = tm_const(),
					  lty.scale = tm_scale(),
					  lty.legend = tm_legend(),
					  lty.chart = tm_chart_none(),
					  lty.free = NA,
					  fill_alpha = tm_const(),
					  fill_alpha.scale = tm_scale(),
					  fill_alpha.legend = tm_legend(),
					  fill_alpha.chart = tm_chart_none(),
					  fill_alpha.free = NA,
					  col_alpha = tm_const(),
					  col_alpha.scale = tm_scale(),
					  col_alpha.legend = tm_legend(),
					  col_alpha.chart = tm_chart_none(),
					  col_alpha.free = NA,
					  plot.order = tm_plot_order("size"),
					  zindex = NA,
					  group = NA,
					  group.control = "check",
					  popup = tm_popup(),
					  popup.vars = NA,
					  popup.format = tm_label_format(),
					  hover = NA,
					  id = "",
					  blend = "over",
					  options = opt_tm_circles(),
					  ...) {

	args = c(as.list(environment()), list(...))

	# tm_symbols expects a shape argument; fix it to a filled circle (21).
	# The "circles" layer class drives dispatch to the geographic-circle
	# DataPlot methods, which ignore shape entirely.
	args$shape = 21
	args$called_from = "tm_circles"
	args$options = complete_options(options, opt_tm_circles())

	tm = do.call(tm_symbols, args)
	tm[[1]]$layer = c("circles", "symbols")
	tm
}


#' @rdname tm_circles
#' @export
opt_tm_circles = function(points_only = "ifany",
						  point_per = "feature",
						  on_surface = FALSE) {
	list(
		trans.args = list(
			points_only = points_only,
			point_per   = point_per,
			on_surface  = on_surface,
			along_lines = FALSE
		),
		mapping.args = list()
	)
}
