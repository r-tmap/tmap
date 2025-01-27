# NEED TO SYNC THIS WITH tmap_options
# generate all options with:
# args = intersect(names(tmap_options_mode("view")),names(tmap_options_mode("plot")))
# cat(paste(args, collapse = ", "))
#' Layout options
#'
#' Specify the layout of the maps. [tm_layout()] is identical as [tm_options()] but only contain the tmap options that are directly related to the layout. [tm_style()] sets the style for the map. A style is a specified set of options (that can be changed afterwards with [tm_layout()]). These functions are used within a plot a plot call (stacked with the `+` operator). Their counterparts [tmap_options()] and [tmap_style()] can be used to set the (layout) options globally.
#'
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_layout}{Vignette about layout}, \href{https://r-tmap.github.io/tmap/articles/adv_margins}{vignette about margins and aspect ratio} and \href{https://r-tmap.github.io/tmap/articles/adv_options}{vignette about options}
#' @inheritParams tmap_options
#' @example ./examples/tm_layout.R
#' @export
tm_layout = function(
		scale, asp, bg.color, outer.bg.color, frame, frame.lwd, frame.r, frame.double_line, outer.margins, inner.margins, inner.margins.extra, meta.margins, meta.auto_margins, between_margin, panel.margin, component.offset, component.stack_margin, grid.mark.height, xylab.height, coords.height, xlab.show, xlab.text, xlab.size, xlab.color, xlab.rotation, xlab.space, xlab.fontface, xlab.fontfamily, xlab.alpha, xlab.side, ylab.show, ylab.text, ylab.size, ylab.color, ylab.rotation, ylab.space, ylab.fontface, ylab.fontfamily, ylab.alpha, ylab.side, panel.type, panel.wrap.pos, panel.xtab.pos, unit, color.sepia_intensity, color.saturation, color_vision_deficiency_sim, text.fontface, text.fontfamily, text.alpha, component.position, component.autoscale, legend.show, legend.design, legend.orientation, legend.position, legend.width, legend.height, legend.stack, legend.group.frame, legend.resize_as_group, legend.reverse, legend.na.show, legend.title.color, legend.title.size, legend.title.fontface, legend.title.fontfamily, legend.title.alpha, legend.xlab.color, legend.xlab.size, legend.xlab.fontface, legend.xlab.fontfamily, legend.xlab.alpha, legend.ylab.color, legend.ylab.size, legend.ylab.fontface, legend.ylab.fontfamily, legend.ylab.alpha, legend.text.color, legend.text.size, legend.text.fontface, legend.text.fontfamily, legend.text.alpha, legend.frame, legend.frame.lwd, legend.frame.r, legend.bg.color, legend.bg.alpha, legend.only, legend.absolute_fontsize, legend.settings.standard.portrait, legend.settings.standard.landscape, chart.show, chart.plot.axis.x, chart.plot.axis.y, chart.position, chart.width, chart.height, chart.stack, chart.group.frame, chart.resize_as_group, chart.reverse, chart.na.show, chart.title.color, chart.title.size, chart.title.fontface, chart.title.fontfamily, chart.title.alpha, chart.xlab.color, chart.xlab.size, chart.xlab.fontface, chart.xlab.fontfamily, chart.xlab.alpha, chart.ylab.color, chart.ylab.size, chart.ylab.fontface, chart.ylab.fontfamily, chart.ylab.alpha, chart.text.color, chart.text.size, chart.text.fontface, chart.text.fontfamily, chart.text.alpha, chart.frame, chart.frame.lwd, chart.frame.r, chart.bg.color, chart.bg.alpha, chart.object.color, title.size, title.color, title.fontface, title.fontfamily, title.alpha, title.bg.color, title.bg.alpha, title.padding, title.frame, title.frame.lwd, title.frame.r, title.stack, title.position, title.width, title.group.frame, title.resize_as_group, credits.size, credits.color, credits.fontface, credits.fontfamily, credits.alpha, credits.bg.color, credits.bg.alpha, credits.padding, credits.frame, credits.frame.lwd, credits.frame.r, credits.stack, credits.position, credits.width, credits.height, credits.group.frame, credits.resize_as_group, compass.north, compass.type, compass.text.size, compass.size, compass.show.labels, compass.cardinal.directions, compass.text.color, compass.color.dark, compass.color.light, compass.lwd, compass.bg.color, compass.bg.alpha, compass.margins, compass.stack, compass.position, compass.frame, compass.frame.lwd, compass.frame.r, compass.group.frame, compass.resize_as_group, logo.height, logo.margins, logo.between_margin, logo.stack, logo.position, logo.frame, logo.frame.lwd, logo.frame.r, logo.group.frame, logo.resize_as_group, scalebar.breaks, scalebar.width, scalebar.text.size, scalebar.text.color, scalebar.color.dark, scalebar.color.light, scalebar.lwd, scalebar.bg.color, scalebar.bg.alpha, scalebar.size, scalebar.margins, scalebar.stack, scalebar.position, scalebar.frame, scalebar.frame.lwd, scalebar.frame.r, scalebar.group.frame, scalebar.resize_as_group, grid.show, grid.labels.pos, grid.x, grid.y, grid.n.x, grid.n.y, grid.crs, grid.col, grid.lwd, grid.alpha, grid.labels.show, grid.labels.size, grid.labels.col, grid.labels.rot, grid.labels.format, grid.labels.cardinal, grid.labels.margin.x, grid.labels.margin.y, grid.labels.space.x, grid.labels.space.y, grid.labels.inside_frame, grid.ticks, grid.lines, grid.ndiscr, mouse_coordinates.stack, mouse_coordinates.position, mouse_coordinates.show, minimap.server, minimap.toggle, minimap.stack, minimap.position, minimap.show, panel.show, panel.labels, panel.label.size, panel.label.color, panel.label.fontface, panel.label.fontfamily, panel.label.alpha, panel.label.bg.color, panel.label.frame, panel.label.frame.lwd, panel.label.frame.r, panel.label.height, panel.label.rot, bbox, qtm.scalebar, qtm.minimap, qtm.mouse_coordinates, earth_boundary, earth_boundary.color, earth_boundary.lwd, earth_datum, space.color, check_and_fix, basemap.show, basemap.server, basemap.alpha, basemap.zoom, tiles.show, tiles.server, tiles.alpha, tiles.zoom, attr.color,
		title = NULL,
		...

	) {

	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_layout"
	do.call(tm_options, args)
}

#' View mode options
#'
#' View mode options. These options are specific to the view mode.
#'
#' @param use_browser If `TRUE` it opens an external browser, and `FALSE` (default) it opens the internal IDEs (e.g. RStudio) browser.
#' @param use_WebGL use webGL for points, lines, and polygons. For large spatial objects, this is much faster than the standard leaflet layer functions. However, it can not always be used for two reasons. First, the number of visual variables are limited; only fill, size, and color (for lines) are supported. Second, projected CRS's are not supported. Furthermore, it has the drawback that polygon borders are not as sharp. By default only `TRUE` for large spatial objects (500 or more features) when the mentioned criteria are met.
#'  By default `TRUE` if no other visual variables are used.
#' @param control.position `r .doc_opt("control.position")`
#' @param control.bases base layers
#' @param control.overlays overlay layers
#' @param control.collapse Should the box be collapsed or expanded?
#' @param set_bounds logical that determines whether maximum bounds are set,
#'   or a bounding box. Not applicable in plot mode.
#'   In view mode, this is passed on to [setMaxBounds()][leaflet::setMaxBounds()]
#' @param set_view numeric vector that determines the view.
#'   Either a vector of three: `lng`, `lat`, and `zoom`, or a single value:
#'    `zoom`. See [setView()][leaflet::setView()].
#'    Only applicable if `bbox` is not specified
#' @param set_zoom_limits numeric vector of two that set the minimum and maximum
#'   zoom levels (see [tileOptions()][leaflet::tileOptions()]).
#' @param use_circle_markers If `TRUE` (default) circle shaped symbols (e.g. `tm_dots()` and `tm_symbols()`) will be rendered as [addCircleMarkers()][leaflet::addCircleMarkers()] instead of [addMarkers()][leaflet::addMarkers()]. The former is faster, the latter can support any symbol since it is based on icons
#' @param leaflet.options options passed on to
#'   [leafletOptions()][leaflet::leafletOptions()]
#' @param ... to catch deprecated arguments
#' @export
#' @seealso [tm_group()]
tm_view = function(use_browser,
				   use_WebGL,
				   control.position,
				   control.bases,
				   control.overlays,
				   control.collapse,
				   set_bounds,
				   set_view,
				   set_zoom_limits,
				   use_circle_markers,
				   leaflet.options,
				   ...) {

	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())
	args$called_from = "tm_view"
	do.call(tm_options, args)

}

#' Plot mode options
#'
#' Plot mode options. This option is specific to the plot mode.
#'
#' @param use_gradient Use gradient fill using [linearGradient()][grid::linearGradient()]
#' @export
tm_plot = function(use_gradient) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	do.call(tm_options, args)
}


#' @rdname tm_extra_innner_margin
#' @export
tm_place_legends_right = function(width = NA) {
	if (is.na(width)) {
		tm_options(legend.position = tm_pos_out("right", "center"))
	} else {
		tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_pos_out("right", "center"))
	}
}

#' @rdname tm_extra_innner_margin
#' @param width width
#' @export
tm_place_legends_left = function(width = NA) {
	if (is.na(width)) {
		tm_options(legend.position = tm_pos_out("left", "center"))
	} else {
		tm_options(meta.margins = c(0, 0, 0, width), legend.position = tm_pos_out("right", "center"))
	}
}

#' @rdname tm_extra_innner_margin
#' @param height height
#' @export
tm_place_legends_bottom = function(height = NA) {
	if (is.na(height)) {
		tm_options(legend.position = tm_pos_out("center", "bottom"))
	} else {
		tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_pos_out("center", "bottom"))
	}
}

#' @rdname tm_extra_innner_margin
#' @export
tm_place_legends_top = function(height = NA) {
	if (is.na(height)) {
		tm_options(legend.position = tm_pos_out("center", "top"))
	} else {
		tm_options(meta.margins = c(height, 0, 0, 0), legend.position = tm_pos_out("center", "top"))
	}
}

#' @rdname tm_extra_innner_margin
#' @param pos.h,pos.v position (horizontal and vertical)
#' @export
tm_place_legends_inside = function(pos.h = NULL, pos.v = NULL) {
	if (is.null(pos.h) || is.null(pos.v)) {
		if (!is.null(pos.h)) {
			warning("tm_place_legends_inside also requires pos.v", call. = FALSE)
		} else if (!is.null(pos.v)) {
			warning("tm_place_legends_inside also requires pos.h", call. = FALSE)
		}
		tm_options(legend.position = tm_pos_auto_in())
	} else {
		tm_options(legend.position = tm_pos_in(pos.h = pos.h, pos.v = pos.v))
	}
}

#' tmap layout: helper functions
#'
#' @param left,right,top,bottom extra margins
#' @export
#' @name tm_extra_innner_margin
tm_extra_innner_margin = function(left = 0, right = 0, top = 0, bottom = 0) {
	tm_options(inner.margins.extra = c(bottom, left, top, right))
}

#' @rdname tmap_options
#' @export
tm_check_fix = function() {
	tm_options(check_and_fix = TRUE)
}


#' @rdname tm_layout
#' @param style name of the style
#' @export
#' @order 1
tm_style = function(style, ...) {
	args = list(...)

	check_style(style)

	args$style = style
	args$called_from = "tm_style"
	do.call(tm_options, args)
}

