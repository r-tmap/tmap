# NEED TO SYNC THIS WITH tmap_options
# generate all options with:
# args = intersect(names(tmap_options_mode("view")),names(tmap_options_mode("plot")))
# cat(paste(args, collapse = ", "))
#' Layout options
#'
#' Set of tmap options that are directly related to the layout.
#'
#' @param crs Map crs (see [tm_shape()]). `NA` means the crs is specified in [tm_shape()]. The crs that is used by the transformation functions is defined in [tm_shape()].
#' @param facet.max Maximum number of facets
#' @param facet.flip Should facets be flipped (in case of facet wrap)? This can also be set via [tm_facets_flip()]
#' @param free.scales For backward compatibility: if this value is set, it will be used to impute the free arguments in the layer functions
#' @param raster.max_cells Maximum number of raster grid cells
#' @param show.messages Show messages?
#' @param show.warnings Show warnings?
#' @param output.format Output format
#' @param output.size Output size
#' @param output.dpi Output dpi
#' @param output.dpi.animation Output dpi for animations
#' @param value.const Default visual value constants e.g. the default fill color for `tm_shape(World) + tm_polygons()`. A list is required with per visual variable a value.
#' @param value.na Default visual values that are used to visualize NA data values. A list is required with per visual variable a value.
#' @param value.null Default visual values that are used to visualize null (out-of-scope) data values. A list is required with per visual variable a value.
#' @param value.blank Default visual values that correspond to blank. For color these are `"#00000000"` meaning transparent. A list is required with per visual variable a value.
#' @param values.var Default values when a data variable to mapped to a visual variable, e.g. a color palette. A list is required with per visual variable a value.
#' @param values.range Default range for values. See `values.range` of [tm_scale_categorical()]. A list is required with per visual variable a value.
#' @param value.neutral Default values for when a data variable to mapped to a visual variable, e.g. a color palette. A list is required with per visual variable a value.
#' @param values.scale Default scales (as in object sizes) for values. See `values.range` of [tm_scale_categorical()]. A list is required with per visual variable a value.
#' @param scales.var Default scale functions per visual variable and type of data variable. A list is required with per visual variable per data type.
#' @param scale.misc.args Default values of scale function-specific arguments. A list is required with per scale function and optional per visual variable.
#' @param continuous.nclass_per_legend_break The number of continuous legend breaks within one 'unit' (label).  The dafault value is 50.
#' @param continuous.nclasses the number of classes of a continuous scale. Should be odd.  The dafault value is 101.
#' @param label.format Format for the labels (was `legend.format` in tmap v3).
#' @param label.na Default label for missing values.
#' @param scale
#' @param asp
#' @param bg.color
#' @param outer.bg.color
#' @param frame
#' @param frame.lwd
#' @param frame.r
#' @param frame.double_line
#' @param outer.margins
#' @param inner.margins
#' @param inner.margins.extra
#' @param meta.margins
#' @param meta.auto_margins
#' @param between_margin
#' @param panel.margin
#' @param component.offset
#' @param component.stack_margin
#' @param grid.mark.height
#' @param xylab.height
#' @param coords.height
#' @param xlab.show
#' @param xlab.text
#' @param xlab.size
#' @param xlab.color
#' @param xlab.rotation
#' @param xlab.space
#' @param xlab.fontface
#' @param xlab.fontfamily
#' @param xlab.side
#' @param ylab.show
#' @param ylab.text
#' @param ylab.size
#' @param ylab.color
#' @param ylab.rotation
#' @param ylab.space
#' @param ylab.fontface
#' @param ylab.fontfamily
#' @param ylab.side
#' @param panel.type
#' @param panel.wrap.pos
#' @param panel.xtab.pos
#' @param unit
#' @param color.sepia_intensity
#' @param color.saturation
#' @param color_vision_deficiency_sim
#' @param text.fontface
#' @param text.fontfamily
#' @param component.position
#' @param component.autoscale
#' @param legend.show
#' @param legend.design
#' @param legend.orientation
#' @param legend.position
#' @param legend.width
#' @param legend.height
#' @param legend.stack
#' @param legend.group.frame
#' @param legend.resize_as_group
#' @param legend.reverse
#' @param legend.na.show
#' @param legend.title.color
#' @param legend.title.size
#' @param legend.title.fontface
#' @param legend.title.fontfamily
#' @param legend.xlab.color
#' @param legend.xlab.size
#' @param legend.xlab.fontface
#' @param legend.xlab.fontfamily
#' @param legend.ylab.color
#' @param legend.ylab.size
#' @param legend.ylab.fontface
#' @param legend.ylab.fontfamily
#' @param legend.text.color
#' @param legend.text.size
#' @param legend.text.fontface
#' @param legend.text.fontfamily
#' @param legend.frame
#' @param legend.frame.lwd
#' @param legend.frame.r
#' @param legend.bg.color
#' @param legend.bg.alpha
#' @param legend.only
#' @param legend.settings.standard.portrait
#' @param legend.settings.standard.landscape
#' @param chart.show
#' @param chart.plot.axis.x
#' @param chart.plot.axis.y
#' @param chart.position
#' @param chart.width
#' @param chart.height
#' @param chart.stack
#' @param chart.group.frame
#' @param chart.resize_as_group
#' @param chart.reverse
#' @param chart.na.show
#' @param chart.title.color
#' @param chart.title.size
#' @param chart.title.fontface
#' @param chart.title.fontfamily
#' @param chart.xlab.color
#' @param chart.xlab.size
#' @param chart.xlab.fontface
#' @param chart.xlab.fontfamily
#' @param chart.ylab.color
#' @param chart.ylab.size
#' @param chart.ylab.fontface
#' @param chart.ylab.fontfamily
#' @param chart.text.color
#' @param chart.text.size
#' @param chart.text.fontface
#' @param chart.text.fontfamily
#' @param chart.frame
#' @param chart.frame.lwd
#' @param chart.frame.r
#' @param chart.bg.color
#' @param chart.bg.alpha
#' @param chart.object.color
#' @param title.show
#' @param title.size
#' @param title.color
#' @param title.fontface
#' @param title.fontfamily
#' @param title.bg.color
#' @param title.bg.alpha
#' @param title.padding
#' @param title.frame
#' @param title.frame.lwd
#' @param title.frame.r
#' @param title.stack
#' @param title.position
#' @param title.width
#' @param title.group.frame
#' @param title.resize_as_group
#' @param credits.show
#' @param credits.size
#' @param credits.color
#' @param credits.fontface
#' @param credits.fontfamily
#' @param credits.bg.color
#' @param credits.bg.alpha
#' @param credits.padding
#' @param credits.frame
#' @param credits.frame.lwd
#' @param credits.frame.r
#' @param credits.stack
#' @param credits.position
#' @param credits.width
#' @param credits.heigth
#' @param credits.group.frame
#' @param credits.resize_as_group
#' @param compass.north
#' @param compass.type
#' @param compass.text.size
#' @param compass.size
#' @param compass.show.labels
#' @param compass.cardinal.directions
#' @param compass.text.color
#' @param compass.color.dark
#' @param compass.color.light
#' @param compass.lwd
#' @param compass.bg.color
#' @param compass.bg.alpha
#' @param compass.margins
#' @param compass.show
#' @param compass.stack
#' @param compass.position
#' @param compass.frame
#' @param compass.frame.lwd
#' @param compass.frame.r
#' @param compass.group.frame
#' @param compass.resize_as_group
#' @param logo.height
#' @param logo.margins
#' @param logo.between_margin
#' @param logo.show
#' @param logo.stack
#' @param logo.position
#' @param logo.frame
#' @param logo.frame.lwd
#' @param logo.frame.r
#' @param logo.group.frame
#' @param logo.resize_as_group
#' @param scalebar.show
#' @param scalebar.breaks
#' @param scalebar.width
#' @param scalebar.text.size
#' @param scalebar.text.color
#' @param scalebar.color.dark
#' @param scalebar.color.light
#' @param scalebar.lwd
#' @param scalebar.bg.color
#' @param scalebar.bg.alpha
#' @param scalebar.size
#' @param scalebar.margins
#' @param scalebar.stack
#' @param scalebar.position
#' @param scalebar.frame
#' @param scalebar.frame.lwd
#' @param scalebar.frame.r
#' @param scalebar.group.frame
#' @param scalebar.resize_as_group
#' @param grid.show
#' @param grid.labels.pos
#' @param grid.x
#' @param grid.y
#' @param grid.n.x
#' @param grid.n.y
#' @param grid.crs
#' @param grid.col
#' @param grid.lwd
#' @param grid.alpha
#' @param grid.labels.show
#' @param grid.labels.size
#' @param grid.labels.col
#' @param grid.labels.rot
#' @param grid.labels.format
#' @param grid.labels.cardinal
#' @param grid.labels.margin.x
#' @param grid.labels.margin.y
#' @param grid.labels.space.x
#' @param grid.labels.space.y
#' @param grid.labels.inside_frame
#' @param grid.ticks
#' @param grid.lines
#' @param grid.ndiscr
#' @param mouse_coordinates.stack
#' @param mouse_coordinates.position
#' @param mouse_coordinates.show
#' @param minimap.server
#' @param minimap.toggle
#' @param minimap.stack
#' @param minimap.position
#' @param minimap.show
#' @param panel.show
#' @param panel.labels
#' @param panel.label.size
#' @param panel.label.color
#' @param panel.label.fontface
#' @param panel.label.fontfamily
#' @param panel.label.bg.color
#' @param panel.label.frame
#' @param panel.label.frame.lwd
#' @param panel.label.frame.r
#' @param panel.label.height
#' @param panel.label.rot
#' @param bbox
#' @param set.bounds
#' @param set.view
#' @param set.zoom.limits
#' @param qtm.scalebar
#' @param qtm.minimap
#' @param qtm.mouse_coordinates
#' @param earth.boundary
#' @param earth.boundary.color
#' @param earth.boundary.lwd
#' @param earth.datum
#' @param space.color
#' @param check_and_fix
#' @param basemap.show
#' @param basemap.server
#' @param basemap.alpha
#' @param basemap.zoom
#' @param tiles.show
#' @param tiles.server
#' @param tiles.alpha
#' @param tiles.zoom
#' @param attr.color
#' @param title,main.title deprecated See [tm_title()]
#' @param view.legend.position deprecated. Use `legend.position`from `tm_layout()` instead.
#' @param ... used to catch other deprecated arguments
#' @example ./examples/tm_layout.R
#' @name tm_layout
#' @export
tm_layout = function(
		crs, facet.max, facet.flip, free.scales, raster.max_cells, show.messages, show.warnings, output.format, output.size, output.dpi, output.dpi.animation, value.const, value.na, value.null, value.blank, values.var, values.range, value.neutral, values.scale, scales.var, scale.misc.args, continuous.nclass_per_legend_break, continuous.nclasses, label.format, label.na, scale, asp, bg.color, outer.bg.color, frame, frame.lwd, frame.r, frame.double_line, outer.margins, inner.margins, inner.margins.extra, meta.margins, meta.auto_margins, between_margin, panel.margin, component.offset, component.stack_margin, grid.mark.height, xylab.height, coords.height, xlab.show, xlab.text, xlab.size, xlab.color, xlab.rotation, xlab.space, xlab.fontface, xlab.fontfamily, xlab.side, ylab.show, ylab.text, ylab.size, ylab.color, ylab.rotation, ylab.space, ylab.fontface, ylab.fontfamily, ylab.side, panel.type, panel.wrap.pos, panel.xtab.pos, unit, color.sepia_intensity, color.saturation, color_vision_deficiency_sim, text.fontface, text.fontfamily, component.position, component.autoscale, legend.show, legend.design, legend.orientation, legend.position, legend.width, legend.height, legend.stack, legend.group.frame, legend.resize_as_group, legend.reverse, legend.na.show, legend.title.color, legend.title.size, legend.title.fontface, legend.title.fontfamily, legend.xlab.color, legend.xlab.size, legend.xlab.fontface, legend.xlab.fontfamily, legend.ylab.color, legend.ylab.size, legend.ylab.fontface, legend.ylab.fontfamily, legend.text.color, legend.text.size, legend.text.fontface, legend.text.fontfamily, legend.frame, legend.frame.lwd, legend.frame.r, legend.bg.color, legend.bg.alpha, legend.only, legend.settings.standard.portrait, legend.settings.standard.landscape, chart.show, chart.plot.axis.x, chart.plot.axis.y, chart.position, chart.width, chart.height, chart.stack, chart.group.frame, chart.resize_as_group, chart.reverse, chart.na.show, chart.title.color, chart.title.size, chart.title.fontface, chart.title.fontfamily, chart.xlab.color, chart.xlab.size, chart.xlab.fontface, chart.xlab.fontfamily, chart.ylab.color, chart.ylab.size, chart.ylab.fontface, chart.ylab.fontfamily, chart.text.color, chart.text.size, chart.text.fontface, chart.text.fontfamily, chart.frame, chart.frame.lwd, chart.frame.r, chart.bg.color, chart.bg.alpha, chart.object.color, title.show, title.size, title.color, title.fontface, title.fontfamily, title.bg.color, title.bg.alpha, title.padding, title.frame, title.frame.lwd, title.frame.r, title.stack, title.position, title.width, title.group.frame, title.resize_as_group, credits.show, credits.size, credits.color, credits.fontface, credits.fontfamily, credits.bg.color, credits.bg.alpha, credits.padding, credits.frame, credits.frame.lwd, credits.frame.r, credits.stack, credits.position, credits.width, credits.heigth, credits.group.frame, credits.resize_as_group, compass.north, compass.type, compass.text.size, compass.size, compass.show.labels, compass.cardinal.directions, compass.text.color, compass.color.dark, compass.color.light, compass.lwd, compass.bg.color, compass.bg.alpha, compass.margins, compass.show, compass.stack, compass.position, compass.frame, compass.frame.lwd, compass.frame.r, compass.group.frame, compass.resize_as_group, logo.height, logo.margins, logo.between_margin, logo.show, logo.stack, logo.position, logo.frame, logo.frame.lwd, logo.frame.r, logo.group.frame, logo.resize_as_group, scalebar.show, scalebar.breaks, scalebar.width, scalebar.text.size, scalebar.text.color, scalebar.color.dark, scalebar.color.light, scalebar.lwd, scalebar.bg.color, scalebar.bg.alpha, scalebar.size, scalebar.margins, scalebar.stack, scalebar.position, scalebar.frame, scalebar.frame.lwd, scalebar.frame.r, scalebar.group.frame, scalebar.resize_as_group, grid.show, grid.labels.pos, grid.x, grid.y, grid.n.x, grid.n.y, grid.crs, grid.col, grid.lwd, grid.alpha, grid.labels.show, grid.labels.size, grid.labels.col, grid.labels.rot, grid.labels.format, grid.labels.cardinal, grid.labels.margin.x, grid.labels.margin.y, grid.labels.space.x, grid.labels.space.y, grid.labels.inside_frame, grid.ticks, grid.lines, grid.ndiscr, mouse_coordinates.stack, mouse_coordinates.position, mouse_coordinates.show, minimap.server, minimap.toggle, minimap.stack, minimap.position, minimap.show, panel.show, panel.labels, panel.label.size, panel.label.color, panel.label.fontface, panel.label.fontfamily, panel.label.bg.color, panel.label.frame, panel.label.frame.lwd, panel.label.frame.r, panel.label.height, panel.label.rot, bbox, set.bounds, set.view, set.zoom.limits, qtm.scalebar, qtm.minimap, qtm.mouse_coordinates, earth.boundary, earth.boundary.color, earth.boundary.lwd, earth.datum, space.color, check_and_fix, basemap.show, basemap.server, basemap.alpha, basemap.zoom, tiles.show, tiles.server, tiles.alpha, tiles.zoom, attr.color,
		title = NULL,
		main.title = NULL,
		view.legend.position = NULL,
		...

	) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	fun = if ("called_from" %in% names(args)) {
		args$called_from
	} else {
		"tm_layout"
	}

	if (!missing(modes) && is.character(modes)) {
		title = modes
		args$modes = NULL
	}

	if (!is.null(view.legend.position)) {
		args$legend.position = args$view.legend.position
		args$legend.position = NULL
	}

	if (!is.null(title) || (!is.null(main.title))) {
		title1 = if (!is.null(title)) {
			title.args = args[substr(names(args), 1, 5) == "title"]
			title.args$title = NULL
			names(title.args) = substr(names(title.args), 7, nchar(names(title.args)))

			v3_title(fun)

			if (!("position" %in% names(title.args))) title.args$position = tm_pos_in("left", "top")
			args$title = NULL
			do.call(tm_title, c(list(text = title), title.args))
		} else {
			NULL
		}
		title2 = if (!is.null(main.title)) {
			main.title.args = args[substr(names(args), 1, 10) == "main.title"]
			main.title.args$main.title = NULL
			if ("main.title.position" %in% names(main.title.args)) {
				main.title.args$main.title.position = tm_pos_out(main.title.args$main.title.position[1], "top")
			}
			names(main.title.args) = substr(names(main.title.args), 12, nchar(names(main.title.args)))
			v3_main_title(fun)
			args$main.title = NULL
			do.call(tm_title, c(list(text = main.title), main.title.args))
		} else {
			NULL
		}
		do.call(tm_options, args) + title1 + title2
	} else {
		do.call(tm_options, args)
	}
}

#' View mode options
#'
#' View mode options. These options are specific to the view mode.
#'
#' @param use.WebGL use webGL for points, lines, and polygons. This is much faster than the standard leaflet layer functions, but the number of visual variables are limited; only fill, size, and color (for lines) are supported. By default `TRUE` if no other visual variables are used.
#' @param control.position position of the control attribute
#' @param control.bases base layers
#' @param control.overlays overlay layers
#' @param set.bounds logical that determines whether maximum bounds are set,
#'   or a bounding box. Not applicable in plot mode.
#'   In view mode, this is passed on to [setMaxBounds()][leaflet::setMaxBounds()]
#' @param set.view numeric vector that determines the view.
#'   Either a vector of three: `lng`, `lat`, and `zoom`, or a single value:
#'    `zoom`. See [setView()][leaflet::setView()].
#'    Only applicable if `bbox` is not specified
#' @param set.zoom.limits numeric vector of two that set the minimum and maximum
#'   zoom levels (see [tileOptions()][leaflet::tileOptions()]).
#' @param leaflet.options options passed on to
#'   [leafletOptions()][leaflet::leafletOptions()]
#' @param view.legend.position deprecated. Use `legend.position`from `tm_layout()` instead.
#' @export
tm_view = function(use.WebGL,
				   control.position,
				   control.bases,
				   control.overlays,
				   set.bounds,
				   set.view,
				   set.zoom.limits,
				   leaflet.options,
				   view.legend.position = NULL) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!is.null(view.legend.position)) {
		args$legend.position = view.legend.position
		args$view.legend.position = NULL
	}

	do.call(tm_options, args)
}

#' Plot mode options
#'
#' Plot mode options. This option is specific to the plot mode.
#'
#' @param use.gradient Use gradient fill using [linearGradient()][grid::linearGradient()]
#' @export
tm_plot = function(use.gradient) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	do.call(tm_options, args)
}


