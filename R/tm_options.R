#' tmap options
#'
#' @inheritParams tmap_options
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_layout}{Vignette about layout}, \href{https://r-tmap.github.io/tmap/articles/adv_margins}{vignette about margins and aspect ratio} and \href{https://r-tmap.github.io/tmap/articles/adv_options}{vignette about options}
#' @param style style see `tm_style()`
#' @export
tm_options = function(crs, facet.max, free.scales, raster.max_cells, raster.warp, show.messages, show.warnings, output.format, output.size, output.dpi, animation.dpi, value.const, value.na, value.null, value.blank, values.var, values.range, value.neutral, values.scale, scales.var, scale.misc.args, continuous.nclass_per_legend_break, continuous.nclasses, label.format, label.na, scale, asp, bg, bg.color, outer.bg, outer.bg.color, frame, frame.color, frame.alpha, frame.lwd, frame.r, frame.double_line, outer.margins, inner.margins, inner.margins.extra, meta.margins, meta.auto_margins, between_margin, panel.margin, xlab.show, xlab.text, xlab.size, xlab.color, xlab.rotation, xlab.space, xlab.fontface, xlab.fontfamily, xlab.alpha, xlab.side, ylab.show, ylab.text, ylab.size, ylab.color, ylab.rotation, ylab.space, ylab.fontface, ylab.fontfamily, ylab.alpha, ylab.side, panel.type, panel.wrap.pos, panel.xtab.pos, unit, color.sepia_intensity, color.saturation, color_vision_deficiency_sim, text.fontface, text.fontfamily, r, component.position, component.offset, component.stack_margin, component.autoscale, component.resize_as_group, component.frame_combine, component.stack, legend.stack, chart.stack, component.equalize, component.frame, component.frame.color, component.frame.alpha, component.frame.lwd, component.frame.r, component.bg, component.bg.color, component.bg.alpha, legend.show, legend.orientation, legend.position, legend.width, legend.height, legend.reverse, legend.na.show, legend.title.color, legend.title.size, legend.title.fontface, legend.title.fontfamily, legend.title.alpha, legend.xlab.color, legend.xlab.size, legend.xlab.rot, legend.xlab.fontface, legend.xlab.fontfamily, legend.xlab.alpha, legend.ylab.color, legend.ylab.size, legend.ylab.rot, legend.ylab.fontface, legend.ylab.fontfamily, legend.ylab.alpha, legend.text.color, legend.text.size, legend.text.fontface, legend.text.fontfamily, legend.text.alpha, legend.frame, legend.frame.color, legend.frame.alpha, legend.frame.lwd, legend.frame.r, legend.bg, legend.bg.color, legend.bg.alpha, legend.only, legend.absolute_fontsize, legend.settings.portrait, legend.settings.landscape, add_legend.position, chart.show, chart.plot.axis.x, chart.plot.axis.y, chart.position, chart.width, chart.height, chart.reverse, chart.na.show, chart.title.color, chart.title.size, chart.title.fontface, chart.title.fontfamily, chart.title.alpha, chart.xlab.color, chart.xlab.size, chart.xlab.fontface, chart.xlab.fontfamily, chart.xlab.alpha, chart.ylab.color, chart.ylab.size, chart.ylab.fontface, chart.ylab.fontfamily, chart.ylab.alpha, chart.text.color, chart.text.size, chart.text.fontface, chart.text.fontfamily, chart.text.alpha, chart.frame, chart.frame.color, chart.frame.alpha, chart.frame.lwd, chart.frame.r, chart.bg, chart.bg.color, chart.bg.alpha, chart.object.color, title.size, title.color, title.fontface, title.fontfamily, title.alpha, title.padding, title.frame, title.frame.color, title.frame.alpha, title.frame.lwd, title.frame.r, title.position, title.width, credits.size, credits.color, credits.fontface, credits.fontfamily, credits.alpha, credits.padding, credits.position, credits.width, credits.height, compass.north, compass.type, compass.text.size, compass.size, compass.show.labels, compass.cardinal.directions, compass.text.color, compass.color.dark, compass.color.light, compass.lwd, compass.margins, compass.position, inset.position, logo.height, logo.margins, logo.between_margin, logo.position, inset_map.height, inset_map.width, inset_map.margins, inset_map.between_margin, inset_map.position, inset_map.frame, inset.height, inset.width, inset.margins, inset.between_margin, inset.frame, inset.bg, inset.bg.color, inset.bg.alpha, inset_grob.height, inset_grob.width, inset_gg.height, inset_gg.width, scalebar.breaks, scalebar.width, scalebar.allow_clipping, scalebar.text.size, scalebar.text.color, scalebar.text.fontface, scalebar.text.fontfamily, scalebar.color.dark, scalebar.color.light, scalebar.lwd, scalebar.size, scalebar.margins, scalebar.position, grid.show, grid.labels.pos, grid.x, grid.y, grid.n.x, grid.n.y, grid.crs, grid.col, grid.lwd, grid.alpha, grid.labels.show, grid.labels.size, grid.labels.col, grid.labels.fontface, grid.labels.fontfamily, grid.labels.rot, grid.labels.format, grid.labels.cardinal, grid.labels.margin.x, grid.labels.margin.y, grid.labels.space.x, grid.labels.space.y, grid.labels.inside_frame, grid.ticks, grid.lines, grid.ndiscr, mouse_coordinates.position, minimap.server, minimap.toggle, minimap.position, panel.show, panel.labels, panel.label.size, panel.label.color, panel.label.fontface, panel.label.fontfamily, panel.label.alpha, panel.label.bg, panel.label.bg.color, panel.label.bg.alpha, panel.label.frame, panel.label.frame.color, panel.label.frame.alpha, panel.label.frame.lwd, panel.label.frame.r, panel.label.height, panel.label.rot, qtm.scalebar, qtm.minimap, qtm.mouse_coordinates, earth_boundary, earth_boundary.color, earth_boundary.lwd, earth_datum, space, space.color, space_overlay, check_and_fix, basemap.show, basemap.server, basemap.alpha, basemap.zoom, tiles.show, tiles.server, tiles.alpha, tiles.zoom, attr.color, crs_extra, crs_global, crs_basemap,
					  title = NULL,
					  main.title = NULL,
					  main.title.size = NULL,
					  main.title.color = NULL,
					  main.title.fontface = NULL,
					  main.title.fontfamily = NULL,
					  main.title.position = NULL,
					  fontface = NULL,
					  fontfamily = NULL,
					  style,
					  ...) {
	args_called = names(rlang::call_match()[-1])
	args = lapply(as.list(rlang::call_match(dots_expand = TRUE)[-1]), eval, envir = parent.frame())

	fun = if ("called_from" %in% args_called) {
		args$called_from
	} else {
		"tm_options"
	}
	args$called_from = NULL

	if ("panel.labels" %in% names(args)) {
		if (is.list(args$panel.labels)) {
			args$panel.labels = lapply(args$panel.labels, encode_expr)
		} else {
			args$panel.labels = encode_expr(args$panel.labels)
		}
	}

	if (!is.null(args$scale) && is.character(args$scale)) {
		# probably caused by tm_layout("my title"
		args$title = scale
		args$scale = NULL
		args_called = unique(c(setdiff(args_called, "scale"), "title"))
	}

	if ("view.legend.position" %in% args_called) {
		args$legend.position = args$view.legend.position
		args$view.legend.position = NULL
		v3_opt(fun, "view.legend.position", "legend.position")
		args_called = unique(c(setdiff(args_called, "view.legend.position"), "legend.position"))
	}
	if ("set.bounds" %in% args_called) {
		args$set_bounds = args$set.bounds
		args$set.bounds = NULL
		v3_opt(fun, "set.bounds", "set_bounds")
		args_called = unique(c(setdiff(args_called, "set.bounds"), "set_bounds"))
	}
	if ("set.view" %in% args_called) {
		args$set_view = args$set.view
		args$set.view = NULL
		v3_opt(fun, "set.view", "set_view")
		args_called = unique(c(setdiff(args_called, "set.view"), "set_view"))
	}
	if ("set.zoom.limits" %in% args_called) {
		args$set_zoom_limits = args$set.zoom.limits
		args$set.zoom.limits = NULL
		v3_opt(fun, "set.zoom.limits", "set_zoom_limits")
		args_called = unique(c(setdiff(args_called, "set.zoom.limits"), "set_zoom_limits"))
	}
	if ("max.raster" %in% args_called) {
		args$raster.max_cells = args$max.raster
		args$max.raster = NULL
		v3_opt(fun, "max.raster", "raster.max_cells")
		args_called = unique(c(setdiff(args_called, "max.raster"), "raster.max_cells"))
	}
	if ("fontfamily" %in% args_called) {
		args$text.fontfamily = args$fontfamily
		args$fontfamily = NULL
		v3_opt(fun, "fontfamily", "text.fontfamily")
		args_called = unique(c(setdiff(args_called, "fontfamily"), "text.fontfamily"))
	}
	if ("fontface" %in% args_called) {
		args$text.fontface = args$fontface
		args$fontface = NULL
		v3_opt(fun, "fontface", "text.fontface")
		args_called = unique(c(setdiff(args_called, "fontface"), "text.fontface"))
	}
	if ("overlays" %in% args_called) {
		args$tiles.server = args$overlays
		args$overlays = NULL
		v3_opt(fun, "overlays", "tiles.server")
	}
	if ("basemaps" %in% args_called) {
		args$basemap.server = args$basemaps
		args$basemaps = NULL
		v3_opt(fun, "basemaps", "basemap.server")
	}


	if (!is.null(args[["title"]]) || (!is.null(args[["main.title"]]))) {
		title1 = if (!is.null(args$title)) {
			title.args = args[substr(names(args), 1, 5) == "title"]
			title.args$title = NULL
			names(title.args) = substr(names(title.args), 7, nchar(names(title.args)))

			v3_title(fun)

			if (!("position" %in% names(title.args))) title.args$position = tm_pos_in("left", "top")
			title = args$title
			args$title = NULL
			do.call(tm_title, c(list(text = title), title.args))
		} else {
			NULL
		}
		title2 = if (!is.null(args[["main.title"]])) {
			main.title.args = args[substr(names(args), 1, 10) == "main.title"]
			main.title.args$main.title = NULL
			if ("main.title.position" %in% names(main.title.args)) {
				main.title.args$main.title.position = tm_pos_out(main.title.args$main.title.position[1], "top")
			}
			names(main.title.args) = substr(names(main.title.args), 12, nchar(names(main.title.args)))
			v3_main_title(fun)
			main.title = args$main.title
			args$main.title = NULL
			do.call(tm_title, c(list(text = main.title), main.title.args))
		} else {
			NULL
		}
		tmo = tm_element_list(do.call(tm_element, c(args, list(calls = args_called, subclass = "tm_options"))))
		tmo + title1 + title2
	} else {
		tm_element_list(do.call(tm_element, c(args, list(calls = args_called, subclass = "tm_options"))))
	}

}
