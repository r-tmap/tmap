#' Map layer: text
#'
#' Map layer that draws symbols Supported visual variables are: `text`
#' (the text itself) `col` (color), `size` (font size), and `fontface` (font face).
#'
#' @param text,text.scale,text.legend,text.chart,text.free `r .doc_vv("text")`
#' @param size,size.scale,size.legend,size.chart,size.free `r .doc_vv("size")`
#' @param col,col.scale,col.legend,col.chart,col.free `r .doc_vv("col")`
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free `r .doc_vv("col_alpha")`
#' @param fontface,fontface.scale,fontface.legend,fontface.chart,fontface.free `r .doc_vv("fontface")`
#' @param fontfamily The font family. See [gpar()][grid::gpar()] for details.
#' @param bgcol,bgcol.scale,bgcol.legend,bgcol.chart,bgcol.free Visual variable that determines
#'   the background color. See Details.
#' @param bgcol_alpha,bgcol_alpha.scale,bgcol_alpha.legend,bgcol_alpha.chart,bgcol_alpha.free Visual variable that determines
#'   the background color transparency. See Details.
#' @param xmod,xmod.scale,xmod.legend,xmod.chart,xmod.free Transformation variable that determines the x offset. See details.
#' @param ymod,ymod.scale,ymod.legend,ymod.chart,ymod.free Transformation variable that determines the y offset. See details.
#'   the text. See details.
#' @param angle,angle.scale,angle.legend,angle.chart,angle.free Rotation angle
#' @param plot.order Specification in which order the spatial features are drawn.
#'   See [tm_plot_order()] for details.
#' @inherit tm_polygons details
#' @inheritParams tm_polygons
#' @seealso \href{https://r-tmap.github.io/tmap/articles/examples_terrain}{Terrain map example}
#' @param options options passed on to the corresponding `opt_<layer_function>` function
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_text.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/examples_topo_Africa}{Topographic map}
#' @export
tm_text = function(text = tm_const(),
				   text.scale = tm_scale(),
				   text.legend = tm_legend(),
				   text.chart = tm_chart_none(),
				   text.free = NA,
				   size = tm_const(),
				   size.scale = tm_scale(),
				   size.legend = tm_legend(),
				   size.chart = tm_chart_none(),
				   size.free = NA,
				   col = tm_const(),
				   col.scale = tm_scale(),
				   col.legend = tm_legend(),
				   col.chart = tm_chart_none(),
				   col.free = NA,
				   col_alpha = tm_const(),
				   col_alpha.scale = tm_scale(),
				   col_alpha.legend = tm_legend(),
				   col_alpha.chart = tm_chart_none(),
				   col_alpha.free = NA,
				   fontface = tm_const(),
				   fontface.scale = tm_scale(),
				   fontface.legend = tm_legend(),
				   fontface.chart = tm_chart_none(),
				   fontface.free = NA,
				   fontfamily = NA,
				   bgcol = tm_const(),
				   bgcol.scale = tm_scale(),
				   bgcol.legend = tm_legend(),
				   bgcol.chart = tm_chart_none(),
				   bgcol.free = NA,
				   bgcol_alpha = tm_const(),
				   bgcol_alpha.scale = tm_scale(),
				   bgcol_alpha.legend = tm_legend(),
				   bgcol_alpha.chart = tm_chart_none(),
				   bgcol_alpha.free = NA,
				   xmod = 0,
				   xmod.scale = tm_scale(),
				   xmod.legend = tm_legend_hide(),
				   xmod.chart = tm_chart_none(),
				   xmod.free = NA,
				   ymod = 0,
				   ymod.scale = tm_scale(),
				   ymod.legend = tm_legend_hide(),
				   ymod.chart = tm_chart_none(),
				   ymod.free = NA,
				   angle = 0,
				   angle.scale = tm_scale(),
				   angle.legend = tm_legend_hide(),
				   angle.chart = tm_chart_none(),
				   angle.free = NA,
				   plot.order = tm_plot_order("size", reverse = FALSE),
				   zindex = NA,
				   group = NA,
				   group.control = "check",
				   options = opt_tm_text(),
				   ...) {

	#if (FALSE) {
	args = list(...)

	layer_fun = if ("called_from" %in% names(args)) {
		args$called_from
	} else {
		"tm_text"
	}

	if (any(v3_only("tm_text") %in% names(args))) {
		v3_start_message()
		if (!("style" %in% names(args))) {
			if (!"breaks" %in% names(args)) {
				style = "pretty"
			} else {
				style = "fixed"
			}
		} else {
			style = args$style
		}

		v3_list_init()
		if (length(style) > 1) {
			style = style[1]
			.TMAP$v3_list$mult = TRUE
		}
		col.scale.args = c(list(n = v3_impute(args, "n", 5),
								style = style,
								style.args = v3_impute(args, "style.args", list())),
							if (style %in% c("cont", "log10")) {
								c({
									if (!is.null(args$breaks)) {
										if (length(args$breaks) > 2) {
											list(ticks = v3_impute(args, "breaks", NULL, "ticks"))
										} else {
											list(ticks = v3_impute(args, "breaks", NULL, "limits"))
										}
									} else {
										list()
									}
								},
								list(outliers.trunc = c(TRUE, FALSE)))
							} else {
								list(breaks = v3_impute(args, "breaks", NULL),
									 interval.closure = v3_impute(args, "interval.closure", "left"),
									 drop.levels = v3_impute(args, "drop.levels", FALSE))
							},
							list(midpoint = v3_impute(args, "midpoint", NULL),
								 as.count = v3_impute(args, "as.count", NA),
								 values = v3_impute(args, "palette", NA, "values"),
								 values.repeat = !v3_impute(args, "stretch.palette", TRUE, "values.repeat"),
								 values.range = v3_impute(args, "contrast", NA, "values.range"),
								 values.scale = 1,
								 value.na = v3_impute(args, "colorNA", NA, "value.na"),
								 value.null = v3_impute(args, "colorNULL", NA, "value.null"),
								 value.neutral = NA,
								 labels = v3_impute(args, "labels", NULL),
								 label.na = v3_impute(args, "textNA", "Missing", "label.na"),
								 label.null = NA,
								 label.format = v3_impute(args, "legend.format", list(), "label.format")))

		col.scale.args$fun_pref = if (style == "cat") {
			"categorical"
		} else if (style %in% c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "log10_pretty")) {
			"intervals"
		} else if (style == "cont") {
			"continuous"
		} else if (style == "log10") {
			"continuous_log10"
		} else {
			stop("unknown style")
		}

		if ("style" %in% names(args)) {
			v3_tm_scale_instead_of_style(style, scale_fun = col.scale.args$fun_pref, vv = "col", layer_fun = layer_fun, arg_list = v3_list_get())
		} else {
			v3_tm_scale(scale_fun = "", vv = "col", layer_fun = layer_fun, arg_list = v3_list_get())
		}

		col.scale = do.call("tm_scale", args = col.scale.args)

		v3_list_init()
		col.legend.args = alist(title = v3_impute(args, "title.col", NA, "title"),
								show = v3_impute(args, "legend.col.show", NULL, "show"),
								na.show = v3_impute(args, "showNA", NA, "na.show"),
								format = v3_impute(args, "legend.format", list(), "format"),
								orientation = ifelse(v3_impute(args, "legend.col.is.portrait", TRUE, "orientation"), "portrait", "landscape"),
								reverse = v3_impute(args, "legend.col.reverse", FALSE, "reverse"))

		v3_tm_legend(fun = layer_fun, vv = "col", arg_list = v3_list_get())
		col.legend = do.call("tm_legend", col.legend.args)

		v3_list_init()
		text.scale = tm_scale_asis(value.neutral = v3_impute(args, "sizes.legend.text", NA, "value.neutral"))
		v3_tm_scale(scale_fun = "asis", vv = "text", layer_fun = layer_fun, arg_list = v3_list_get())

		if ("legend.hist" %in% names(args) && args$legend.hist) {
			col.chart = tm_chart_histogram()
			v3_tm_chart_hist(layer_fun = layer_fun, vv = "col", arg = "legend.hist")
			# to do: histogram title
		}

		v3_list_init()
		size.scale.args = list(values = tm_seq(v3_impute(args, "size.lowerbound", 0, "values = tm_seq(<size.lowerbound>, 1)"), 1, power = 1/v3_impute(args, "root", 3, "values = tm_seq(<size.lowerbound>, 1, power = 1/<root>)")),
							   values.scale = v3_impute(args, "scale", 1, "values.scale"),
							   limits = v3_impute(args, "size.lim", NULL, "limits"),
							   outliers.trunc = c(v3_impute(args, "print.tiny", FALSE, paste0("outliers.trunc = c(", {if ("print.tiny" %in% names(args)) !args$print.tiny else TRUE}, ", TRUE)") ), TRUE),
							   ticks = v3_impute(args, "breaks", NULL, "ticks"),
							   midpoint = v3_impute(args, "midpoint", NULL),
							   labels = v3_impute(args, "sizes.legend.labels", NULL, "labels"))
		v3_tm_scale(scale_fun = "continuous", vv = "size", layer_fun = layer_fun, arg_list = v3_list_get())
		size.scale = do.call("tm_scale_continuous", size.scale.args)


		v3_list_init()
		size.legend.args = alist(title = v3_impute(args, "title.size", NA, "title"),
							   show = v3_impute(args, "legend.size.show", NULL, "show"),
							   na.show = v3_impute(args, "showNA", NA, "na.show"),
							   format = v3_impute(args, "legend.format", list(), "format"),
							   orientation = ifelse(v3_impute(args, "legend.size.is.portrait", TRUE, "orientation"), "portrait", "landscape"),
							   reverse = v3_impute(args, "legend.size.reverse", FALSE, "reverse"))
		v3_tm_legend(fun = "tm_text", vv = "size", arg_list = v3_list_get())
		size.legend = do.call("tm_legend", size.legend.args)



		v4_opt_args = c("along_lines", "shadow", "just", "clustering", "point.label", "remove_overlap")
		v3_opt_args = c("along_lines", "shadow", "just", "clustering", "auto.placement", "remove_overlap")
		osel = which(v3_opt_args %in% names(args))
		if (length(osel)) {
			o3 = v3_opt_args[osel]
			o4 = v4_opt_args[osel]
			v3_layer_opt(o3, o4, layer_fun)
		}


	#}
	}

	# unused arguments: typos?
	unused = setdiff(names(args), c(v3_only("tm_text"), "called_from"))

	if (length(unused)) {
		message_layer_unused_args(layer_fun, unused)
	}

	tm_element_list(tm_element(
		layer = "text",
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.args = options$trans.args,
		trans.apply_to = "this",
		mapping.aes = list(
						   text = tmapScale(aes = "text",
						   				value = text,
						   				scale = text.scale,
						   				legend = text.legend,
						   				chart = text.chart,
						   				free = text.free),
						   size = tmapScale(aes = "size",
						   				value = size,
						   				scale = size.scale,
						   				legend = size.legend,
						   				chart = size.chart,
						   				free = size.free),
						   col = tmapScale(aes = "col",
						   				value = col,
						   				scale = col.scale,
						   				legend = col.legend,
						   				chart = col.chart,
						   				free = col.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
						   					  chart = col_alpha.chart,
						   					  free = col_alpha.free),
						   bgcol = tmapScale(aes = "bgcol",
						   				value = bgcol,
						   				scale = bgcol.scale,
						   				legend = bgcol.legend,
						   				chart = bgcol.chart,
						   				free = bgcol.free),
						   bgcol_alpha = tmapScale(aes = "bgcol_alpha",
						   					  value = bgcol_alpha,
						   					  scale = bgcol_alpha.scale,
						   					  legend = bgcol_alpha.legend,
						   					  chart = bgcol_alpha.chart,
						   					  free = bgcol_alpha.free),
						   angle = tmapScale(aes = "angle",
						   						value = angle,
						   						scale = angle.scale,
						   						legend = angle.legend,
						   						chart = angle.chart,
						   						free = angle.free),
						   fontface = tmapScale(aes = "fontface",
						   				value = fontface,
						   				scale = fontface.scale,
						   				legend = fontface.legend,
						   				chart = fontface.chart,
						   				free = fontface.free),
						   xmod = tmapScale(aes = "xmod",
						   				 value = xmod,
						   				 scale = xmod.scale,
						   				 legend = xmod.legend,
						   				 chart = xmod.chart,
						   				 free = xmod.free),
						   ymod = tmapScale(aes = "ymod",
						   				 value = ymod,
						   				 scale = ymod.scale,
						   				 legend = ymod.legend,
						   				 chart = ymod.chart,
						   				 free = ymod.free)),

		gpar = tmapGpar(fill = NA,
						col = "__col",
						shape = NA,
						size = NA,
						cex = "__size",
						fill_alpha = NA,
						col_alpha = "__col_alpha",
						pattern = NA,
						text = "__text",
						fontface = "__fontface",
						fontfamily = fontfamily,
						lty = NA,
						lwd = NA,
						linejoin = NA,
						lineend = NA,
						bgcol = "__bgcol",
						bgcol_alpha = "__bgcol_alpha",
						xmod = "__xmod",
						ymod = "__ymod",
						angle = "__angle"),
		tpar = tmapTpar(),
		plot.order = plot.order,
		mapping.args = options$mapping.args,
		zindex = zindex,
		group = group,
		group.control = group.control,
		popup.vars = FALSE,
		popup.format = list(),
		hover = "",
		id = "",
		subclass = c("tm_aes_layer", "tm_layer")))
}


#' @rdname tm_text
#' @export
tm_labels = function(text = tm_const(),
					text.scale = tm_scale(),
					text.legend = tm_legend(),
					text.chart = tm_chart_none(),
					text.free = NA,
					size = tm_const(),
					size.scale = tm_scale(),
					size.legend = tm_legend(),
					size.chart = tm_chart_none(),
					size.free = NA,
					col = tm_const(),
					col.scale = tm_scale(),
					col.legend = tm_legend(),
					col.chart = tm_chart_none(),
					col.free = NA,
					col_alpha = tm_const(),
					col_alpha.scale = tm_scale(),
					col_alpha.legend = tm_legend(),
					col_alpha.chart = tm_chart_none(),
					col_alpha.free = NA,
					fontface = tm_const(),
					fontface.scale = tm_scale(),
					fontface.legend = tm_legend(),
					fontface.chart = tm_chart_none(),
					fontface.free = NA,
					fontfamily = "",
					bgcol = tm_const(),
					bgcol.scale = tm_scale(),
					bgcol.legend = tm_legend(),
					bgcol.chart = tm_chart_none(),
					bgcol.free = NA,
					bgcol_alpha = tm_const(),
					bgcol_alpha.scale = tm_scale(),
					bgcol_alpha.legend = tm_legend(),
					bgcol_alpha.chart = tm_chart_none(),
					bgcol_alpha.free = NA,
					xmod = 0,
					xmod.scale = tm_scale(),
					xmod.legend = tm_legend_hide(),
					xmod.chart = tm_chart_none(),
					xmod.free = NA,
					ymod = 0,
					ymod.scale = tm_scale(),
					ymod.legend = tm_legend_hide(),
					ymod.chart = tm_chart_none(),
					ymod.free = NA,
					angle = 0,
					angle.scale = tm_scale(),
					angle.legend = tm_legend_hide(),
					angle.chart = tm_chart_none(),
					angle.free = NA,
					plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
					zindex = NA,
					group = NA,
					group.control = "check",
					options = opt_tm_labels(),
					...) {
	args = c(as.list(environment()), list(...))
	tm = do.call(tm_text, args)
	tm[[1]]$layer = c("labels", "text")
	tm
}



#' @rdname tm_text
#' @export
tm_labels_highlighted = function(text = tm_const(),
								 text.scale = tm_scale(),
								 text.legend = tm_legend(),
								 text.chart = tm_chart_none(),
								 text.free = NA,
								 size = tm_const(),
								 size.scale = tm_scale(),
								 size.legend = tm_legend(),
								 size.chart = tm_chart_none(),
								 size.free = NA,
								 col = tm_const(),
								 col.scale = tm_scale(),
								 col.legend = tm_legend(),
								 col.chart = tm_chart_none(),
								 col.free = NA,
								 col_alpha = tm_const(),
								 col_alpha.scale = tm_scale(),
								 col_alpha.legend = tm_legend(),
								 col_alpha.chart = tm_chart_none(),
								 col_alpha.free = NA,
								 fontface = tm_const(),
								 fontface.scale = tm_scale(),
								 fontface.legend = tm_legend(),
								 fontface.chart = tm_chart_none(),
								 fontface.free = NA,
								 fontfamily = "",
								 bgcol = tm_const(),
								 bgcol.scale = tm_scale(),
								 bgcol.legend = tm_legend(),
								 bgcol.chart = tm_chart_none(),
								 bgcol.free = NA,
								 bgcol_alpha = tm_const(),
								 bgcol_alpha.scale = tm_scale(),
								 bgcol_alpha.legend = tm_legend(),
								 bgcol_alpha.chart = tm_chart_none(),
								 bgcol_alpha.free = NA,
								 xmod = 0,
								 xmod.scale = tm_scale(),
								 xmod.legend = tm_legend_hide(),
								 xmod.chart = tm_chart_none(),
								 xmod.free = NA,
								 ymod = 0,
								 ymod.scale = tm_scale(),
								 ymod.legend = tm_legend_hide(),
								 ymod.chart = tm_chart_none(),
								 ymod.free = NA,
								 angle = 0,
								 angle.scale = tm_scale(),
								 angle.legend = tm_legend_hide(),
								 angle.chart = tm_chart_none(),
								 angle.free = NA,
								 plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
								 zindex = NA,
								 group = NA,
								 group.control = "check",
								 options = opt_tm_labels(),
								 ...) {
	args = c(as.list(environment()), list(...))
	tm = do.call(tm_text, args)
	tm[[1]]$layer = c("labels_highlighted", "labels", "text")
	tm
}

#' @param points_only should only point geometries of the shape object (defined in [tm_shape()]) be plotted? By default `"ifany"`, which means `TRUE` in case a geometry collection is specified.
#' @param point_per specification of how spatial points are mapped when the geometry is a multi line or a multi polygon. One of \code{"feature"}, \code{"segment"} or \code{"largest"}. The first generates a spatial point for every feature, the second for every segment (i.e. subfeature), the third only for the largest segment (subfeature). Note that the last two options can be significant slower.
#' @param on_surface In case of polygons, centroids are computed. Should the points be on the surface? If `TRUE`, which is slower than the default `FALSE`, centroids outside the surface are replaced with points computed with [sf::st_point_on_surface()].
#' @param shadow Shadow behind the text. Logical or color.
#' @param shadow.offset.x,shadow.offset.y Shadow offset in line heights
#' @param just justification of the text relative to the point coordinates. Either one of the following values: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}, or a vector of two values where first value specifies horizontal and the second value vertical justification. Besides the mentioned values, also numeric values between 0 and 1 can be used. 0 means left justification for the first value and bottom justification for the second value. Note that in view mode, only one value is used.
#' @param along_lines logical that determines whether labels are rotated along the spatial lines. Only applicable if a spatial lines shape is used.
#' @param bg.padding The padding of the background in terms of line heights.
#' @param clustering value that determines whether the text labels are clustered in \code{"view"} mode. One of: \code{TRUE}, \code{FALSE}, or the output of \code{\link[leaflet:markerClusterOptions]{markerClusterOptions}}.
#' @param point.label logical that determines whether the labels are placed automatically. By default `FALSE` for `tm_text`, and `TRUE` for `tm_labels` if the number of labels is less than 500 (otherwise it will be too slow).
#' @param point.label.gap numeric that determines the gap between the point and label
#' @param point.label.method the optimization method, either `"SANN"` for simulated annealing (the default) or `"GA"` for a genetic algorithm.
#' @param remove_overlap logical that determines whether the overlapping labels are removed
#' @rdname tm_text
#' @export
opt_tm_text = function(points_only = "ifany",
					   point_per = "feature",
					   on_surface = FALSE,
					   shadow = FALSE,
					   shadow.offset.x = 0.1,
					   shadow.offset.y = 0.1,
					   just = "center",
					   along_lines = FALSE,
					   bg.padding = 0.4,
					   clustering = FALSE,
					   point.label = FALSE,
					   point.label.gap = 0,
					   point.label.method = "SANN",
					   remove_overlap = FALSE) {
	list(trans.args = list(points_only = points_only,
						   point_per = point_per,
						   on_surface = on_surface,
						   along_lines = along_lines),
		 mapping.args = list(shadow = shadow,
		 					shadow.offset.x = shadow.offset.x,
		 					shadow.offset.y = shadow.offset.y,
		 					just = just,
		 					along_lines = along_lines,
		 					bg.padding = bg.padding,
		 					clustering = clustering,
		 					point.label = point.label,
		 					point.label.gap = point.label.gap,
		 					point.label.method = point.label.method,
		 					remove_overlap = remove_overlap))
}

#' @rdname tm_text
#' @export
opt_tm_labels = function(points_only = "ifany",
						 point_per = "feature",
						 on_surface = FALSE,
						 shadow = FALSE,
						 shadow.offset.x = 0.1,
						 shadow.offset.y = 0.1,
						 just = "center",
						 along_lines = TRUE,
						 bg.padding = 0.4,
						 clustering = FALSE,
						 point.label = NA,
						 point.label.gap = 0.4,
						 point.label.method = "SANN",
						 remove_overlap = FALSE) {
	list(trans.args = list(points_only = points_only,
						   point_per = point_per,
						   on_surface = on_surface,
						   along_lines = along_lines),
		 mapping.args = list(shadow = shadow,
		 					shadow.offset.x = shadow.offset.x,
		 					shadow.offset.y = shadow.offset.y,
		 					just = just,
		 					along_lines = along_lines,
		 					bg.padding = bg.padding,
		 					clustering = clustering,
		 					point.label = point.label,
		 					point.label.gap = point.label.gap,
		 					point.label.method = point.label.method,
		 					remove_overlap = remove_overlap))
}
