#' @rdname tm_text
#' @name opt_tm_text
#' @export
opt_tm_text = function(points.only = "ifany",
					   point.per = "feature",
					   shadow = FALSE,
					   shadow.offset.x = 0.1,
					   shadow.offset.y = 0.1,
					   just = "center",
					   along.lines = FALSE,
					   bg.padding = 0.4,
					   clustering = FALSE, 
					   point.label = FALSE,
					   point.label.gap = 0,
					   point.label.method = "SANN",
					   remove.overlap = FALSE) {
	list(trans.args = list(points.only = points.only,
						   point.per = point.per,
						   along.lines = along.lines),
		 mapping.args = list(shadow = shadow,
		 					shadow.offset.x = shadow.offset.x,
		 					shadow.offset.y = shadow.offset.y,
		 					just = just,
		 					along.lines = along.lines,
		 					bg.padding = bg.padding,
		 					clustering = clustering,
		 					point.label = point.label,
		 					point.label.gap = point.label.gap,
		 					point.label.method = point.label.method,
		 					remove.overlap = remove.overlap))
}


#' @rdname tm_text
#' @name opt_tm_labels
#' @export
opt_tm_labels = function(points.only = "ifany",
						 point.per = "feature",
						 shadow = FALSE,
						 shadow.offset.x = 0.1,
						 shadow.offset.y = 0.1,
						 just = "center",
						 along.lines = TRUE,
						 bg.padding = 0.4,
						 clustering = TRUE, 
						 point.label = TRUE,
						 point.label.gap = 0.4,
						 point.label.method = "SANN",
						 remove.overlap = FALSE) {
	list(trans.args = list(points.only = points.only,
						   point.per = point.per,
						   along.lines = along.lines),
		 mapping.args = list(shadow = shadow,
		 					shadow.offset.x = shadow.offset.x,
		 					shadow.offset.y = shadow.offset.y,
		 					just = just,
		 					along.lines = along.lines,
		 					bg.padding = bg.padding,
		 					clustering = clustering,
		 					point.label = point.label,
		 					point.label.gap = point.label.gap,
		 					point.label.method = point.label.method,
		 					remove.overlap = remove.overlap))
}

#' @param markers.on.top.of.text should markers be plot on top of the text (by default `FALSE`)
#' @param dots.icon.scale scaling number that determines how large the icons (or grobs) are in plot mode in comparison to proportional symbols (such as bubbles). In view mode, the size is determined by the icon specification (see \code{\link{tmap_icons}}) or, if grobs are specified by \code{grob.width} and \code{grob.heigth}
#' @param dots.just justification of the text relative to the point coordinates. Either one of the following values: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}, or a vector of two values where first value specifies horizontal and the second value vertical justification. Besides the mentioned values, also numeric values between 0 and 1 can be used. 0 means left justification for the first value and bottom justification for the second value. Note that in view mode, only one value is used.
#' @param dots.grob.dim vector of four values that determine how grob objects (see details) are shown in view mode. The first and second value are the width and height of the displayed icon. The third and fourth value are the width and height of the rendered png image that is used for the icon. Generally, the third and fourth value should be large enough to render a ggplot2 graphic successfully. Only needed for the view mode.
#' @rdname tm_text
#' @name opt_tm_markers
#' @export
opt_tm_markers = function(markers.on.top.of.text = FALSE,
						  points.only = "ifany",
						  point.per = "feature",
						  shadow = FALSE,
						  shadow.offset.x = 0.1,
						  shadow.offset.y = 0.1,
						  just = "center",
						  along.lines = TRUE,
						  bg.padding = 0.4,
						  clustering = TRUE, 
						  point.label = TRUE,
						  point.label.gap = 0.4,
						  point.label.method = "SANN",
						  remove.overlap = FALSE,
						  dots.just = NA,
						  dots.icon.scale = 3,
						  dots.grob.dim = c(width=48, height=48, render.width=256, render.height=256)) {
	list(markers = list(markers.on.top.of.text = markers.on.top.of.text),
		 text = opt_tm_labels(points.only = points.only,
		 					 shadow = shadow,
		 					 shadow.offset.x = shadow.offset.x,
		 					 shadow.offset.y = shadow.offset.y,
		 					 just = just,
		 					 along.lines = along.lines,
		 					 bg.padding = bg.padding,
		 					 clustering = clustering,
		 					 point.label = point.label,
		 					 point.label.gap = point.label.gap,
		 					 point.label.method = point.label.method,
		 					 remove.overlap = remove.overlap),
		 dots = opt_tm_dots(points.only = points.only,
		 				   icon.scale = dots.icon.scale,
		 				   just = dots.just,
		 				   grob.dim = dots.grob.dim))
}




#' Map layer: text
#' 
#' Map layer that draws symbols Supported visual variables are: `text`
#' (the text itself) `col` (color), `size` (font size), and `fontface` (font face).
#' 
#' The visual variable arguments (e.g. `col`) can be specified with either a
#' data variable name (of the object specified in [tm_shape()]), or with a visual
#' value (for `col`, a color is expected). Multiple values can be specified:
#' in that case facets are created. These facets can be combined with other
#' faceting data variables, specified with [tm_facets()].
#' 
#' The `.scale` arguments determine the used scale to map the data values to
#' visual variable values. These can be specified with one of the available 
#' `tm_scale_()` functions. The default scale that is used is specified by the
#' tmap option `scales.var`.
#' 
#' The `.legend` arguments determine the used legend, specified with [tm_legend()].
#' The default legend and its settings are determined by the tmap options `legend.`.
#' 
#' The `.free` arguments determine whether scales are applied freely across facets,
#' or shared. A logical value is required. They can also be specified with a
#' vector of three logical values; these determine whether scales are applied
#' freely per facet dimension. This is only useful when facets are applied 
#' (see [tm_facets()]). There are maximally three facet dimensions: rows, columns,
#' and pages. This only applies for a facet grid ([tm_facets_grid()]).
#' For instance, `col.free = c(TRUE, FALSE, FALSE)` means that for the visual
#' variable `col`, each row of facets will has its own scale, and therefore its
#' own legend. For facet wraps and stacks ([tm_facets_wrap()] and [tm_facets_stack()])
#' there is only one facet dimension, so the `.free` argument requires only one logical value.
#' 
#' @param text,text.scale,text.legend,text.chart,text.free Visual variable that determines
#'   the text. See details.
#' @param size,size.scale,size.legend,size.chart,size.free Visual variable that determines
#'   the font size. See details.
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines
#'   the col color. See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free Visual variable that determines
#'   the border color alpha transparency. See Details.
#' @param fontface,fontface.scale,fontface.legend,fontface.chart,fontface.free Visual variable that determines
#'   the font face. See Details.
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
#' @param zindex Map layers are drawn on top of each other. The `zindex` numbers
#'   (one for each map layer) determines the stacking order.
#'   By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only
#'   relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer
#'   groups can be switched on and off. Options: `"radio"` for radio buttons
#'   (meaning only one group can be shown), `"check"` for check boxes (so multiple groups can be shown),
#'   and `"none"` for no control (the group cannot be (de)selected).
#' @param options options passed on to the corresponding `opt_<layer_function>` function 
#' @param points.only should only point geometries of the shape object (defined in [tm_shape()]) be plotted? By default `"ifany"`, which means `TRUE` in case a geometry collection is specified.
#' @param point.per specification of how spatial points are mapped when the geometry is a multi line or a multi polygon. One of \code{"feature"}, \code{"segment"} or \code{"largest"}. The first generates a spatial point for every feature, the second for every segment (i.e. subfeature), the third only for the largest segment (subfeature). Note that the last two options can be significant slower.
#' @param shadow Shadow behind the text. Logical or color.
#' @param shadow.offset.x,shadow.offset.y Shadow offset in line heights
#' @param just justification of the text relative to the point coordinates. Either one of the following values: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}, or a vector of two values where first value specifies horizontal and the second value vertical justification. Besides the mentioned values, also numeric values between 0 and 1 can be used. 0 means left justification for the first value and bottom justification for the second value. Note that in view mode, only one value is used.
#' @param along.lines logical that determines whether labels are rotated along the spatial lines. Only applicable if a spatial lines shape is used.
#' @param bg.padding The padding of the background in terms of line heights.
#' @param clustering value that determines whether the text labels are clustered in \code{"view"} mode. One of: \code{TRUE}, \code{FALSE}, or the output of \code{\link[leaflet:markerClusterOptions]{markerClusterOptions}}.
#' @param point.label logical that determines whether the labels are placed automatically.
#' @param point.label.gap numeric that determines the gap between the point and label
#' @param point.label.method the optimization method, either `"SANN"` for simulated annealing (the default) or `"GA"` for a genetic algorithm.
#' @param remove.overlap logical that determines whether the overlapping labels are removed
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_text.R 
#' @rdname tm_text
#' @name tm_text
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
				   plot.order = tm_plot_order("size", reverse = FALSE),
				   zindex = NA,
				   group = NA,
				   group.control = "check",
				   options = opt_tm_text(),
				   ...) {
	
	#if (FALSE) {
	args = list(...)
	

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
			v3_tm_scale_instead_of_style(style, scale_fun = col.scale.args$fun_pref, vv = "col", layer_fun = "tm_text", arg_list = v3_list_get())
		} else {
			v3_tm_scale(scale_fun = "", vv = "col", layer_fun = "tm_text", arg_list = v3_list_get())
		}
		
		col.scale = do.call("tm_scale", args = col.scale.args)		
		
		v3_list_init()
		col.legend.args = alist(title = v3_impute(args, "title.col", NA, "title"),
								show = v3_impute(args, "legend.col.show", NULL, "show"),
								na.show = v3_impute(args, "showNA", NA, "na.show"),
								format = v3_impute(args, "legend.format", list(), "format"),
								orientation = ifelse(v3_impute(args, "legend.col.is.portrait", TRUE, "orientation"), "portrait", "landscape"),
								reverse = v3_impute(args, "legend.col.reverse", FALSE, "reverse"))

		v3_tm_legend(fun = "tm_text", vv = "col", arg_list = v3_list_get())
		col.legend = do.call("tm_legend", col.legend.args)
		
		v3_list_init()
		text.scale = tm_scale_asis(value.neutral = v3_impute(args, "sizes.legend.text", NA, "value.neutral"))
		v3_tm_scale(scale_fun = "asis", vv = "text", layer_fun = "tm_text", arg_list = v3_list_get())
		
		if ("legend.hist" %in% names(args) && args$legend.hist) {
			col.chart = tm_chart_histogram()
			v3_tm_chart_hist(layer_fun = "tm_text", vv = "col", arg = "legend.hist")
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
		v3_tm_scale(scale_fun = "continuous", vv = "size", layer_fun = "tm_text", arg_list = v3_list_get())
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
		
			   

		v4_opt_args = c("along.lines", "shadow", "just", "clustering", "point.label", "remove.overlap")
		v3_opt_args = c("along.lines", "shadow", "just", "clustering", "auto.placement", "remove.overlap")
		osel = which(v3_opt_args %in% names(args))
		if (length(osel)) {
			o3 = v3_opt_args[osel]
			o4 = v4_opt_args[osel]
			v3_opt(o3, o4, "tm_text")
		}
			
		
	#}
	}
	
	tm_element_list(tm_element(
		layer = "text",
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.args = options$trans.args,
		trans.isglobal = FALSE,
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
		mapping.fun = "Text",
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
#' @name tm_labels
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
#' @name tm_labels_highlighted
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


#' @rdname tm_text
#' @name tm_markers
#' @export
tm_markers = function(text = tm_const(),
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
					 options = opt_tm_markers(),
					 ...) {
	e = as.list(environment())
	e$options = options$text
	a = list(...)
	
	sel = substr(names(a), 1, 5) == "dots_"
	a_dots = a[sel]
	a_other = a[!sel]
	names(a_dots) = substr(names(a_dots), 6, nchar(names(a_dots)))
	
	args_dots = c(a_dots, list(plot.order = plot.order, zindex = zindex, group = group, group.control = group.control, options = options$dots))
	args_text = c(e, a_other)
	
	tm_t = do.call(tm_text, args_text)
	tm_t[[1]]$layer = c("labels", "text")
	tm_d = do.call(tm_dots, args_dots)
	tm_d[[1]]$layer = c("markers", "symbols")
	
	if (options$markers$markers.on.top.of.text) tm_t + tm_d else tm_d + tm_t
}
