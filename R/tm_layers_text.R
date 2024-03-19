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
#' @param shadow Shadow behind the text. Logical or color.
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
#' @param bgcol,bgcol.scale,bgcol.legend,bgcol.chart,bgcol.free Visual variable that determines
#'   the background color. See Details.
#' @param bgcol_alpha,bgcol_alpha.scale,bgcol_alpha.legend,bgcol_alpha.chart,bgcol_alpha.free Visual variable that determines
#'   the background color transparency. See Details.
#' @param xmod,xmod.scale,xmod.legend,xmod.chart,xmod.free Transformation variable that determines the x offset. See details.
#' @param ymod,ymod.scale,ymod.legend,ymod.chart,ymod.free Transformation variable that determines the y offset. See details.
#'   the text. See details.
#' @param angle,angle.scale,angle.legend,angle.chart,angle.free Rotation angle
#' @param points.only should only point geometries of the shape object (defined in [tm_shape()]) be plotted? By default `"ifany"`, which means `TRUE` in case a geometry collection is specified.
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
				   shadow = FALSE,
				   plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
				   zindex = NA,
				   group = NA,
				   group.control = "check",
				   points.only = "ifany",
				   just = "center",
				   along.lines = FALSE,
				   bg.padding = 0.4,
				   clustering = FALSE, 
				   point.label = FALSE,
				   point.label.gap = 0,
				   point.label.method = "SANN",
				   remove.overlap = FALSE,
				   ...) {
	
	#if (FALSE) {
	args = list(...)
	
	trans.args = list(points.only = points.only, along.lines = along.lines)
	mapping.args = list(clustering = clustering, point.label = point.label, remove.overlap = remove.overlap, point.label.gap = point.label.gap, point.label.method = point.label.method, just = just, bg.padding = bg.padding)

	# dput(names(formals("tm_text")))
	v3 = c("root", "clustering", "size.lim", "sizes.legend", 
		   "sizes.legend.labels", "sizes.legend.text", "n", "style", "style.args", 
		   "as.count", "breaks", "interval.closure", "palette", "labels", 
		   "drop.levels", "labels.text", "midpoint", "stretch.palette", 
		   "contrast", "colorNA", "textNA", "showNA", "colorNULL", "fontface", 
		   "fontfamily", "alpha", "case", "bg.alpha", 
		   "size.lowerbound", "print.tiny", "scale", "auto.placement", 
		   "along.lines", "overwrite.lines", "xmod", "ymod", "title.size", 
		   "title.col", "legend.size.show", "legend.col.show", "legend.format", 
		   "legend.size.is.portrait", "legend.col.is.portrait", "legend.size.reverse", 
		   "legend.col.reverse", "legend.hist", "legend.hist.title", "legend.size.z", 
		   "legend.col.z", "legend.hist.z", "id", "auto.palette.mapping", 
		   "max.categories")
	
	
	if (any(v3 %in% names(args))) {
		message("tm_text: Deprecated tmap v3 code detected. Code translated to v4")
		if (!("style" %in% names(args))) {
			if (!"breaks" %in% names(args)) {
				style = "pretty"
			} else {
				style = "fixed"
			}
		} else {
			style = args$style
		}
		
		imp = function(name, value) {
			if (name %in% names(args)) args[[name]] else value
		}
		
		col.scale.args = list(n = imp("n", 5), 
				   style = style, 
				   style.args = imp("style.args", list()), 
				   breaks = imp("breaks", NULL), 
				   interval.closure = imp("interval.closure", "left"), 
				   drop.levels = imp("drop.levels", FALSE),
				   midpoint = imp("midpoint", NULL), 
				   as.count = imp("as.count", NA), 
				   values = imp("palette", NA), 
				   values.repeat = !imp("stretch.palette", TRUE), 
				   values.range = imp("contrast", NA), 
				   values.scale = 1, 
				   value.na = imp("colorNA", NA), 
				   value.null = imp("colorNULL", NA), 
				   value.neutral = NA, 
				   labels = imp("labels", NULL), 
				   label.na = imp("textNA", "Missing"), 
				   label.null = NA, 
				   label.format = imp("legend.format", list()))
		col.scale.args$fun_pref = if (style == "cat") {
			"categorical"
		} else if (style %in% c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "log10_pretty")) {
			"intervals"
		} else if (style == "cont") {
			"continuous"
		} else if (style == "log10") {
			"continuous_log"
		} else {
			stop("unknown style")
		}
		
		col.scale = do.call("tm_scale", args = col.scale.args)		
		
		col.legend.args = list(title = imp("title.col", NA),
								show = imp("legend.col.show", NULL),
								na.show = imp("na.show", NA),
								format = imp("legend.format", list()),
								orientation = ifelse(imp("legend.col.is.portrait", TRUE), "portrait", "landscape"),
								reverse = imp("legend.col.reverse", FALSE))
		
		col.legend = do.call("tm_legend", col.legend.args)
		
		text.scale = tm_scale_asis(value.neutral = imp("sizes.legend.text", NA))
		
		size.scale.args = list(values = tmap_seq(0, 1, power = 1/imp("root", 3)),
							   limits = imp("size.lim", NULL),
							   outliers.trunc = c(imp("print.tiny", FALSE), TRUE),
							   ticks = imp("breaks", NULL),
							   midpoint = imp("midpoint", NULL),
							   labels = imp("sizes.legend.labels", NULL))
		size.scale = do.call("tm_scale_continuous", size.scale.args)
		
		size.legend.args = list(title = imp("title.size", NA),
							   show = imp("legend.size.show", NULL),
							   na.show = imp("na.show", NA),
							   format = imp("legend.format", list()),
							   orientation = ifelse(imp("legend.size.is.portrait", TRUE), "portrait", "landscape"),
							   reverse = imp("legend.size.reverse", FALSE))
							   
		if ("legend.hist" %in% names(args) && args$legend.hist) {
			col.chart = tm_chart_histogram()
			# to do: histogram title
		}				   

	#}
	}
	
	tm_element_list(tm_element(
		layer = "text",
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.args = trans.args,
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
						angle = "__angle",
						shadow = shadow),
		tpar = tmapTpar(),
		plot.order = plot.order,
		mapping.fun = "Text",
		mapping.args = mapping.args,
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
					shadow = FALSE,
					plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
					zindex = NA,
					group = NA,
					group.control = "check",
					points.only = "ifany",
					along.lines = TRUE,
					clustering = FALSE, 
					point.label = TRUE,
					point.label.gap = 0.3,
					remove.overlap = FALSE,
					...) {
	args = c(as.list(environment()), list(...))
	tm = do.call(tm_text, args)
	tm[[1]]$layer = c("labels", "text")
	tm
}

