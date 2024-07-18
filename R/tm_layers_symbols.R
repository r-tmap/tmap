#' @rdname tm_symbols
#' @name opt_tm_symbols
#' @export
opt_tm_symbols = function(points.only = "ifany",
						  point.per = "feature",
						  icon.scale = 3,
						  just = NA,
						  grob.dim = c(width=48, height=48, render.width=256, render.height=256)) {
	list(trans.args = list(points.only = points.only, point.per = point.per, along.lines = FALSE),
		 mapping.args = list(icon.scale = icon.scale,
		 					just = just,
		 					grob.dim = grob.dim))
}

#' @rdname tm_symbols
#' @name opt_tm_dots
#' @export
opt_tm_dots = opt_tm_symbols

#' @rdname tm_symbols
#' @name opt_tm_bubbles
#' @export
opt_tm_bubbles = opt_tm_symbols

#' @rdname tm_symbols
#' @name opt_tm_squares
#' @export
opt_tm_squares = opt_tm_symbols

#' Map layer: symbols
#' 
#' Map layer that draws symbols Supported visual variables are:
#' `fill` (the fill color), `col` (the border color), `size` the symbol size,
#' `shape` the symbol shape, `lwd` (line width), `lty` (line type), `fill_alpha`
#' (fill color alpha transparency) and `col_alpha` (border color alpha transparency).
#' 
#' The visual variable arguments (e.g. `col`) can be specified with either a data
#' variable name (e.g., a spatial vector attribute or a raster layer of the object
#' specified in [tm_shape()]), or with a visual value (for `col`, a color is expected).
#' Multiple values can be specified: in that case facets are created.
#' These facets can be combined with other faceting data variables, specified with [tm_facets()].
#' 
#' * The `*.scale` arguments determine the used scale to map the data values to
#' visual variable values. These can be specified with one of the available
#' `tm_scale_*()` functions. The default is specified by the tmap option ([tm_options()]) `scales.var`.
#' 
#' * The `*.legend` arguments determine the used legend, specified with [tm_legend()].
#' The default legend and its settings are determined by the tmap options ([tm_options()]) `legend.` .
#' 
#' * The `*.chart` arguments specify additional charts, specified with `tm_chart_`, e.g. [tm_chart_histogram()]
#' 
#' * The `*.free` arguments determine whether scales are applied freely across facets, or shared.
#' A logical value is required. They can also be specified with a vector of three
#' logical values; these determine whether scales are applied freely per facet dimension.
#' This is only useful when facets are applied (see [tm_facets()]).
#' There are maximally three facet dimensions: rows, columns, and pages. This only
#' applies for a facet grid ([tm_facets_grid()]). For instance, `col.free = c(TRUE, FALSE, FALSE)`
#' means that for the visual variable `col`, each row of facets will have its own
#' scale, and therefore its own legend. For facet wraps and stacks
#' ([tm_facets_wrap()] and [tm_facets_stack()]) there is only one facet dimension,
#' so the `*.free` argument requires only one logical value.
#'
#' A symbol shape specification is one of the following three options.
#' \enumerate{
#'  \item{A numeric value that specifies the plotting character of the symbol. See parameter \code{pch} of \code{\link[graphics:points]{points}} and the last example to create a plot with all options. Note that this is not supported for the \code{"view" mode.}}
#'  \item{A \code{\link[grid:grid.grob]{grob}} object, which can be a ggplot2 plot object created with \code{\link[ggplot2:ggplotGrob]{ggplotGrob}}. To specify multiple shapes, a list of grob objects is required. See example of a proportional symbol map with ggplot2 plots}.
#'  \item{An icon specification, which can be created with \code{\link{tmap_icons}}.}
#'  }
#'  To specify multiple shapes (needed for the \code{shapes} argument), a vector or list of these shape specification is required. The shape specification options can also be mixed. For the \code{shapes} argument, it is possible to use a named vector or list, where the names correspond to the value of the variable specified by the \code{shape} argument.
#'  For small multiples, a list of these shape specification(s) should be provided.
#'
#' @param fill,fill.scale,fill.legend,fill.chart,fill.free Visual variable that determines the fill color. See details.
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines the col color. See details.
#' @param size,size.scale,size.legend,size.chart,size.free Visual variable that determines the size. See details.
#' @param shape,shape.scale,shape.legend,shape.chart,shape.free Visual variable that determines the shape. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.chart,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.chart,lty.free Visual variable that determines the line type. See details.
#' @param fill_alpha,fill_alpha.scale,fill_alpha.legend,fill_alpha.chart,fill_alpha.free Visual variable that determines
#'   the fill color alpha transparency See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free Visual variable that determines
#'   the border color alpha transparency. See details.
#' @param plot.order Specification in which order the spatial features are drawn.
#'   See [tm_plot_order()] for details.
#' @param zindex Map layers are drawn on top of each other. The `zindex` numbers
#'   (one for each map layer) determines the stacking order. By default the map
#'   layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs.
#'   This is only relevant in view mode, where layer groups can be switched
#'   (see `group.control`)
#' @param group.control In view mode, the group control determines how layer
#'   groups can be switched on and off. Options: `"radio"` for radio buttons
#'   (meaning only one group can be shown), `"check"` for check boxes
#'   (so multiple groups can be shown), and `"none"` for no control (the group cannot be (de)selected).
#' @param options options passed on to the corresponding `opt_<layer_function>` function 
#' @param popup.vars names of data variables that are shown in the popups in
#'   `"view"` mode. Set popup.vars to `TRUE` to show all variables in the shape object.
#'   Set popup.vars to `FALSE` to disable popups. Set popup.vars to a character vector
#'   of variable names to those those variables in the popups. The default (`NA`)
#'   depends on whether visual variables (e.g.`col`) are used. If so, only those are shown.
#'   If not all variables in the shape object are shown.
#' @param popup.format list of formatting options for the popup values.
#'   See the argument `legend.format` for options. Only applicable for numeric data
#'   variables. If one list of formatting options is provided, it is applied to
#'   all numeric variables of `popup.vars`. Also, a (named) list of lists can be provided.
#'   In that case, each list of formatting options is applied to the named variable.
#' @param hover name of the data variable that specifies the hover labels (view mode only). Set to `FALSE` to disable hover labels. By default `FALSE`, unless `id` is specified. In that case, it is set to `id`,
#' @param id name of the data variable that specifies the indices of the spatial features.
#'   Only used for `"view"` mode.
#' @param points.only should only point geometries of the shape object (defined in [tm_shape()]) be plotted? By default `"ifany"`, which means `TRUE` in case a geometry collection is specified. 
#' @param point.per specification of how spatial points are mapped when the geometry is a multi line or a multi polygon. One of \code{"feature"}, \code{"segment"} or \code{"largest"}. The first generates a spatial point for every feature, the second for every segment (i.e. subfeature), the third only for the largest segment (subfeature). Note that the last two options can be significant slower.
#' @param icon.scale scaling number that determines how large the icons (or grobs) are in plot mode in comparison to proportional symbols (such as bubbles). For view mode, use the argument `grob.dim`
#' @param just justification of the text relative to the point coordinates. Either one of the following values: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}, or a vector of two values where first value specifies horizontal and the second value vertical justification. Besides the mentioned values, also numeric values between 0 and 1 can be used. 0 means left justification for the first value and bottom justification for the second value. Note that in view mode, only one value is used.
#' @param grob.dim vector of four values that determine how grob objects (see details) are shown in view mode. The first and second value are the width and height of the displayed icon. The third and fourth value are the width and height of the rendered png image that is used for the icon. Generally, the third and fourth value should be large enough to render a ggplot2 graphic successfully. Only needed for the view mode.
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_symbols.R 
#' @export
#' @name tm_symbols
#' @rdname tm_symbols
tm_symbols = function(size = tm_const(),
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
					  shape = tm_const(),
					  shape.scale = tm_scale(),
					  shape.legend = tm_legend(),
					  shape.chart = tm_chart_none(),
					  shape.free = NA,
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
					  popup.vars = NA,
					  popup.format = list(),
					  hover = NA,
					  id = "",
					  options = opt_tm_symbols(),
					  ...) {
	
	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	

	res = v3_symbols(args, args_called)
	if (!is.null(res)) {
		fill = res$fill
		col = res$col
		col_alpha = res$col_alpha
		fill_alpha = res$fill_alpha
		fill.scale = res$fill.scale
		fill.legend = res$fill.legend
		size.scale = res$size.scale
		size.legend = res$size.legend
		shape.scale = res$shape.scale
		shape.legend = res$shape.legend
		fill.chart = res$fill.chart
	}

	tm_element_list(tm_element(
		layer = "symbols",
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.args = options$trans.args,
		trans.isglobal = FALSE,
		mapping.aes = list(size = tmapScale(aes = "size",
											value = size,
											scale = size.scale,
											legend = size.legend,
											chart = size.chart,
											free = size.free),
						   
						   fill = tmapScale(aes = "fill",
						   				 value = fill,
						   				 scale = fill.scale,
						   				 legend = fill.legend,
						   				 chart = fill.chart,
						   				 free = fill.free),
						   col = tmapScale(aes = "col",
						   				value = col,
						   				scale = col.scale,
						   				legend = col.legend,
						   				chart = col.chart,
						   				free = col.free),
						   shape = tmapScale(aes = "shape",
						   				  value = shape,
						   				  scale = shape.scale,
						   				  legend = shape.legend,
						   				  chart = shape.chart,
						   				  free = shape.free),
						   lwd = tmapScale(aes = "lwd",
						   				value = lwd,
						   				scale = lwd.scale,
						   				legend = lwd.legend,
						   				chart = lwd.chart,
						   				free = lwd.free),
						   lty = tmapScale(aes = "lty",
						   				value = lty,
						   				scale = lty.scale,
						   				legend = lty.legend,
						   				chart = lty.chart,
						   				free = lty.free),
						   fill_alpha = tmapScale(aes = "fill_alpha",
						   					   value = fill_alpha,
						   					   scale = fill_alpha.scale,
						   					   legend = fill_alpha.legend,
						   					   chart = fill_alpha.chart,
						   					   free = fill_alpha.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
						   					  chart = col_alpha.chart,
						   					  free = col_alpha.free)),
		
		gpar = tmapGpar(fill = "__fill",
						col = "__col",
						shape = "__shape",
						size = "__size",
						fill_alpha = "__fill_alpha",
						col_alpha = "__col_alpha",
						pattern = "fill",
						lty = "__lty",
						lwd = "__lwd",
						linejoin = NA,
						lineend = NA),
		tpar = tmapTpar(),
		plot.order = plot.order,
		mapping.fun = "Symbols",
		mapping.args = options$mapping.args,
		zindex = zindex,
		group = group,
		group.control = group.control,
		popup.vars = popup.vars,
		popup.format = popup.format,
		hover = hover,
		id = id,
		subclass = c("tm_aes_layer", "tm_layer")))
}

v3_symbols = function(args, args_called) {
	if (any(v3_only("tm_symbols") %in% names(args))) {
		layer_fun = if ("called_from" %in% names(args)) {
			args$called_from
		} else {
			"symbols"
		}
		
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
		
		# v3 visual variable: fill
		fill.scale.args = c(list(n = v3_impute(args, "n", 5), 
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
		fill.scale.args$fun_pref = if (style == "cat") {
			"categorical"
		} else if (style %in% c("fixed", "sd", "equal", "pretty", "quantile",
								"kmeans", "hclust", "bclust", "fisher", "jenks",
								"dpih", "headtails", "log10_pretty")) {
			"intervals"
		} else if (style == "cont") {
			"continuous"
		} else if (style == "log10") {
			"continuous_log"
		} else if (style == "order") {
			"rank"
		} else {
			stop("unknown style")
		}
		
		if ("style" %in% names(args)) {
			v3_tm_scale_instead_of_style(style, scale_fun = fill.scale.args$fun_pref, vv = "fill", layer_fun = layer_fun, arg_list = v3_list_get())
		} else {
			v3_tm_scale(scale_fun = "", vv = "fill", layer_fun = layer_fun, arg_list = v3_list_get())
		}
		fill.scale = do.call("tm_scale", args = fill.scale.args)		
		
		if ("col" %in% names(args_called)) {
			fill = args_called$col
			isn_fill = is.null(fill)
			v3_message_col_fill(layer_fun = layer_fun)
			if (isn_fill) {
				v3_message_vv_null(layer_fun = layer_fun)
				fill = NA
			}
			col = tm_const()
		} else {
			fill = tm_const()
			col = tm_const()
			isn_fill = FALSE
		}
		
		if ("border.col" %in% names(args)) {
			col = args$border.col
			isn_col = is.null(col)
			if (!("col" %in% names(args_called))) v3_message_col_fill(layer_fun = layer_fun)
			if (isn_col) {
				if (!isn_fill) v3_message_vv_null(layer_fun = layer_fun)
				col = NA
			}
		}
		
		
		if ("alpha" %in% names(args)) {
			v3_message_fill_alpha(layer_fun = layer_fun)
			fill_alpha = args$alpha
		} else {
			fill_alpha = tm_const()
		}
		
		if ("border.alpha" %in% names(args)) {
			v3_message_col_alpha(layer_fun = layer_fun)
			col_alpha = args$border.alpha
		} else {
			col_alpha = tm_const()
		}
		
		v3_list_init()
		if ("legend.show" %in% names(args) && !args$legend.show) {
			v3_tm_legend_hide(layer_fun, arg = "legend.show", vv = "fill")
			fill.legend = tm_legend_hide()
		} else {
			fill.legend.args = list(atitle = v3_impute(args, "title.col", NA, "title"),
									na.show = v3_impute(args, "showNA", NA, "na.show"),
									format = v3_impute(args, "legend.format", list(), "format"),
									orientation = ifelse(v3_impute(args, "legend.is.portrait", TRUE, "orientation"), "portrait", "landscape"),
									reverse = v3_impute(args, "legend.reverse", FALSE, "reverse"))
			fill.legend = do.call("tm_legend", fill.legend.args)
			v3_tm_legend(fun = layer_fun, vv = "fill", arg_list = v3_list_get())
		}	
		
		
		# v3 visual variable: size
		v3_list_init()
		size.scale.args = list(ticks = v3_impute(args, "sizes.legend", NULL, "ticks"),
							   values.range = {if ("size.lim" %in% names(args)) {
							   	c(args[["size.lim"]][1] / args[["size.lim"]][2], 1)
							   } else c(0, 1)},
							   values.scale = v3_impute(args, "scale", 1, "values.scale"),
							   limits = v3_impute(args, "size.lim", NULL, "limits"),
							   outliers.trunc = c(TRUE, FALSE),
							   labels = v3_impute(args, "sizes.legend.labels", NULL, "labels"),
							   fun_pref = "continuous")
		if ("size" %in% names(args_called)) v3_tm_scale(scale_fun = "continuous", vv = "size", layer_fun = layer_fun, arg_list = v3_list_get())
		size.scale = do.call("tm_scale", args = size.scale.args)		
		
		v3_list_init()
		if ("legend.size.show" %in% names(args) && !args$legend.size.show) {
			v3_tm_legend_hide(layer_fun, arg = "legend.size.show", vv = "size")
			size.legend = tm_legend_hide()
		} else {
			size.legend.args = alist(title = v3_impute(args, "title.size", NA, "title"),
									na.show = v3_impute(args, "showNA", NA),
									format = v3_impute(args, "legend.format", list(), "format"),
									orientation = ifelse(v3_impute(args, "legend.size.is.portrait", FALSE), "portrait", "landscape"),
									reverse = v3_impute(args, "legend.size.reverse", FALSE))
			if ("size" %in% names(args_called)) v3_tm_legend(fun = layer_fun, vv = "size", arg_list = v3_list_get())
			size.legend = do.call("tm_legend", size.legend.args)
		}
		
		# v3 visual variable: shape
		if (!("shapes.style" %in% names(args))) {
			if (!"shapes.breaks" %in% names(args)) {
				shapes.style = "pretty"
			} else {
				shapes.style = "fixed"
			}
		} else {
			shapes.style = args$shapes.style
		}
		
		v3_list_init()
		shape.scale.args = list(n = v3_impute(args, "shapes.n", 5, "n"), 
								style = shapes.style, 
								style.args = v3_impute(args, "shapes.style.args", list(), "style.args"), 
								breaks = v3_impute(args, "shapes.breaks", NULL, "breaks"), 
								interval.closure = v3_impute(args, "shapes.interval.closure", "left", "interval.closure"), 
								drop.levels = v3_impute(args, "drop.levels", FALSE),
								midpoint = v3_impute(args, "midpoint", NULL), 
								as.count = v3_impute(args, "as.count", NA), 
								value.neutral = v3_impute(args, "shapes.legend", NA, "value.neutral"),
								values = v3_impute(args, "shapes", 21:25, "values"),
								labels = v3_impute(args, "shapes.labels", NULL, "labels"), 
								label.na = v3_impute(args, "shape.textNA", NA, "label.na"), 
								label.null = NA, 
								label.format = v3_impute(args, "legend.format", list(), "label.format"),
								fun_pref = "intervals")
		shape.scale = do.call("tm_scale", args = shape.scale.args)		
		if ("shape" %in% names(args_called)) {
			if ("shapes.style" %in% names(args)) {
				v3_tm_scale_instead_of_style(shapes.style, scale_fun = shape.scale.args$fun_pref, vv = "shape", layer_fun = layer_fun, arg_list = v3_list_get())
			} else {
				v3_tm_scale(scale_fun = shape.scale.args$fun_pref, vv = "shape", layer_fun = layer_fun, arg_list = v3_list_get())
			}
		}
		
		v3_list_init()
		if ("legend.shape.show" %in% names(args) && !args$legend.shape.show) {
			v3_tm_legend_hide(layer_fun, arg = "legend.shape.show", vv = "shape")
			shape.legend = tm_legend_hide()
		} else {
			shape.legend.args = alist(title = v3_impute(args, "title.shape", NA),
									 na.show = v3_impute(args, "shape.showNA ", NA),
									 format = v3_impute(args, "legend.format", list(), "format"),
									 orientation = ifelse(v3_impute(args, "legend.shape.is.portrait", TRUE), "portrait", "landscape"),
									 reverse = v3_impute(args, "legend.shape.reverse", FALSE))
			if ("shape" %in% names(args_called))v3_tm_legend(fun = layer_fun, vv = "shape", arg_list = v3_list_get())
			shape.legend = do.call("tm_legend", shape.legend.args)
		}
		
		if ("legend.hist" %in% names(args) && args$legend.hist) {
			fill.chart = tm_chart_histogram()
			v3_tm_chart_hist(layer_fun = layer_fun, vv = "fill", arg = "legend.hist")
			
			# to do: histogram title
		} else {
			fill.chart = tm_chart_none()
		}
		
		
		v4_opt_args = c("icon.scale", "just", "grob.dim")
		v3_opt_args = c("icon.scale", "just", "grob.dim")
		osel = which(v3_opt_args %in% names(args))
		if (length(osel)) {
			o3 = v3_opt_args[osel]
			o4 = v4_opt_args[osel]
			v3_opt(o3, o4, layer_fun)
		}
		
		list(fill = fill,
			 col = col,
			 col_alpha = col_alpha,
			 fill_alpha = fill_alpha,
			 fill.scale = fill.scale,
			 fill.legend = fill.legend,
			 size.scale = size.scale,
			 size.legend = size.legend,
			 shape.scale = shape.scale,
			 shape.legend = shape.legend,
			 fill.chart = fill.chart)
	} else {
		NULL
	}
}



#' @export
#' @name tm_dots
#' @rdname tm_symbols
tm_dots = function(fill = tm_const(),
				   fill.scale = tm_scale(),
				   fill.legend = tm_legend(),
				   fill.free = NA,
				   size = tm_const(),
				   size.scale = tm_scale(),
				   size.legend = tm_legend(),
				   size.free = NA,
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
				   plot.order = tm_plot_order("size"),
				   zindex = NA,
				   group = NA,
				   group.control = "check",
				   options = opt_tm_dots(),
				   ...) {
	
		args = c(as.list(environment()), list(...))
		args$called_from = "tm_dots"
		
		tm = do.call(tm_symbols, args)
		tm[[1]]$layer = c("dots", "symbols")
		tm
				

}


#' @export
#' @name tm_bubbles
#' @rdname tm_symbols
tm_bubbles = function(size = tm_const(),
					  size.scale = tm_scale(),
					  size.legend = tm_legend(),
					  size.free = NA,
					  fill = tm_const(),
					  fill.scale = tm_scale(),
					  fill.legend = tm_legend(),
					  fill.free = NA,
					  col = tm_const(),
					  col.scale = tm_scale(),
					  col.legend = tm_legend(),
					  col.free = NA,
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
					  plot.order = tm_plot_order("size"),
					  zindex = NA,
					  group = NA,
					  group.control = "check",
					  options = opt_tm_bubbles(),
					  ...) {
	
	args = c(as.list(environment()), list(...))
	args$called_from = "tm_bubbles"
	
	tm = do.call(tm_symbols, args)
	tm[[1]]$layer = c("bubbles", "symbols")
	tm
}


#' @export
#' @name tm_squares
#' @rdname tm_symbols
tm_squares = function(size = tm_const(),
					  size.scale = tm_scale(),
					  size.legend = tm_legend(),
					  size.free = NA,
					  fill = tm_const(),
					  fill.scale = tm_scale(),
					  fill.legend = tm_legend(),
					  fill.free = NA,
					  col = tm_const(),
					  col.scale = tm_scale(),
					  col.legend = tm_legend(),
					  col.free = NA,
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
					  plot.order = tm_plot_order("size"),
					  zindex = NA,
					  group = NA,
					  group.control = "check",
					  options = opt_tm_squares(),
					  ...) {
	
	args = c(as.list(environment()), list(...))
	args$called_from = "tm_squares"
	tm = do.call(tm_symbols, args)
	tm[[1]]$layer = c("squares", "symbols")
	tm
}

