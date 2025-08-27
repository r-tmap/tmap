#' Map layer: symbols
#'
#' Map layer that draws symbols Supported visual variables are:
#' `fill` (the fill color), `col` (the border color), `size` the symbol size,
#' `shape` the symbol shape, `lwd` (line width), `lty` (line type), `fill_alpha`
#' (fill color alpha transparency) and `col_alpha` (border color alpha transparency).
#'
#' A symbol shape specification is one of the following three options.
#' \enumerate{
#'  \item{A numeric value that specifies the plotting character of the symbol. See parameter \code{pch} of \code{\link[graphics:points]{points}} and the last example to create a plot with all options. Note that this is not supported for the \code{"view" mode.}}
#'  \item{A \code{\link[grid:grid.grob]{grob}} object, which can be a ggplot2 plot object created with \code{\link[ggplot2:ggplotGrob]{ggplotGrob}}. To specify multiple shapes, a list of grob objects is required. Tip: for proportional symbols, such as donuts or pies, see the extension package \href{https://r-tmap.github.io/tmap/articles/ext_glyphs}{tmap.glyphs}}.
#'  \item{An icon specification, which can be created with \code{\link{tmap_icons}}.}
#'  }
#'  To specify multiple shapes (needed for the \code{shapes} argument), a vector or list of these shape specification is required. The shape specification options can also be mixed. For the \code{shapes} argument, it is possible to use a named vector or list, where the names correspond to the value of the variable specified by the \code{shape} argument.
#'  For small multiples, a list of these shape specification(s) should be provided.
#'
#' @param fill,fill.scale,fill.legend,fill.chart,fill.free `r .doc_vv("fill")`
#' @param col,col.scale,col.legend,col.chart,col.free `r .doc_vv("col")`
#' @param size,size.scale,size.legend,size.chart,size.free `r .doc_vv("size")`
#' @param shape,shape.scale,shape.legend,shape.chart,shape.free `r .doc_vv("shape")`
#' @param lwd,lwd.scale,lwd.legend,lwd.chart,lwd.free `r .doc_vv("lwd")`
#' @param lty,lty.scale,lty.legend,lty.chart,lty.free `r .doc_vv("lty")`
#' @param fill_alpha,fill_alpha.scale,fill_alpha.legend,fill_alpha.chart,fill_alpha.free `r .doc_vv("fill_alpha")`
#'   the fill color alpha transparency See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free `r .doc_vv("col_alpha")`
#' @param plot.order Specification in which order the spatial features are drawn.
#'   See [tm_plot_order()] for details.
#' @param options options passed on to the corresponding `opt_<layer_function>` function
#' @inherit tm_polygons details
#' @inheritParams tm_polygons
#' @seealso \href{https://r-tmap.github.io/tmap/articles/examples_bubble}{Bubble map example} and \href{https://r-tmap.github.io/tmap/articles/examples_terrain}{terrain map example}
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_symbols.R
#' @export
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
					  popup.format = tm_label_format(),
					  hover = NA,
					  id = "",
					  options = opt_tm_symbols(),
					  ...) {

	args = list(...)
	args_called = names(rlang::call_match()[-1])


	res = v3_symbols(args, args_called, col)
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

	# unused arguments: typos?
	unused = setdiff(names(args), c(v3_only("tm_symbols"), "called_from"))

	if (length(unused)) {
		layer_fun = if ("called_from" %in% names(args)) {
			args$called_from
		} else {
			"tm_symbols"
		}

		message_layer_unused_args(layer_fun, unused)
	}





	tm_element_list(tm_element(
		layer = "symbols",
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.args = options$trans.args,
		trans.apply_to = "this",
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

v3_symbols = function(args, args_called, arg_col = NULL) {
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

		if ("col" %in% args_called) {
			fill = arg_col
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
			if (!("col" %in% args_called)) v3_message_col_fill(layer_fun = layer_fun)
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
		if ("size" %in% args_called) v3_tm_scale(scale_fun = "continuous", vv = "size", layer_fun = layer_fun, arg_list = v3_list_get())
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
			if ("size" %in% args_called) v3_tm_legend(fun = layer_fun, vv = "size", arg_list = v3_list_get())
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
		if ("shape" %in% args_called) {
			if ("shapes.style" %in% args) {
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
			if ("shape" %in% args_called)v3_tm_legend(fun = layer_fun, vv = "shape", arg_list = v3_list_get())
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
			v3_layer_opt(o3, o4, layer_fun)
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
				   plot.order = tm_plot_order("DATA"),
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

#' @rdname tm_symbols
#' @inheritParams tm_text
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
	args_text = c(e, a_other, list(called_from = "tm_markers"))

	tm_t = do.call(tm_text, args_text)
	tm_t[[1]]$layer = c("labels", "text")

	tm_d = do.call(tm_dots, args_dots)
	tm_d[[1]]$layer = c("markers", "symbols")

	if (options$markers$markers_on_top_of_text) tm_t + tm_d else tm_d + tm_t
}

#' @inheritParams opt_tm_labels
#' @param markers_on_top_of_text should markers be plot on top of the text (by default `FALSE`)
#' @param dots.icon.scale scaling number that determines how large the icons (or grobs) are in plot mode in comparison to proportional symbols (such as bubbles). In view mode, the size is determined by the icon specification (see \code{\link{tmap_icons}}) or, if grobs are specified by \code{grob.width} and \code{grob.height}
#' @param dots.just justification of the text relative to the point coordinates. Either one of the following values: \code{"left"} , \code{"right"}, \code{"center"}, \code{"bottom"}, and \code{"top"}, or a vector of two values where first value specifies horizontal and the second value vertical justification. Besides the mentioned values, also numeric values between 0 and 1 can be used. 0 means left justification for the first value and bottom justification for the second value. Note that in view mode, only one value is used.
#' @param dots.grob.dim vector of four values that determine how grob objects (see details) are shown in view mode. The first and second value are the width and height of the displayed icon. The third and fourth value are the width and height of the rendered png image that is used for the icon. Generally, the third and fourth value should be large enough to render a ggplot2 graphic successfully. Only needed for the view mode.
#' @rdname tm_symbols
#' @export
opt_tm_markers = function(markers_on_top_of_text = FALSE,
						  points_only = "ifany",
						  point_per = "feature",
						  on_surface = FALSE,
						  shadow = FALSE,
						  shadow.offset.x = 0.1,
						  shadow.offset.y = 0.1,
						  just = "center",
						  along_lines = TRUE,
						  bg.padding = 0.4,
						  clustering = TRUE,
						  point.label = TRUE,
						  point.label.gap = 0.4,
						  point.label.method = "SANN",
						  remove_overlap = FALSE,
						  dots.just = NA,
						  dots.icon.scale = 3,
						  dots.grob.dim = c(width=48, height=48, render.width=256, render.height=256)) {
	list(markers = list(markers_on_top_of_text = markers_on_top_of_text),
		 text = opt_tm_labels(points_only = points_only,
		 					 point_per = point_per,
		 					 on_surface = on_surface,
		 					 shadow = shadow,
		 					 shadow.offset.x = shadow.offset.x,
		 					 shadow.offset.y = shadow.offset.y,
		 					 just = just,
		 					 along_lines = along_lines,
		 					 bg.padding = bg.padding,
		 					 clustering = clustering,
		 					 point.label = point.label,
		 					 point.label.gap = point.label.gap,
		 					 point.label.method = point.label.method,
		 					 remove_overlap = remove_overlap),
		 dots = opt_tm_dots(points_only = points_only,
		 				   point_per = point_per,
		 				   on_surface = on_surface,
		 				   icon.scale = dots.icon.scale,
		 				   just = dots.just,
		 				   grob.dim = dots.grob.dim))
}


#' @param icon.scale scaling number that determines how large the icons (or grobs) are in plot mode in comparison to proportional symbols (such as bubbles). For view mode, use the argument `grob.dim`
#' @param grob.dim vector of four values that determine how grob objects (see details) are shown in view mode. The first and second value are the width and height of the displayed icon. The third and fourth value are the width and height of the rendered png image that is used for the icon. Generally, the third and fourth value should be large enough to render a ggplot2 graphic successfully. Only needed for the view mode.
#' @rdname tm_symbols
#' @export
opt_tm_symbols = function(points_only = "ifany",
						  point_per = "feature",
						  on_surface = FALSE,
						  icon.scale = 3,
						  just = NA,
						  grob.dim = c(width=48, height=48, render.width=256, render.height=256)) {
	list(trans.args = list(points_only = points_only, point_per = point_per, on_surface = on_surface, along_lines = FALSE),
		 mapping.args = list(icon.scale = icon.scale,
		 					just = just,
		 					grob.dim = grob.dim))
}

#' @rdname tm_symbols
#' @export
opt_tm_dots = opt_tm_symbols


#' @rdname tm_symbols
#' @export
opt_tm_bubbles = opt_tm_symbols

#' @rdname tm_symbols
#' @export
opt_tm_squares = opt_tm_symbols
