#' Map layer: lines
#'
#' Map layer that draws lines. Supported visual variables are: `col` (the color), `lwd` (line width), `lty` (line type), and `col_alpha` (color alpha transparency).
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
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines the col color. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.chart,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.chart,lty.free Visual variable that determines the line type. See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free Visual variable that determines the border color alpha transparency. See details.
#' @param linejoin,lineend line join and line end. See [gpar()][grid::gpar()] for details.
#' @param plot.order Specification in which order the spatial features are drawn. See [tm_plot_order()] for details.
#' @param options options passed on to the corresponding `opt_<layer_function>` function
#' @inheritParams tm_polygons
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_lines.R
#' @export
tm_lines = function(col = tm_const(),
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
					col_alpha = tm_const(),
					col_alpha.scale = tm_scale(),
					col_alpha.legend = tm_legend(),
					col_alpha.chart = tm_chart_none(),
					col_alpha.free = NA,
					linejoin = "round",
					lineend = "round",
					plot.order = tm_plot_order("lwd", reverse = TRUE, na.order = "bottom"),
					zindex = NA,
					group = NA,
					group.control = "check",
					popup.vars = NA,
					popup.format = list(),
					hover = NA,
					id = "",
					options = opt_tm_lines(),
					...) {

	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())

	if (any(v3_only("tm_lines") %in% names(args))) {
		layer_fun = if ("called_from" %in% names(args)) {
			args$called_from
		} else {
			"lines"
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
		# v3 visual variable: col
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
		if (!(layer_fun == "qtm" && (!"col" %in% names(args)))) {
			if ("style" %in% names(args)) {
				v3_tm_scale_instead_of_style(style, scale_fun = col.scale.args$fun_pref, vv = "col", layer_fun = layer_fun, arg_list = v3_list_get())
			} else {
				v3_tm_scale(scale_fun = "", vv = "col", layer_fun = layer_fun, arg_list = v3_list_get())
			}
		}

		col.scale = do.call("tm_scale", args = col.scale.args)



		if ("alpha" %in% names(args)) {
			col_alpha = args$alpha
			v3_message_col_alpha(layer_fun = layer_fun, orig = "alpha")

		}


		v3_list_init()
		if ("legend.col.show" %in% names(args) && !args$legend.col.show) {
			v3_tm_legend_hide(layer_fun, arg = "legend.col.show", vv = "col")
			lwd.legend = tm_legend_hide()
		} else {
			col.legend.args = alist(title = v3_impute(args, "title.col", NA, "title"),
									na.show = v3_impute(args, "showNA", NA),
									format = v3_impute(args, "legend.format", list(), "format"),
									orientation = ifelse(v3_impute(args, "legend.col.is.portrait", TRUE, "orientation"), "portrait", "landscape"),
									reverse = v3_impute(args, "legend.reverse", FALSE, "reverse"))
			col.legend = do.call("tm_legend", col.legend.args)
			v3_tm_legend(fun = layer_fun, vv = "col", arg_list = v3_list_get())
		}

		# v3 visual variable: lwd
		# v3 visual variable: size
		v3_list_init()
		lwd.scale.args = list(ticks = v3_impute(args, "lwd.legend", NULL, "ticks"),
							  labels = v3_impute(args, "lwd.legend.labels", NULL, "labels"),
							  values.scale = v3_impute(args, "scale", 1, "values.scale"),
							  fun_pref = "continuous")
		if ("lwd" %in% names(args_called)) v3_tm_scale(scale_fun = "continuous", vv = "lwd", layer_fun = layer_fun, arg_list = v3_list_get())
		lwd.scale = do.call("tm_scale", args = lwd.scale.args)

		v3_list_init()
		if ("legend.lwd.show" %in% names(args) && !args$legend.lwd.show) {
			v3_tm_legend_hide(layer_fun, arg = "legend.lwd.show", vv = "lwd")
			lwd.legend = tm_legend_hide()
		} else {
			lwd.legend.args = alist(title = v3_impute(args, "title.lwd", NA, "title"),
								   na.show = v3_impute(args, "showNA", NA),
								   format = v3_impute(args, "legend.format", list(), "format"),
								   orientation = ifelse(v3_impute(args, "legend.lwd.is.portrait", FALSE), "portrait", "landscape"),
								   reverse = v3_impute(args, "legend.lwd.reverse", FALSE))
			if ("lwd" %in% names(args_called)) v3_tm_legend(fun = layer_fun, vv = "lwd", arg_list = v3_list_get())
			lwd.legend = do.call("tm_legend", lwd.legend.args)
		}


		if ("legend.hist" %in% names(args) && args$legend.hist) {
			col.chart = tm_chart_histogram()
			v3_tm_chart_hist(layer_fun = layer_fun, vv = "col", arg = "legend.hist")

			# to do: histogram title
		}
	}



	tm_element_list(tm_element(
		layer = "lines",
		trans.fun = tmapTransLines,
		trans.aes = list(),
		trans.args = options$trans.args,
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   chart = col.chart,
										   free = col.free),
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
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
						   					  chart = col_alpha.chart,
						   					  free = col_alpha.free)),

		gpar = tmapGpar(fill = NA,
						col = "__col",
						shape = NA,
						size = NA,
						fill_alpha = NA,
						col_alpha = "__col_alpha",
						pattern = NA,
						lty = "__lty",
						lwd = "__lwd",
						linejoin = linejoin,
						lineend = lineend),
		tpar = tmapTpar(),
		plot.order = plot.order,
		mapping.fun = "Lines",
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

#' @rdname tm_lines
#' @param lines.only should only line geometries of the shape object (defined in [tm_shape()]) be plotted, or also other geometry types (like polygons)? By default `"ifany"`, which means `TRUE` in case a geometry collection is specified.
#' @export
opt_tm_lines = function(lines.only = "ifany") {
	list(trans.args = list(lines.only = lines.only),
		 mapping.args = list())
}
