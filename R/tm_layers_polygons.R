#' Map layer: polygons
#'
#' Map layer that draws polygons. Supported visual variables are: `fill` (the fill color),
#' `col` (the border color), `lwd` (line width), `lty` (line type),
#' `fill_alpha` (fill color alpha transparency) and `col_alpha` (border color alpha transparency).
#'
#' The visual variable arguments (e.g. `col`) can be specified with either a data
#' variable name (e.g., a spatial vector attribute or a raster layer of the object
#' specified in [tm_shape()]), or with a visual value (for `col`, a color is expected).
#' See \href{https://r-tmap.github.io/tmap/articles/basics_vv}{vignette about visual variables}.
#'
#' Multiple values can be specified: in that case facets are created.
#' These facets can be combined with other faceting data variables, specified with [tm_facets()].
#' See \href{https://r-tmap.github.io/tmap/articles/basics_facets}{vignette about facets}.
#'
#' - The `*.scale` arguments determine the used scale to map the data values to
#' visual variable values. These can be specified with one of the available
#' `tm_scale_*()` functions. The default is specified by the tmap option ([tm_options()]) `scales.var`.
#' See \href{https://r-tmap.github.io/tmap/articles/basics_scales}{vignette about scales}.
#'
#' - The `*.legend` arguments determine the used legend, specified with [tm_legend()].
#' The default legend and its settings are determined by the tmap options ([tm_options()]) `legend.` .
#' See \href{https://r-tmap.github.io/tmap/articles/basics_legends}{vignette about legends}.
#'
#' - The `*.chart` arguments specify additional charts, specified with `tm_chart_`, e.g. [tm_chart_histogram()].
#' See \href{https://r-tmap.github.io/tmap/articles/basics_charts}{vignette about charts}.
#'
#' - The `*.free` arguments determine whether scales are applied freely across facets, or shared.
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
#' @param fill,fill.scale,fill.legend,fill.chart,fill.free `r .doc_vv("fill")`
#' @param col,col.scale,col.legend,col.chart,col.free `r .doc_vv("col")`
#' @param lwd,lwd.scale,lwd.legend,lwd.chart,lwd.free `r .doc_vv("lwd")`
#' @param lty,lty.scale,lty.legend,lty.chart,lty.free `r .doc_vv("lty")`
#' @param fill_alpha,fill_alpha.scale,fill_alpha.chart,fill_alpha.legend,fill_alpha.free `r .doc_vv("fill_alpha")`
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free `r .doc_vv("col_alpha")`
#' @param linejoin,lineend Line join and line end. See [gpar()][grid::gpar()] for details.
#' @param plot.order Specification in which order the spatial features are drawn.
#'   See [tm_plot_order()] for details.
#' @param zindex Map layers are drawn on top of each other. The `zindex` numbers
#'   (one for each map layer) determines the stacking order.
#'   By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only
#'   relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups
#'   can be switched on and off. Options: `"radio"` for radio buttons
#'   (meaning only one group can be shown), `"check"` for check boxes
#'   (so multiple groups can be shown), and `"none"` for no control
#'   (the group cannot be (de)selected).
#' @param options options passed on to the corresponding `opt_<layer_function>` function
#' @param popup.vars names of data variables that are shown in the popups
#'   in `"view"` mode. Set popup.vars to `TRUE` to show all variables in the
#'   shape object. Set popup.vars to `FALSE` to disable popups. Set `popup.vars`
#'   to a character vector of variable names to those those variables in the popups.
#'   The default (`NA`) depends on whether visual variables (e.g.`fill`) are used.
#'   If so, only those are shown. If not all variables in the shape object are shown.
#' @param popup.format list of formatting options for the popup values.
#'   See the argument `legend.format` for options. Only applicable for
#'   numeric data variables. If one list of formatting options is provided,
#'   it is applied to all numeric variables of `popup.vars`. Also, a (named)
#'   list of lists can be provided. In that case, each list of formatting options
#'   is applied to the named variable.
#' @param hover name of the data variable that specifies the hover labels (view mode only). Set to `FALSE` to disable hover labels. By default `FALSE`, unless `id` is specified. In that case, it is set to `id`,
#' @param id name of the data variable that specifies the indices of the spatial
#'   features. Only used for `"view"` mode.
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_polygons.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/examples_choro_World}{Choropleth example (1)} and \href{https://r-tmap.github.io/tmap/articles/examples_choro_NLD}{choropleth example (2)}
#' @export
tm_polygons = function(fill = tm_const(),
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
					   options = opt_tm_polygons(),
					   ...) {

	args_called = names(rlang::call_match()[-1])
	args = list(...)

	layer_fun = if ("called_from" %in% names(args)) {
		args$called_from
	} else {
		"tm_polygons"
	}

	if (any(v3_only("tm_polygons") %in% names(args))) {



		v3_start_message()


		if (!("style" %in% names(args))) {
			if (!"breaks" %in% names(args) || is.null(args$breaks)) {
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


		fill.scale.args$fun_pref = if (style[1] == "cat") {
			"categorical"
		} else if (style %in% c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails", "log10_pretty")) {
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
			v3_tm_scale_instead_of_style(style[1], scale_fun = fill.scale.args$fun_pref, vv = "fill", layer_fun = layer_fun, arg_list = v3_list_get())
		} else {
			v3_tm_scale(scale_fun = "", vv = "fill", layer_fun = layer_fun, arg_list = v3_list_get())
		}

		fill.scale = do.call("tm_scale", args = fill.scale.args)

		if ("convert2density" %in% names(args) && args$convert2density) {
			fill.scale$convert2density = TRUE
			v3_convert2density(layer_fun)
		}

		if ("col" %in% args_called && (!layer_fun %in% c("tm_fill", "qtm"))) {
			fill = col
			col = tm_const()
			v3_message_col_fill(layer_fun = layer_fun)
		}



		if ("border.col" %in% names(args)) {
			col = args$border.col
			if (!("col" %in% args_called)) v3_message_col_fill(layer_fun = layer_fun)
		}
		if (identical(args$called_from, "tm_borders")) {
			fill = NA
		}

		if ("alpha" %in% names(args)) {
			fill_alpha = args$alpha
			v3_message_fill_alpha(layer_fun = layer_fun)
		}

		if ("border.alpha" %in% names(args)) {
			col_alpha = args$border.alpha
			v3_message_col_alpha(layer_fun = layer_fun)
		}


		v3_list_init()
		if ("legend.show" %in% names(args) && !args$legend.show) {
			v3_tm_legend_hide(layer_fun, arg = "legend.show", vv = "fill")
			fill.legend = tm_legend_hide()
		} else {
			fill.legend.args = alist(title = v3_impute(args, "title", NA),
									na.show = v3_impute(args, "showNA", NA, "na.show"),
									format = v3_impute(args, "legend.format", list(), "format"),
									orientation = ifelse(v3_impute(args, "legend.is.portrait", TRUE, "orientation"), "portrait", "landscape"),
									reverse = v3_impute(args, "legend.reverse", FALSE, "reverse"))
			fill.legend = do.call("tm_legend", fill.legend.args)
			v3_tm_legend(fun = layer_fun, vv = "fill", arg_list = v3_list_get())
		}

		if ("legend.hist" %in% names(args) && args$legend.hist) {
			fill.chart = tm_chart_histogram()
			v3_tm_chart_hist(layer_fun = layer_fun, vv = "fill", arg = "legend.hist")

			# to do: histogram title
		}

	}

	# unused arguments: typos?
	unused = setdiff(names(args), c(v3_only("tm_polygons"), "called_from"))

	if (length(unused)) {
		message_layer_unused_args(layer_fun, unused)
	}


	tm_element_list(tm_element(
		layer = "polygons",
		trans.fun = tmapTransPolygons,
		trans.args = options$trans.args,
		trans.aes = list(),
		trans.apply_to = "this",
		mapping.aes = list(fill = tmapScale(aes = "fill",
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
						shape = NA,
						size = NA,
						fill_alpha = "__fill_alpha",
						col_alpha = "__col_alpha",
						pattern = "fill",
						lty = "__lty",
						lwd = "__lwd",
						linejoin = linejoin,
						lineend = lineend),
		tpar = tmapTpar(area = "AREA"),
		plot.order = plot.order,
		mapping.fun = "Polygons",
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

#' @rdname tm_polygons
#' @export
tm_fill = function(...) {
	args = list(...)
	# tricks to make backward comp. work
	if (!("col" %in% names(args))) {
		args["col"] = list(NULL)
	}
	args$called_from = if (names(args)[1] == "") "tm_fill" else  "tm_polygons"
	do.call(tm_polygons, args)
}

#' @rdname tm_polygons
#' @export
tm_borders = function(col = tm_const(), ...) {
	args = list(...)
	if (!("fill" %in% names(args))) {
		args["fill"] = list(NULL)
	}
	args$called_from = "tm_borders"
	args$popup.vars = FALSE
	args$hover = FALSE
	do.call(tm_polygons, c(list(col = col), args))
}


#' @param polygons.only should only polygon geometries of the shape object (defined in [tm_shape()]) be plotted? By default `"ifany"`, which means `TRUE` in case a geometry collection is specified.
#' @rdname tm_polygons
#' @export
opt_tm_polygons = function(polygons.only = "ifany") {
	list(trans.args = list(polygons.only = polygons.only),
		 mapping.args = list())
}

