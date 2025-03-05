#' Map layer: raster
#'
#' Map layer that draws rasters. Supported visual variable is: `col` (the  color).
#'
#' @param col,col.scale,col.legend,col.chart,col.free  `r .doc_vv("col")`
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free  `r .doc_vv("col_alpha")`
#' @inheritParams tm_polygons
#' @param options options passed on to the corresponding `opt_<layer_function>` function
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_raster.R
#' @inherit tm_polygons details
#' @export
tm_raster = function(col = tm_vars(),
					 col.scale = tm_scale(),
					 col.legend = tm_legend(),
					 col.chart = tm_chart_none(),
					 col.free = NA,
					 col_alpha = tm_const(),
					 col_alpha.scale = tm_scale(),
					 col_alpha.legend = tm_legend(),
					 col_alpha.chart = tm_chart_none(),
					 col_alpha.free = NA,
					 zindex = NA,
					 group = NA,
					 group.control = "check",
					 options = opt_tm_raster(),
					 ...) {


	args_called = names(rlang::call_match()[-1])
	args = list(...)

	layer_fun = if ("called_from" %in% names(args)) {
		args$called_from
	} else {
		"tm_raster"
	}

	if (any(v3_only("tm_raster") %in% names(args))) {


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
		if ("style" %in% names(args)) {
			v3_tm_scale_instead_of_style(style, scale_fun = col.scale.args$fun_pref, vv = "col", layer_fun = layer_fun, arg_list = v3_list_get())
		} else {
			v3_tm_scale(scale_fun = "", vv = "col", layer_fun = layer_fun, arg_list = v3_list_get())
		}
		col.scale = do.call("tm_scale", args = col.scale.args)

		if ("alpha" %in% names(args)) {
			col_alpha = args$alpha
			v3_message_col_alpha(layer_fun = layer_fun, orig = "alpha")

		}

		v3_list_init()
		if ("legend.show" %in% names(args) && !args$legend.show) {
			v3_tm_legend_hide(layer_fun, arg = "legend.show", vv = "col")
			lwd.legend = tm_legend_hide()
		} else {
			col.legend.args = alist(title = v3_impute(args, "title", NA, "title"),
								   na.show = v3_impute(args, "showNA", NA),
								   format = v3_impute(args, "legend.format", list(), "format"),
								   orientation = ifelse(v3_impute(args, "legend.is.portrait", TRUE, "orientation"), "portrait", "landscape"),
								   reverse = v3_impute(args, "legend.reverse", FALSE, "reverse"))
			col.legend = do.call("tm_legend", col.legend.args)
			v3_tm_legend(fun = layer_fun, vv = "col", arg_list = v3_list_get())
		}

		if ("legend.hist" %in% names(args) && args$legend.hist) {
			col.chart = tm_chart_histogram()
			v3_tm_chart_hist(layer_fun = layer_fun, vv = "col", arg = "legend.hist")

			# to do: histogram title
		}
	}

	# unused arguments: typos?
	unused = setdiff(names(args), c(v3_only("tm_raster"), "called_from"))

	if (length(unused)) {
		message_layer_unused_args(layer_fun, unused)
	}



	# needed for color maps without categories (then tm_scale_categorical is used without legend, unless called)
	col.legend$called = "col.legend" %in% args_called


	tm_element_list(tm_element(
		layer = "raster",
		trans.fun = tmapTransRaster,
		trans.aes = list(),
		trans.args = options$trans.args,
		trans.apply_to = "this",
		mapping.aes = list(col = tmapScale(aes = "col",
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
						   					  free = col_alpha.free)),

		gpar = tmapGpar(fill = "__col",
						col = NA,
						shape = NA,
						size = NA,
						fill_alpha = "__col_alpha",
						col_alpha = NA,
						pattern = "fill",
						lty = NA,
						lwd = NA,
						linejoin = NA,
						lineend = NA),
		tpar = tmapTpar(),
		plot.order = tm_plot_order("DATA"),
		mapping.fun = "Raster",
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

#' @rdname tm_raster
#' @param interpolate Should the raster image be interpolated? Currently only applicable in view mode (passed on to [`grid`][grid::rasterGrob()])
#' @export
opt_tm_raster = function(interpolate = FALSE) {
	list(trans.args = list(),
		 mapping.args = list(interpolate = interpolate))
}

