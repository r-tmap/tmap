#' @rdname tm_raster
#' @name opt_tm_raster
#' @export
opt_tm_raster = function() {
	list(trans.args = list(),
		 mapping.args = list())
}


#' Map layer: raster
#' 
#' Map layer that draws rasters. Supported visual variable is: `col` (the  color).
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
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines the color.
#'   See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free Visual variable
#'   that determines the alpha transparency. See details.
#' @param zindex Map layers are drawn on top of each other.
#'   The `zindex` numbers (one for each map layer) determines the stacking order.
#'   By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only
#'   relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups
#'   can be switched on and off. Options: `"radio"` for radio buttons (meaning only
#'   one group can be shown), `"check"` for check boxes (so multiple groups can be
#'   shown), and `"none"` for no control (the group cannot be (de)selected).
#' @param options options passed on to the corresponding `opt_<layer_function>` function 
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_raster.R 
#' @export
tm_raster = function(col = tm_shape_vars(),
					 col.scale = tm_scale(value.na = "#00000000"),
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
	
	
	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	
	if (any(v3_only("tm_raster") %in% names(args))) {
		layer_fun = paste0("tm_", {if ("called_from" %in% names(args)) {
			args$called_from
		} else {
			"raster"
		}})
		
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
		col.scale.args = list(n = v3_impute(args, "n", 5), 
							  style = style, 
							  style.args = v3_impute(args, "style.args", list()), 
							  breaks = v3_impute(args, "breaks", NULL), 
							  interval.closure = v3_impute(args, "interval.closure", "left"), 
							  drop.levels = v3_impute(args, "drop.levels", FALSE),
							  midpoint = v3_impute(args, "midpoint", NULL), 
							  as.count = v3_impute(args, "as.count", NA), 
							  values = v3_impute(args, "palette", NA, "values"), 
							  values.repeat = !v3_impute(args, "stretch.palette", TRUE, "values.repeat"), 
							  values.range = v3_impute(args, "contrast", NA, "values.range"), 
							  values.scale = 1, 
							  value.na = v3_impute(args, "colorNA", NA, "value.na"), 
							  value.null = v3_impute(args, "colorNULL", NA, "value.null"), 
							  value.neutral = NA, 
							  labels = v3_impute(args, "labels", NULL), 
							  label.na = v3_impute(args, "textNA", NA, "label.na"), 
							  label.null = NA, 
							  label.format = v3_impute(args, "legend.format", list(), "label.format"))
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
			col.legend.args = list(title = v3_impute(args, "title", NA, "title"),
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

	# needed for color maps without categories (then tm_scale_categorical is used without legend, unless called)
	col.legend$called = "col.legend" %in% names(args_called)
	
	
	tm_element_list(tm_element(
		layer = "raster",
		trans.fun = tmapTransRaster,
		trans.aes = list(),
		trans.args = options$trans.args,
		trans.isglobal = FALSE,
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
