#' @rdname tm_polygons
#' @name opt_tm_polygons
#' @export
opt_tm_polygons = function(polygons.only = "ifany") {
	list(trans.args = list(polygons.only = polygons.only),
		 mapping.args = list())
}


#' Map layer: polygons
#' 
#' Map layer that draws polygons. Supported visual variables are: `fill` (the fill color),
#' `col` (the border color), `lwd` (line width), `lty` (line type),
#' `fill_alpha` (fill color alpha transparency) and `col_alpha` (border color alpha transparency).
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
#' @param fill,fill.scale,fill.legend,fill.chart,fill.free Visual variable that determines the fill color. See details.
#' @param col,col.scale,col.legend,col.chart,col.free Visual variable that determines the border color. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.chart,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.chart,lty.free Visual variable that determines the line type. See details.
#' @param fill_alpha,fill_alpha.scale,fill_alpha.chart,fill_alpha.legend,fill_alpha.free Visual
#'   variable that determines the fill color alpha transparency See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.chart,col_alpha.free Visual variable
#'   that determines the border color alpha transparency. See details.
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
#' @param hover name of the data variable that specifies the hover labels
#' @param id name of the data variable that specifies the indices of the spatial
#'   features. Only used for `"view"` mode.
#' @param polygons.only should only polygon geometries of the shape object (defined in [tm_shape()]) be plotted? By default `"ifany"`, which means `TRUE` in case a geometry collection is specified.
#' @param ... to catch deprecated arguments from version < 4.0
#' @example ./examples/tm_polygons.R 
#' @name tm_polygons
#' @rdname tm_polygons
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
					   plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
					   zindex = NA,
					   group = NA,
					   group.control = "check",
					   popup.vars = NA,
					   popup.format = list(),
					   hover = "",
					   id = "",
					   options = opt_tm_polygons(),
					   ...) {
	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	v3 = c("alpha", "palette", "convert2density", "area", "n", 
		   "style", "style.args", "as.count", "breaks", "interval.closure", 
		   "labels", "drop.levels", "midpoint", "stretch.palette", "contrast", 
		   "colorNA", "textNA", "showNA", "colorNULL", "thres.poly", "title", 
		   "legend.show", "legend.format", "legend.is.portrait", "legend.reverse", 
		   "legend.hist", "legend.hist.title", "legend.z", "legend.hist.z", 
		   "interactive", "auto.palette.mapping", "max.categories")
	
	
	if (any(v3 %in% names(args))) {
		message("tm_polygons: Deprecated tmap v3 code detected. Code translated to v4")
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
		
		fill.scale.args = list(n = imp("n", 5), 
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
							   label.na = imp("textNA", NA), 
							   label.null = NA, 
							   label.format = imp("legend.format", list()))
		fill.scale.args$fun_pref = if (style == "cat") {
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
		
		fill.scale = do.call("tm_scale", args = fill.scale.args)		
		
		if ("col" %in% names(args_called) &&  (is.null(args_called$called_from) || (args_called$called_from != "fill"))) {
			fill = col
			col = tm_const()
		}
		if ("border.col" %in% names(args) && !identical(args$called_from, "borders")) {
			col = args$border.col
		}
		if (identical(args$called_from, "borders")) {
			fill = NA
		}
		
		if ("alpha" %in% names(args)) {
			fill_alpha = args$alpha
		}
		
		fill.legend.args = alist(title = imp("title", NA),
								 show = imp("legend.show", NULL),
								 na.show = imp("na.show", NA),
								 format = imp("legend.format", list()),
								 orientation = ifelse(imp("legend.is.portrait", TRUE), "portrait", "landscape"),
								 reverse = imp("legend.reverse", FALSE))
		
		fill.legend = do.call("tm_legend", fill.legend.args)
		
		if ("legend.hist" %in% names(args) && args$legend.hist) {
			fill.chart = tm_chart_histogram()
			# to do: histogram title
		}
		
	}
	
	
	tm_element_list(tm_element(
		layer = "polygons",
		trans.fun = tmapTransPolygons,
		trans.args = options$trans.args,
		trans.aes = list(),
		trans.isglobal = FALSE,
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

#' @name tm_fill
#' @rdname tm_polygons
#' @export
tm_fill = function(...) {
	args = list(...)
	if (!("col" %in% names(args))) {
		args$col = NA
	}
	args$called_from = "fill"
	do.call(tm_polygons, args)
}

#' @name tm_borders
#' @rdname tm_polygons
#' @export
tm_borders = function(col = tm_const(), ...) {
	args = list(...)
	if (!("fill" %in% names(args))) {
		args$fill = NA
	}
	args$called_from = "borders"
	do.call(tm_polygons, c(list(col = col), args))
}
