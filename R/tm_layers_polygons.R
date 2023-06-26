#' Map layer: polygons
#' 
#' Map layer that draws polygons. Supported visual variables are: \code{fill} (the fill color), \code{col} (the border color), \code{lwd} (line width), \code{lty} (line type), \code{fill_alpha} (fill color alpha transparency) and \code{col_alpha} (border color alpha transparency).
#' 
#' The visual variable arguments (e.g. \code{col}) can be specified with either a data variable name (e.g., a spatial vector attribute or a raster layer of the object specified in \code{\link{tm_shape}}), or with a visual value (for \code{col}, a color is expected). Multiple values can be specified: in that case facets are created. These facets can be combined with other faceting data variables, specified with \code{\link{tm_facets}}.
#' 
#' The \code{.scale} arguments determine the used scale to map the data values to visual variable values. These can be specified with one of the available \code{tm_scale_} functions. The default is specified by the tmap option (\code{\link{tm_options}}) \code{scales.var}.
#' 
#' The \code{.legend} arguments determine the used legend, specified with \code{\link{tm_legend}}. The default legend and its settings are determined by the tmap options (\code{\link{tm_options}}) \code{legend.} .
#' 
#' The \code{.free} arguments determine whether scales are applied freely across facets, or shared. A logical value is required. They can also be specified with a vector of three logical values; these determine whether scales are applied freely per facet dimension. This is only useful when facets are applied (see \code{\link{tm_facets}}). There are maximally three facet dimensions: rows, columns, and pages. This only applies for a facet grid (\code{\link{tm_facets_grid}}). For instance, \code{col.free = c(TRUE, FALSE, FALSE)} means that for the visual variable \code{col}, each row of facets will have its own scale, and therefore its own legend. For facet wraps and stacks (\code{\link{tm_facets_wrap}} and \code{\link{tm_facets_stack}}) there is only one facet dimension, so the \code{.free} argument requires only one logical value.
#' 
#' @param fill,fill.scale,fill.legend,fill.free Visual variable that determines the fill color. See details.
#' @param col,col.scale,col.legend,col.free Visual variable that determines the border color. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.free Visual variable that determines the line type. See details.
#' @param fill_alpha,fill_alpha.scale,fill_alpha.legend,fill_alpha.free Visual variable that determines the fill color alpha transparency See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free Visual variable that determines the border color alpha transparency. See details.
#' @param linejoin,lineend Line join and line end. See \code{\link[grid:gpar]{gpar}} for details.
#' @param plot.order Specification in which order the spatial features are drawn. See \code{\link{tm_plot_order}} for details.
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups can be switched on and off. Options: `"radio"` for radio buttons (meaning only one group can be shown), `"check"` for check boxes (so multiple groups can be shown), and `"none"` for no control (the group cannot be (de)selected).
#' @example ./examples/tm_polygons.R 
#' @name tm_polygons
#' @rdname tm_polygons
#' @export
tm_polygons = function(fill = tm_const(), 
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
					   linejoin = "round",
					   lineend = "round",
					   plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
					   trans.args = list(),
					   mapping.args = list(),
					   zindex = NA,
					   group = NA,
					   group.control = "check",
					   ...) {
	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	v3 = c("alpha", "palette", "convert2density", "area", "n", 
		   "style", "style.args", "as.count", "breaks", "interval.closure", 
		   "labels", "drop.levels", "midpoint", "stretch.palette", "contrast", 
		   "colorNA", "textNA", "showNA", "colorNULL", "thres.poly", "title", 
		   "legend.show", "legend.format", "legend.is.portrait", "legend.reverse", 
		   "legend.hist", "legend.hist.title", "legend.z", "legend.hist.z", 
		   "id", "interactive", "popup.vars", "popup.format", "auto.palette.mapping", "max.categories")
	
	
	if (any(v3 %in% names(args))) {
		message("Deprecated tmap v3 code detected. Code translated to v4")
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
		} else if (style %in% c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails")) {
			"intervals"
		} else if (style == "cont") {
			"continuous"
		} else if (style == "log10") {
			"continuous_log"
		} else {
			stop("unknown style")
		}
		
		fill.scale = do.call("tm_scale", args = fill.scale.args)		
		
		if ("col" %in% names(args_called) && (args_called$called_from != "fill")) {
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
	}
	
	
	tm_element_list(tm_element(
		layer = "polygons",
		trans.fun = tmapTransPolygons,
		trans.args = trans.args,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(fill = tmapScale(aes = "fill",
											value = fill,
											scale = fill.scale,
											legend = fill.legend,
											free = fill.free),
						   col = tmapScale(aes = "col",
						   				value = col,
						   				scale = col.scale,
						   				legend = col.legend,
						   				free = col.free),
						   lwd = tmapScale(aes = "lwd",
						   				value = lwd,
						   				scale = lwd.scale,
						   				legend = lwd.legend,
						   				free = lwd.free),
						   lty = tmapScale(aes = "lty",
						   				value = lty,
						   				scale = lty.scale,
						   				legend = lty.legend,
						   				free = lty.free),
						   fill_alpha = tmapScale(aes = "fill_alpha",
						   					   value = fill_alpha,
						   					   scale = fill_alpha.scale,
						   					   legend = fill_alpha.legend,
						   					   free = fill_alpha.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
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
		mapping.args = mapping.args,
		zindex = zindex,
		group = group,
		group.control = group.control,
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