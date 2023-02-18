#' Determine plotting order of features
#' 
#' Determine plotting order of features.
#' 
#' @param aes visual variable for which the values determine the plotting order. Example: bubble map where the \code{"size"} aesthetic is used. A data variable (say population) is mapped via a continuous scale (\code{tm_scale_continuous}) to bubble sizes. The bubbles are plotted in order of size. How is determined by the other arguments. Use \code{"DATA"} to keep the same order as in the data. Another special value is \code{"AREA"} which is preserved for polygons: rather than a data variable the polygon area determines the plotting order.
#' @param reverse logical that determines whether the visual values are plotted in reversed order. The visual values (specified with tmap option \code{"values.var"}) are by default reversed, so plotted starting from the last value. In the bubble map example, this means that large bubbles are plotted first, hence at the bottom.
#' @param na.order where should features be plotted that have an \code{NA} value for (at least) one other aesthetic variable? In the (order) \code{"mix"}, at the \code{"bottom"}, or on \code{"top"}? In the bubble map example: if fill color is missing for some bubble, where should those bubbles be plotted?
#' @param null.order where should non-selected (aka null) features be plotted?
#' @param null.below.na should null features be plotted below NA features?
#' @export
tm_plot_order = function(aes, reverse = TRUE, na.order = c("mix", "bottom", "top"), null.order = c("bottom", "mix", "top"), null.below.na = TRUE) {
	na.order = match.arg(na.order)
	null.order = match.arg(null.order)
	structure(list(aes = aes, reverse = reverse, na.order = na.order, null.order = null.order, null.below.na = null.below.na), class = "tm_plot_order")
}

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
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called. Not implemented yet.
#' @param group Name of the group to which this layer belongs. Not implemented yet.
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

		if ("col" %in% names(args_called)) {
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







#' @export
tm_cartogram = function(size = 1,
						size.scale = tm_scale(),
						size.legend = tm_legend_hide(),
						size.free = NA,
						plot.order = tm_plot_order("size", reverse = FALSE),
						trans.args = list(type = c("cont", "ncont", "dorling"), itermax = 15),
						...) {
	po = plot.order
	type = match.arg(type)
	tmp = do.call(tm_polygons, list(...))
	tmp[[1]] = within(tmp[[1]], {
		trans.fun = tmapTransCartogram
		trans.args = trans.args
		trans.aes = list(size = tmapScale(aes = "area",
										  value = size,
										  scale = size.scale,
										  legend = size.legend,
										  free = size.free))
		tpar = tmapTpar(area = "__area")
		trans.isglobal = TRUE
		plot.order = po
	})
	tmp
}

# toy example
tm_balloons = function(col = tm_const(),
					   col.scale = tm_scale(),
					   col.legend = tm_legend(),
					   col.free = NA,
					   size = 1,
					   size.scale = tm_scale(),
					   size.legend = tm_legend_hide(),
					   size.free = NA,
					   alpha = 0.5,
					   lwd = 2,
					   zindex = NA,
					   group = NA) {
	
	tm_element_list(tm_element(
		layer = "balloons",
		trans.fun = tmapTransCartogram,
		trans.args = list(type = "cont", itermax = 15),
		trans.aes = list(size = tmapScale(aes = "area",
										  value = size,
										  scale = size.scale,
										  legend = size.legend,
										  free = size.free)),
		trans.isglobal = TRUE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free)),
		
		gpar = tmapGpar(fill = "__col",
						col = "__col",
						shape = NA,
						size = NA,
						fill_alpha = alpha,
						col_alpha = 1,
						pattern = "fill",
						lty = NA,
						lwd = lwd,
						linejoin = NA,
						lineend = NA),
		tpar = tmapTpar(area = "__area"),
		mapping.fun = "Polygons",
		zindex = zindex,
		group = group,
		subclass = c("tm_aes_layer", "tm_layer")))
}



#' Map layer: raster
#' 
#' Map layer that draws rasters. Supported visual variable is: \code{col} (the  color).
#' 
#' The visual variable arguments (e.g. \code{col}) can be specified with either a data variable name (of the object specified in \code{\link{tm_shape}}), or with a visual value (for \code{col}, a color is expected). Multiple values can be specified: in that case facets are created. These facets can be combined with other faceting data variables, specified with \code{\link{tm_facets}}.
#' 
#' The \code{.scale} arguments determine the used scale to map the data values to visual variable values. These can be specified with one of the available \code{tm_scale_} functions. The default scale that is used is specified by the tmap option \code{scales.var}.
#' 
#' The \code{.legend} arguments determine the used legend, specified with \code{\link{tm_legend}}. The default legend and its settings are determined by the tmap options \code{legend.}.
#' 
#' The \code{.free} arguments determine whether scales are applied freely across facets, or shared. A logical value is required. They can also be specified with a vector of three logical values; these determine whether scales are applied freely per facet dimension. This is only useful when facets are applied (see \code{\link{tm_facets}}). There are maximally three facet dimensions: rows, columns, and pages. This only applies for a facet grid (\code{\link{tm_facets_grid}}). For instance, \code{col.free = c(TRUE, FALSE, FALSE)} means that for the visual variable \code{col}, each row of facets will have its own scale, and therefore its own legend. For facet wraps and stacks (\code{\link{tm_facets_wrap}} and \code{\link{tm_facets_stack}}) there is only one facet dimension, so the \code{.free} argument requires only one logical value.
#' 
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color. See details.
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called. Not implemented yet.
#' @param group Name of the group to which this layer belongs. Not implemented yet.
#' @example ./examples/tm_raster.R 
#' @export
tm_raster = function(col = tm_shape_vars(),
					 col.scale = tm_scale(value.na = "#00000000"),
					 col.legend = tm_legend(),
					 col.free = NA,
					 col_alpha = tm_const(),
					 col_alpha.scale = tm_scale(),
					 col_alpha.legend = tm_legend(),
					 col_alpha.free = NA,
					 trans.args = list(),
					 mapping.args = list(),
					 zindex = NA,
					 group = NA) {
	
	tm_element_list(tm_element(
		layer = "raster",
		trans.fun = tmapTransRaster,
		trans.aes = list(),
		trans.args = trans.args,
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   				value = col_alpha,
						   				scale = col_alpha.scale,
						   				legend = col_alpha.legend,
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
		mapping.args = mapping.args,
		zindex = zindex,
		group = group,
		subclass = c("tm_aes_layer", "tm_layer")))
}



#' Map layer: RGB
#' 
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color.
#' @export
tm_rgb = function(col = tm_mv(1:3),
				  col.scale = tm_scale_rgb(),
				  col.legend = tm_legend(),
				  col.free = NA) {
	do.call(tm_raster, args = list(col = col, col.scale = col.scale, col.legend = col.legend, col.free = col.free))
}






#' Map layer: symbols
#' 
#' Map layer that draws symbols Supported visual variables are: \code{fill} (the fill color), \code{col} (the border color), \code{size} the symbol size, \code{shape} the symbol shape, \code{lwd} (line width), \code{lty} (line type), \code{fill_alpha} (fill color alpha transparency) and \code{col_alpha} (border color alpha transparency).
#' 
#' The visual variable arguments (e.g. \code{col}) can be specified with either a data variable name (of the object specified in \code{\link{tm_shape}}), or with a visual value (for \code{col}, a color is expected). Multiple values can be specified: in that case facets are created. These facets can be combined with other faceting data variables, specified with \code{\link{tm_facets}}.
#' 
#' The \code{.scale} arguments determine the used scale to map the data values to visual variable values. These can be specified with one of the available \code{tm_scale_} functions. The default scale that is used is specified by the tmap option \code{scales.var}.
#' 
#' The \code{.legend} arguments determine the used legend, specified with \code{\link{tm_legend}}. The default legend and its settings are determined by the tmap options \code{legend.}.
#' 
#' The \code{.free} arguments determine whether scales are applied freely across facets, or shared. A logical value is required. They can also be specified with a vector of three logical values; these determine whether scales are applied freely per facet dimension. This is only useful when facets are applied (see \code{\link{tm_facets}}). There are maximally three facet dimensions: rows, columns, and pages. This only applies for a facet grid (\code{\link{tm_facets_grid}}). For instance, \code{col.free = c(TRUE, FALSE, FALSE)} means that for the visual variable \code{col}, each row of facets will have its own scale, and therefore its own legend. For facet wraps and stacks (\code{\link{tm_facets_wrap}} and \code{\link{tm_facets_stack}}) there is only one facet dimension, so the \code{.free} argument requires only one logical value.
#' 
#' @param fill,fill.scale,fill.legend,fill.free Visual variable that determines the fill color. See details.
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color. See details.
#' @param size,size.scale,size.legend,size.free Visual variable that determines the size. See details.
#' @param shape,shape.scale,shape.legend,shape.free Visual variable that determines the shape. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.free Visual variable that determines the line type. See details.
#' @param fill_alpha,fill_alpha.scale,fill_alpha.legend,fill_alpha.free Visual variable that determines the fill color alpha transparency See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free Visual variable that determines the border color alpha transparency. See details.
#' @param linejoin,lineend line join and line end. See \code{\link[grid:gpar]{gpar}} for details.
#' @param plot.order Specification in which order the spatial features are drawn. See \code{\link{tm_plot_order}} for details.
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called. Not implemented yet.
#' @param group Name of the group to which this layer belongs. Not implemented yet.
#' @example ./examples/tm_symbols.R 
#' @export
tm_symbols = function(fill = tm_const(),
					  fill.scale = tm_scale(),
					  fill.legend = tm_legend(),
					  fill.free = NA,
					  col = tm_const(),
					  col.scale = tm_scale(),
					  col.legend = tm_legend(),
					  col.free = NA,
					  size = tm_const(),
					  size.scale = tm_scale(),
					  size.legend = tm_legend(),
					  size.free = NA,
					  shape = tm_const(),
					  shape.scale = tm_scale(),
					  shape.legend = tm_legend(),
					  shape.free = NA,
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
					  trans.args = list(),
					  mapping.args = list(),
					  zindex = NA,
					  group = NA) {
	
	tm_element_list(tm_element(
		layer = "symbols",
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.args = trans.args,
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free),
						   fill = tmapScale(aes = "fill",
											value = fill,
											scale = fill.scale,
											legend = fill.legend,
											free = fill.free),
						   size = tmapScale(aes = "size",
						   				value = size,
						   				scale = size.scale,
						   				legend = size.legend,
						   				free = size.free),
						   shape = tmapScale(aes = "shape",
						   				value = shape,
						   				scale = shape.scale,
						   				legend = shape.legend,
						   				free = shape.free),
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
		mapping.args = mapping.args,
		zindex = zindex,
		group = group,
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_bubbles = function(fill = tm_const(),
					  fill.scale = tm_scale(),
					  fill.legend = tm_legend(),
					  fill.free = NA,
					  col = tm_const(),
					  col.scale = tm_scale(),
					  col.legend = tm_legend(),
					  col.free = NA,
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
					  col_alpha = tm_const(),
					  col_alpha.scale = tm_scale(),
					  col_alpha.legend = tm_legend(),
					  col_alpha.free = NA,
					  plot.order = tm_plot_order("size"),
					  trans.args = list(),
					  mapping.args = list(),
					  zindex = NA,
					  group = NA) {
	
	tm_element_list(tm_element(
		layer = c("bubbles", "symbols"),
		trans.fun = tmapTransCentroid,
		trans.args = trans.args,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free),
						   fill = tmapScale(aes = "fill",
						   				 value = fill,
						   				 scale = fill.scale,
						   				 legend = fill.legend,
						   				 free = fill.free),
						   size = tmapScale(aes = "size",
						   				 value = size,
						   				 scale = size.scale,
						   				 legend = size.legend,
						   				 free = size.free),
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
						shape = 21,
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
		mapping.args = mapping.args,
		zindex = zindex,
		group = group,
		subclass = c("tm_aes_layer", "tm_layer")))
}

tm_dots = function(fill = tm_const(),
				   fill.scale = tm_scale(),
				   fill.legend = tm_legend(),
				   fill.free = NA,
				   col = tm_const(),
				   col.scale = tm_scale(),
				   col.legend = tm_legend(),
				   col.free = NA,
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
				   col_alpha = tm_const(),
				   col_alpha.scale = tm_scale(),
				   col_alpha.legend = tm_legend(),
				   col_alpha.free = NA,
				   plot.order = tm_plot_order("size"),
				   trans.args = list(),
				   mapping.args = list(),
				   zindex = NA,
				   group = NA) {
	
	tm_element_list(tm_element(
		layer = c("dots", "symbols"),
		trans.fun = tmapTransCentroid,
		trans.args = trans.args,
		trans.aes = list(),
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
										   value = col,
										   scale = col.scale,
										   legend = col.legend,
										   free = col.free),
						   fill = tmapScale(aes = "fill",
						   				 value = fill,
						   				 scale = fill.scale,
						   				 legend = fill.legend,
						   				 free = fill.free),
						   size = tmapScale(aes = "size",
						   				 value = size,
						   				 scale = size.scale,
						   				 legend = size.legend,
						   				 free = size.free),
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
						shape = 19,
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
		mapping.args = mapping.args,
		zindex = zindex,
		group = group,
		subclass = c("tm_aes_layer", "tm_layer")))
}


#' Map layer: lines
#' 
#' Map layer that draws symbols Supported visual variables are: \code{col} (the color), \code{lwd} (line width), \code{lty} (line type), and \code{col_alpha} (color alpha transparency).
#' 
#' The visual variable arguments (e.g. \code{col}) can be specified with either a data variable name (of the object specified in \code{\link{tm_shape}}), or with a visual value (for \code{col}, a color is expected). Multiple values can be specified: in that case facets are created. These facets can be combined with other faceting data variables, specified with \code{\link{tm_facets}}.
#' 
#' The \code{.scale} arguments determine the used scale to map the data values to visual variable values. These can be specified with one of the available \code{tm_scale_} functions. The default scale that is used is specified by the tmap option \code{scales.var}.
#' 
#' The \code{.legend} arguments determine the used legend, specified with \code{\link{tm_legend}}. The default legend and its settings are determined by the tmap options \code{legend.}.
#' 
#' The \code{.free} arguments determine whether scales are applied freely across facets, or shared. A logical value is required. They can also be specified with a vector of three logical values; these determine whether scales are applied freely per facet dimension. This is only useful when facets are applied (see \code{\link{tm_facets}}). There are maximally three facet dimensions: rows, columns, and pages. This only applies for a facet grid (\code{\link{tm_facets_grid}}). For instance, \code{col.free = c(TRUE, FALSE, FALSE)} means that for the visual variable \code{col}, each row of facets will have its own scale, and therefore its own legend. For facet wraps and stacks (\code{\link{tm_facets_wrap}} and \code{\link{tm_facets_stack}}) there is only one facet dimension, so the \code{.free} argument requires only one logical value.
#' 
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.free Visual variable that determines the line type. See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free Visual variable that determines the border color alpha transparency. See details.
#' @param linejoin,lineend line join and line end. See \code{\link[grid:gpar]{gpar}} for details.
#' @param plot.order Specification in which order the spatial features are drawn. See \code{\link{tm_plot_order}} for details.
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called. Not implemented yet.
#' @param group Name of the group to which this layer belongs. Not implemented yet.
#' @example ./examples/tm_lines.R 
#' @export
tm_lines = function(col = tm_const(),
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
					group = NA) {
	
	tm_element_list(tm_element(
		layer = "lines",
		trans.fun = tmapTransLines,
		trans.aes = list(),
		trans.args = trans.args,
		trans.isglobal = FALSE,
		mapping.aes = list(col = tmapScale(aes = "col",
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
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
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
		mapping.args = mapping.args,
		zindex = zindex,
		group = group,
		subclass = c("tm_aes_layer", "tm_layer")))
}



#' Map layer: text
#' 
#' Map layer that draws symbols Supported visual variables are: \code{text} (the text itself) \code{col} (color), \code{size} (font size), and \code{fontface} (font face).
#' 
#' The visual variable arguments (e.g. \code{col}) can be specified with either a data variable name (of the object specified in \code{\link{tm_shape}}), or with a visual value (for \code{col}, a color is expected). Multiple values can be specified: in that case facets are created. These facets can be combined with other faceting data variables, specified with \code{\link{tm_facets}}.
#' 
#' The \code{.scale} arguments determine the used scale to map the data values to visual variable values. These can be specified with one of the available \code{tm_scale_} functions. The default scale that is used is specified by the tmap option \code{scales.var}.
#' 
#' The \code{.legend} arguments determine the used legend, specified with \code{\link{tm_legend}}. The default legend and its settings are determined by the tmap options \code{legend.}.
#' 
#' The \code{.free} arguments determine whether scales are applied freely across facets, or shared. A logical value is required. They can also be specified with a vector of three logical values; these determine whether scales are applied freely per facet dimension. This is only useful when facets are applied (see \code{\link{tm_facets}}). There are maximally three facet dimensions: rows, columns, and pages. This only applies for a facet grid (\code{\link{tm_facets_grid}}). For instance, \code{col.free = c(TRUE, FALSE, FALSE)} means that for the visual variable \code{col}, each row of facets will have its own scale, and therefore its own legend. For facet wraps and stacks (\code{\link{tm_facets_wrap}} and \code{\link{tm_facets_stack}}) there is only one facet dimension, so the \code{.free} argument requires only one logical value.
#' 
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.free Visual variable that determines the line type. See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free Visual variable that determines the border color alpha transparency. See details.
#' @param linejoin,lineend line join and line end. See \code{\link[grid:gpar]{gpar}} for details.
#' @param plot.order Specification in which order the spatial features are drawn. See \code{\link{tm_plot_order}} for details.
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called. Not implemented yet.
#' @param group Name of the group to which this layer belongs. Not implemented yet.
#' @example ./examples/tm_lines.R 
#' @export
tm_text = function(text = tm_const(),
				   text.scale = tm_scale(),
				   text.legend = tm_legend(),
				   text.free = NA,
				   size = tm_const(),
				   size.scale = tm_scale(),
				   size.legend = tm_legend(),
				   size.free = NA,
				   col = tm_const(),
				   col.scale = tm_scale(),
				   col.legend = tm_legend(),
				   col.free = NA,
				   col_alpha = tm_const(),
				   col_alpha.scale = tm_scale(),
				   col_alpha.legend = tm_legend(),
				   col_alpha.free = NA,
				   fontface = tm_const(),
				   fontface.scale = tm_scale(),
				   fontface.legend = tm_legend(),
				   fontface.free = NA,
				   fontfamily = "",
				   shadow = FALSE,
				   plot.order = tm_plot_order("AREA", reverse = FALSE, na.order = "bottom"),
				   trans.args = list(),
				   mapping.args = list(clustering = FALSE),
				   zindex = NA,
				   group = NA,
				   ...) {
	
	#if (FALSE) {
	args = list(...)

	# dput(names(formals("tm_text")))
	v3 = c("root", "clustering", "size.lim", "sizes.legend", 
		   "sizes.legend.labels", "sizes.legend.text", "n", "style", "style.args", 
		   "as.count", "breaks", "interval.closure", "palette", "labels", 
		   "drop.levels", "labels.text", "midpoint", "stretch.palette", 
		   "contrast", "colorNA", "textNA", "showNA", "colorNULL", "fontface", 
		   "fontfamily", "alpha", "case", "shadow", "bg.color", "bg.alpha", 
		   "size.lowerbound", "print.tiny", "scale", "auto.placement", "remove.overlap", 
		   "along.lines", "overwrite.lines", "just", "xmod", "ymod", "title.size", 
		   "title.col", "legend.size.show", "legend.col.show", "legend.format", 
		   "legend.size.is.portrait", "legend.col.is.portrait", "legend.size.reverse", 
		   "legend.col.reverse", "legend.hist", "legend.hist.title", "legend.size.z", 
		   "legend.col.z", "legend.hist.z", "id", "auto.palette.mapping", 
		   "max.categories")
	
	
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
		} else if (style %in% c("fixed", "sd", "equal", "pretty", "quantile", "kmeans", "hclust", "bclust", "fisher", "jenks", "dpih", "headtails")) {
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
						   				free = text.free),
						   size = tmapScale(aes = "size",
						   				value = size,
						   				scale = size.scale,
						   				legend = size.legend,
						   				free = size.free),
						   col = tmapScale(aes = "col",
						   				value = col,
						   				scale = col.scale,
						   				legend = col.legend,
						   				free = col.free),
						   col_alpha = tmapScale(aes = "col_alpha",
						   					  value = col_alpha,
						   					  scale = col_alpha.scale,
						   					  legend = col_alpha.legend,
						   					  free = col_alpha.free),
						   fontface = tmapScale(aes = "fontface",
						   				value = fontface,
						   				scale = fontface.scale,
						   				legend = fontface.legend,
						   				free = fontface.free)),
		
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
						shadow = shadow),
		tpar = tmapTpar(),
		plot.order = plot.order,
		mapping.fun = "Text",
		mapping.args = mapping.args,
		zindex = zindex,
		group = group,
		subclass = c("tm_aes_layer", "tm_layer")))
}
