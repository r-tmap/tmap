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
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups can be switched on and off. Options: `"radio"` for radio buttons (meaning only one group can be shown), `"check"` for check boxes (so multiple groups can be shown), and `"none"` for no control (the group cannot be (de)selected).
#' @example ./examples/tm_symbols.R 
#' @export
#' @name tm_symbols
#' @rdname tm_symbols
tm_symbols = function(size = tm_const(),
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
					  mapping.args = list(icon.scale = 3,
					  					just = NA,
					  					grob.dim = c(width=48, height=48, render.width=256, render.height=256)),
					  zindex = NA,
					  group = NA,
					  group.control = "check",
					  ...) {
	
	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	res = v3_symbols(args, args_called)
	if (!is.null(res)) {
		fill = res$fill
		col = res$col
		fill_alpha = res$fill_alpha
		fill.scale = res$fill.scale
		fill.legend = res$fill.legend
		size.scale = res$size.scale
		size.legend = res$size.legend
		shape.scale = res$shape.scale
		shape.legend = res$shape.legend
	}
	

	tm_element_list(tm_element(
		layer = "symbols",
		trans.fun = tmapTransCentroid,
		trans.aes = list(),
		trans.args = trans.args,
		trans.isglobal = FALSE,
		mapping.aes = list(size = tmapScale(aes = "size",
											value = size,
											scale = size.scale,
											legend = size.legend,
											free = size.free),
						   
						   fill = tmapScale(aes = "fill",
						   				 value = fill,
						   				 scale = fill.scale,
						   				 legend = fill.legend,
						   				 free = fill.free),
						   col = tmapScale(aes = "col",
						   				value = col,
						   				scale = col.scale,
						   				legend = col.legend,
						   				free = col.free),
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
		group.control = group.control,
		subclass = c("tm_aes_layer", "tm_layer")))
}

v3_symbols = function(args, args_called) {
	v3 = c("alpha", "border.col", "border.lwd", "border.alpha", "scale", 
		   "perceptual", "clustering", "size.max", "size.lim", "sizes.legend", 
		   "sizes.legend.labels", "n", "style", "style.args", "as.count", 
		   "breaks", "interval.closure", "palette", "labels", "drop.levels", 
		   "midpoint", "stretch.palette", "contrast", "colorNA", "textNA", 
		   "showNA", "colorNULL", "shapes", "shapes.legend", "shapes.legend.fill", 
		   "shapes.labels", "shapes.drop.levels", "shapeNA", "shape.textNA", 
		   "shape.showNA", "shapes.n", "shapes.style", "shapes.style.args", 
		   "shapes.as.count", "shapes.breaks", "shapes.interval.closure", 
		   "legend.max.symbol.size", "just", "jitter", "xmod", "ymod", "icon.scale", 
		   "grob.dim", "title.size", "title.col", "title.shape", "legend.size.show", 
		   "legend.col.show", "legend.shape.show", "legend.format", "legend.size.is.portrait", 
		   "legend.col.is.portrait", "legend.shape.is.portrait", "legend.size.reverse", 
		   "legend.col.reverse", "legend.shape.reverse", "legend.hist", 
		   "legend.hist.title", "legend.size.z", "legend.col.z", "legend.shape.z", 
		   "legend.hist.z", "id", "interactive", "popup.vars", "popup.format", 
		   "auto.palette.mapping", "max.categories")
	
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
		
		# v3 visual variable: fill
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
			fill = args_called$col
			col = tm_const()
		} else {
			fill = tm_const()
			col = tm_const()
		}
		if ("border.col" %in% names(args)) {
			col = args$border.col
		}
		
		
		if ("alpha" %in% names(args)) {
			fill_alpha = args$alpha
		} else {
			fill_alpha = tm_const()
		}
		
		fill.legend.args = alist(title = imp("title.col", NA),
								 show = imp("legend.show", NULL),
								 na.show = imp("na.show", NA),
								 format = imp("legend.format", list()),
								 orientation = ifelse(imp("legend.is.portrait", TRUE), "portrait", "landscape"),
								 reverse = imp("legend.reverse", FALSE))
		
		fill.legend = do.call("tm_legend", fill.legend.args)
		
		
		# v3 visual variable: size
		size.scale.args = list(ticks = imp("sizes.legend", NULL),
							   value.range = {if ("size.lim" %in% names(args)) {
							   	c(args[["size.lim"]][1] / args[["size.lim"]][2], 1)
							   }},
							   limits = imp("size.lim", NULL),
							   outliers.trunc = c(TRUE, FALSE),
							   labels = imp("sizes.legend.labels", NULL),
							   fun_pref = "continuous")
		
		size.scale = do.call("tm_scale", args = size.scale.args)		
		
		
		size.legend.args = alist(title = imp("title.size", NA),
								 show = imp("legend.size.show", NULL),
								 na.show = imp("showNA", NA),
								 format = imp("legend.format", list()),
								 orientation = ifelse(imp("legend.size.is.portrait", FALSE), "portrait", "landscape"),
								 reverse = imp("legend.size.reverse", FALSE))
		
		size.legend = do.call("tm_legend", size.legend.args)
		
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
		
		shape.scale.args = list(n = imp("shapes.n", 5), 
								style = shapes.style, 
								style.args = imp("shapes.style.args", list()), 
								breaks = imp("shapes.breaks", NULL), 
								interval.closure = imp("shapes.interval.closure", "left"), 
								drop.levels = imp("drop.levels", FALSE),
								midpoint = imp("midpoint", NULL), 
								as.count = imp("as.count", NA), 
								values = imp("shapes", 21:25),
								labels = imp("shapes.labels", NULL), 
								label.na = imp("shape.textNA", NA), 
								label.null = NA, 
								label.format = imp("legend.format", list()),
								fun_pref = "intervals")
		
		shape.scale = do.call("tm_scale", args = shape.scale.args)		
		
		
		shape.legend.args = alist(title = imp("title.shape", NA),
								 show = imp("legend.shape.show", NULL),
								 na.show = imp("shape.showNA ", NA),
								 format = imp("legend.format", list()),
								 orientation = ifelse(imp("legend.shape.is.portrait", TRUE), "portrait", "landscape"),
								 reverse = imp("legend.shape.reverse", FALSE))
		
		shape.legend = do.call("tm_legend", shape.legend.args)
		
		
		list(fill = fill,
			 col = col,
			 fill_alpha = fill_alpha,
			 fill.scale = fill.scale,
			 fill.legend = fill.legend,
			 size.scale = size.scale,
			 size.legend = size.legend,
			 shape.scale = shape.scale,
			 shape.legend = shape.legend)
	} else {
		NULL
	}
}


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
					  trans.args = list(),
					  mapping.args = list(),
					  zindex = NA,
					  group = NA,
					  group.control = "check",
					  ...) {
	
	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	res = v3_symbols(args, args_called)
	if (!is.null(res)) {
		fill = res$fill
		col = res$col
		fill_alpha = res$fill_alpha
		fill.scale = res$fill.scale
		fill.legend = res$fill.legend
		size.scale = res$size.scale
		size.legend = res$size.legend
		shape.scale = res$shape.scale
		shape.legend = res$shape.legend
	}
	
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
		group.control = group.control,
		subclass = c("tm_aes_layer", "tm_layer")))
}

#' @export
#' @name tm_dots
#' @rdname tm_symbols
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
				   group = NA,
				   group.control = "check") {
	
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
		group.control = group.control,
		subclass = c("tm_aes_layer", "tm_layer")))
}