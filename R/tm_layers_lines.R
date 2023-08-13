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
#' @param trans.args,mapping.args lists that are passed on to internal transformation and mapping functions respectively
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups can be switched on and off. Options: `"radio"` for radio buttons (meaning only one group can be shown), `"check"` for check boxes (so multiple groups can be shown), and `"none"` for no control (the group cannot be (de)selected).
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
					group = NA,
					group.control = "check",
					...) {
	
	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	v3 = c("alpha", "scale", "lwd.legend.labels", "lwd.legend.col", "n", 
		   "style", "style.args", "as.count", "breaks", "interval.closure", 
		   "palette", "labels", "drop.levels", "midpoint", "stretch.palette", 
		   "contrast", "colorNA", "textNA", "showNA", "colorNULL", "title.col", 
		   "title.lwd", "legend.col.show", "legend.lwd.show", "legend.format", 
		   "legend.col.is.portrait", "legend.lwd.is.portrait", "legend.col.reverse", 
		   "legend.lwd.reverse", "legend.hist", "legend.hist.title", "legend.col.z", 
		   "legend.lwd.z", "legend.hist.z", "id", "interactive", "popup.vars", 
		   "popup.format", "auto.palette.mapping", "max.categories")
	
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
		
		# v3 visual variable: col
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
							   label.na = imp("textNA", NA), 
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
		
		if ("alpha" %in% names(args)) {
			col_alpha = args$alpha
		}
		
		col.legend.args = alist(title = imp("title.col", NA),
								 show = imp("legend.col.show", NULL),
								 na.show = imp("showNA", NA),
								 format = imp("legend.format", list()),
								 orientation = ifelse(imp("legend.col.is.portrait", TRUE), "portrait", "landscape"),
								 reverse = imp("legend.col.reverse", FALSE))
		
		col.legend = do.call("tm_legend", col.legend.args)
		
		
		# v3 visual variable: lwd
		lwd.scale.args = list(values = imp("lwd.legend", NA),
							  labels = imp("lwd.legend.labels", NULL),
							  fun_pref = "continuous")

		lwd.scale = do.call("tm_scale", args = lwd.scale.args)		
		

		lwd.legend.args = alist(title = imp("title.lwd", NA),
								show = imp("legend.lwd.show", NULL),
								na.show = imp("showNA", NA),
								format = imp("legend.format", list()),
								orientation = ifelse(imp("legend.lwd.is.portrait", TRUE), "portrait", "landscape"),
								reverse = imp("legend.lwd.reverse", FALSE))
		
		lwd.legend = do.call("tm_legend", lwd.legend.args)
		
	}
	
	

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
		group.control = group.control,
		subclass = c("tm_aes_layer", "tm_layer")))
}
