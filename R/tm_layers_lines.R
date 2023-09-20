#' Map layer: lines
#' 
#' Map layer that draws symbols Supported visual variables are: `col` (the color), `lwd` (line width), `lty` (line type), and `col_alpha` (color alpha transparency).
#' 
#' The visual variable arguments (e.g. `col`) can be specified with either a data variable name (of the object specified in [tm_shape()]), or with a visual value (for `col`, a color is expected). Multiple values can be specified: in that case facets are created. These facets can be combined with other faceting data variables, specified with [tm_facets()].
#' 
#' The `.scale` arguments determine the used scale to map the data values to visual variable values. These can be specified with one of the available `tm_scale_` functions. The default scale that is used is specified by the tmap option `scales.var`.
#' 
#' The `.legend` arguments determine the used legend, specified with [tm_legend()]. The default legend and its settings are determined by the tmap options `legend.`.
#' 
#' The `.free` arguments determine whether scales are applied freely across facets, or shared. A logical value is required. They can also be specified with a vector of three logical values; these determine whether scales are applied freely per facet dimension. This is only useful when facets are applied (see [tm_facets()]). There are maximally three facet dimensions: rows, columns, and pages. This only applies for a facet grid ([tm_facets_grid()]). For instance, `col.free = c(TRUE, FALSE, FALSE)` means that for the visual variable `col`, each row of facets will have its own scale, and therefore its own legend. For facet wraps and stacks ([tm_facets_wrap()] and [tm_facets_stack()]) there is only one facet dimension, so the `.free` argument requires only one logical value.
#' 
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color. See details.
#' @param lwd,lwd.scale,lwd.legend,lwd.free Visual variable that determines the line width. See details.
#' @param lty,lty.scale,lty.legend,lty.free Visual variable that determines the line type. See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free Visual variable that determines the border color alpha transparency. See details.
#' @param linejoin,lineend line join and line end. See [gpar()][grid::gpar()] for details.
#' @param plot.order Specification in which order the spatial features are drawn. See [tm_plot_order()] for details.
#' @param trans.args,mapping.args lists that are passed on to internal transformation and mapping functions respectively
#' @param zindex Map layers are drawn on top of each other. The `zindex` numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups can be switched on and off. Options: `"radio"` for radio buttons (meaning only one group can be shown), `"check"` for check boxes (so multiple groups can be shown), and `"none"` for no control (the group cannot be (de)selected).
#' @param popup.vars names of data variables that are shown in the popups in `"view"` mode. Set popup.vars to `TRUE` to show all variables in the shape object. Set popup.vars to `FALSE` to disable popups. Set popup.vars to a character vector of variable names to those those variables in the popups. The default (`NA`) depends on whether visual variables (e.g.`col`) are used. If so, only those are shown. If not all variables in the shape object are shown.
#' @param popup.format list of formatting options for the popup values. See the argument `legend.format` for options. Only applicable for numeric data variables. If one list of formatting options is provided, it is applied to all numeric variables of `popup.vars`. Also, a (named) list of lists can be provided. In that case, each list of formatting options is applied to the named variable.
#' @param hover name of the data variable that specifies the hover labels
#' 
#' @param id name of the data variable that specifies the indices of the spatial features. Only used for `"view"` mode.
#' @param ... to catch deprecated arguments from version < 4.0
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
					plot.order = tm_plot_order("LENGTH", reverse = FALSE, na.order = "bottom"),
					trans.args = list(lines.only = "ifany"),
					mapping.args = list(),
					zindex = NA,
					group = NA,
					group.control = "check",
					popup.vars = NA,
					popup.format = list(),
					hover = "",
					id = "",
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
		popup.vars = popup.vars,
		popup.format = popup.format,
		hover = hover,
		id = id,
		subclass = c("tm_aes_layer", "tm_layer")))
}
