#' Map layer: text
#' 
#' Map layer that draws symbols Supported visual variables are: `text` (the text itself) `col` (color), `size` (font size), and `fontface` (font face).
#' 
#' The visual variable arguments (e.g. `col`) can be specified with either a data variable name (of the object specified in [tm_shape()]), or with a visual value (for `col`, a color is expected). Multiple values can be specified: in that case facets are created. These facets can be combined with other faceting data variables, specified with [tm_facets()].
#' 
#' The `.scale` arguments determine the used scale to map the data values to visual variable values. These can be specified with one of the available `tm_scale_` functions. The default scale that is used is specified by the tmap option `scales.var`.
#' 
#' The `.legend` arguments determine the used legend, specified with [tm_legend()]. The default legend and its settings are determined by the tmap options `legend.`.
#' 
#' The `.free` arguments determine whether scales are applied freely across facets, or shared. A logical value is required. They can also be specified with a vector of three logical values; these determine whether scales are applied freely per facet dimension. This is only useful when facets are applied (see [tm_facets()]). There are maximally three facet dimensions: rows, columns, and pages. This only applies for a facet grid ([tm_facets_grid()]). For instance, `col.free = c(TRUE, FALSE, FALSE)` means that for the visual variable `col`, each row of facets will have its own scale, and therefore its own legend. For facet wraps and stacks ([tm_facets_wrap()] and [tm_facets_stack()]) there is only one facet dimension, so the `.free` argument requires only one logical value.
#' 
#' @param text,text.scale,text.legend,text.free Visual variable that determines the text. See details.
#' @param size,size.scale,size.legend,size.free Visual variable that determines the font size. See details.
#' @param col,col.scale,col.legend,col.free Visual variable that determines the col color. See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free Visual variable that determines the border color alpha transparency. See details.
#' @param fontface,fontface.scale,fontface.legend,fontface.free Visual variable that determines the font face. See details.
#' @param fontfamily The font family. See [grid::gpar()] for details.
#' @param shadow Shadow behind the text. Logical or color.
#' @param plot.order Specification in which order the spatial features are drawn. See [tm_plot_order()] for details.
#' @param trans.args,mapping.args lists that are passed on to internal transformation and mapping functions respectively
#' @param zindex Map layers are drawn on top of each other. The `zindex` numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups can be switched on and off. Options: `"radio"` for radio buttons (meaning only one group can be shown), `"check"` for check boxes (so multiple groups can be shown), and `"none"` for no control (the group cannot be (de)selected).
#' @param ... to catch deprecated arguments from version < 4.0
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
				   trans.args = list(points.only = "ifany"),
				   mapping.args = list(clustering = FALSE),
				   zindex = NA,
				   group = NA,
				   group.control = "check",
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
		group.control = group.control,
		popup.vars = FALSE,
		popup.format = list(),
		hover = "",
		id = "",
		subclass = c("tm_aes_layer", "tm_layer")))
}
