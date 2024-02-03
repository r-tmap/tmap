#' Map layer: raster
#' 
#' Map layer that draws rasters. Supported visual variable is: `col` (the  color).
#' 
#' The visual variable arguments (e.g. `col`) can be specified with either a data
#' variable name (of the object specified in [tm_shape()]), or with a visual value
#' (for `col`, a color is expected). Multiple values can be specified: in that
#' case facets are created. These facets can be combined with other faceting data
#' variables, specified with [tm_facets()].
#' 
#' * The `.scale` arguments determine the used scale to map the data values to
#'   visual variable values. These can be specified with one of the available
#'   `tm_scale_()` functions. The default scale that is used is specified by the
#'   tmap option `scales.var`.
#' 
#' * The `.legend` arguments determine the used legend, specified with [tm_legend()].
#'   The default legend and its settings are determined by the tmap options `legend.`.
#' 
#' * The `.free` arguments determine whether scales are applied freely across
#'   facets, or shared. A logical value is required. They can also be specified
#'   with a vector of three logical values; these determine whether scales are applied
#'   freely per facet dimension. This is only useful when facets are applied (see [tm_facets()]).
#'   There are maximally three facet dimensions: rows, columns, and pages.
#'   This only applies for a facet grid ([tm_facets_grid()]).
#'   For instance, `col.free = c(TRUE, FALSE, FALSE)` means that for the visual
#'   variable `col`, each row of facets will have its own scale, and therefore its
#'   own legend. For facet wraps and stacks ([tm_facets_wrap()] and [tm_facets_stack()]),
#'   there is only one facet dimension, so the `.free` argument requires only one logical value.
#' 
#' @param col,col.scale,col.legend,col.free Visual variable that determines the color.
#'   See details.
#' @param col_alpha,col_alpha.scale,col_alpha.legend,col_alpha.free Visual variable
#'   that determines the alpha transparency. See details.
#' @param trans.args,mapping.args lists that are passed on to internal transformation
#'   and mapping functions respectively.
#' @param zindex Map layers are drawn on top of each other.
#'   The `zindex` numbers (one for each map layer) determines the stacking order.
#'   By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only
#'   relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups
#'   can be switched on and off. Options: `"radio"` for radio buttons (meaning only
#'   one group can be shown), `"check"` for check boxes (so multiple groups can be
#'   shown), and `"none"` for no control (the group cannot be (de)selected).
#' @param ... to catch deprecated arguments from version < 4.0
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
					 group = NA,
					 group.control = "check",
					 ...) {
	
	
	args = list(...)
	args_called = as.list(match.call()[-1]) #lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	
	v3 = c("alpha", "palette", "n", "style", "style.args", "as.count", 
		   "breaks", "interval.closure", "labels", "drop.levels", "midpoint", 
		   "stretch.palette", "contrast", "saturation", "interpolate", "colorNA", 
		   "textNA", "showNA", "colorNULL", "title", "legend.show", "legend.format", 
		   "legend.is.portrait", "legend.reverse", "legend.hist", "legend.hist.title", 
		   "legend.z", "legend.hist.z", "auto.palette.mapping", 
		   "max.categories", "max.value")

	if (any(v3 %in% names(args))) {
		message("tm_raster: Deprecated tmap v3 code detected. Code translated to v4")
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
							   label.na = imp("textNA", NA), 
							   label.null = NA, 
							   label.format = imp("legend.format", list()))
		col.scale.args$fun_pref = if (style == "cat") {
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
		
		col.scale = do.call("tm_scale", args = col.scale.args)		
		
		if ("alpha" %in% names(args)) {
			col_alpha = args$alpha
		}
		
		col.legend.args = alist(title = imp("title", NA),
								 show = imp("legend.show", NULL),
								 na.show = imp("na.show", NA),
								 format = imp("legend.format", list()),
								 orientation = ifelse(imp("legend.is.portrait", TRUE), "portrait", "landscape"),
								 reverse = imp("legend.reverse", FALSE))
		
		col.legend = do.call("tm_legend", col.legend.args)
	}
	
	
	
	
	
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
		group.control = group.control,
		popup.vars = FALSE,
		popup.format = list(),
		hover = "",
		id = "",
		subclass = c("tm_aes_layer", "tm_layer")))
}
