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
#' @param zindex Map layers are drawn on top of each other. The \code{zindex} numbers (one for each map layer) determines the stacking order. By default the map layers are drawn in the order they are called.
#' @param group Name of the group to which this layer belongs. This is only relevant in view mode, where layer groups can be switched (see `group.control`)
#' @param group.control In view mode, the group control determines how layer groups can be switched on and off. Options: `"radio"` for radio buttons (meaning only one group can be shown), `"check"` for check boxes (so multiple groups can be shown), and `"none"` for no control (the group cannot be (de)selected).
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
					 group.control = "check") {
	
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
		subclass = c("tm_aes_layer", "tm_layer")))
}
