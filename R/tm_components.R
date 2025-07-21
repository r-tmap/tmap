#' Group components
#'
#' @param group_id id of the component group. By default set to `""`, which will apply to all components. There are two other options. 1) To use the same (self-chosen) name that corresponds to the `group_id` argument of a component function, such as [tm_legend()] and [tm_title()]. 2) To specify one (or more) component function names, e.g. `"tm_legend"` or `c("tm_scalebar", "tm_compass")`.
#' @param position The position specification of the components in this group: an object created with `tm_pos_in()` or `tm_pos_out()`. Or, as a shortcut, a vector of two values, specifying the x and y coordinates. The first is `"left"`, `"center"` or `"right"` (or upper case, meaning tighter to the map frame), the second `"top"`, `"center"` or `"bottom"`. Numeric values are also supported, where 0, 0 means left bottom and 1, 1 right top. See also \href{https://r-tmap.github.io/tmap/articles/adv_positions}{vignette about positioning}.
#' @param stack stacking `"horizontal"` or `"vertical"`
#' @param frame_combine put frame around all components that are drawn on the same location. Whether a frame is drawn is still decided by the `frame` argument of the  'main' (first) component.
#' @param equalize in case `frame_combine` is `FALSE`, should the separate frames be equalized, i.e. have the same width (when stacked vertically) or height (when stacked horizontally)?
#' @param resize_as_group in case a component if rescaled because of the limited space, rescale the other components proportionally?
#' @param stack_margin Margin between components
#' @param offset Offset margin between frame and the components block
#' @param frame Should a frame be drawn? By default `TRUE` for legends, charts and insets, and `FALSE` otherwise.
#' @param frame.color frame color
#' @param frame.alpha frame alpha transparancy
#' @param frame.lwd frame line width
#' @param frame.r Radius of the rounded frame corners. 0 means no rounding.
#' @param bg Background color the components block. Is usually set in each component function, but if specified here, it will overwrite them.
#' @param bg.color Background color the components block. Is usually set in each component function, but if specified here, it will overwrite them.
#' @param bg.alpha Background alpha transparency of the components block. Is usually set in each component function, but if specified here, it will overwrite them.
#' @return A [`tmap-element`]
#' @export
tm_components = function(
		group_id = "",
		position,
		stack,     # was 'stack' in each tm_legend or tm_<comp> function
		frame_combine,
		equalize,
		resize_as_group,        # from tm_legend/tm_<comp>.
		stack_margin,     # margin between components
		offset,            # offset margin
		frame ,
		frame.color,
		frame.alpha,
		frame.lwd,
		frame.r,
		bg,
		bg.color,
		bg.alpha) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())
	args$called = names(args)
	optname = paste0("component_", paste(group_id, collapse = "^"))
	x = structure(list(args), names= optname)
	do.call(tm_options, x)
}
