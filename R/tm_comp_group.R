#' Group components
#'
#' @param id id of the component group. Refers to the `group_id` argument of each component function, such as [tm_legend()] and [tm_title()].
#' @param stack stacking Horizontal or vertical
#' @param frame_combine put frame around all components that are drawn on the same location. Whether a frame is drawn is still decided by the `frame` argument of the  'main' (first) component.
#' @param resize_as_group in case a component if rescaled because of the limited space, rescale the other components proportionally?
#' @param stack_margin Margin between components
#' @param offset Offset margin between frame and the components block
#' @param frame Frane of the components. Is usually set in each component function, but if specified here, it will overwrite them.
#' @param bg.color Background color the components block. Is usually set in each component function, but if specified here, it will overwrite them.
#' @param bg.alpha Background alpha transparency of the components block. Is usually set in each component function, but if specified here, it will overwrite them.
#' @return A [`tmap-element`]
#' @export
tm_comp_group = function(
		id,
		position,
		stack,     # was 'stack' in each tm_legend or tm_<comp> function
		frame_combine,
		resize_as_group,        # from tm_legend/tm_<comp>.
		stack_margin,     # margin between components
		offset,            # offset margin

		# also: arguments from tm_legend/tm_<comp>, as 'apply-to-all' components.
		frame ,
		bg.color,
		bg.alpha) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	optname = paste0("component_", id)
	x = structure(list(args), names= optname)
	do.call(tm_options, x)
}
