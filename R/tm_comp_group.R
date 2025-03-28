tm_comp_group = function(
		pos.h, pos.v,
		stack,     # was 'stack' in each tm_legend or tm_<comp> function
		combine,
		resize_as_group,        # from tm_legend/tm_<comp>.
		stack_margin,     # margin between components
		offset,            # offset margin

		# also: arguments from tm_legend/tm_<comp>, as 'apply-to-all' components.
		frame ,
		bg.color,
		bg.alpha) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	name = paste(pos.h, pos.v, sep = "_")
	optname = paste0("component_", name)
	x = structure(list(args), names= optname)
	do.call(tm_options, x)
}
