#' Layout options
#' 
#' Set of tmap options that are directly related to the layout.
#' 
#' @param scale TODO
#' @param asp TODO
#' @param bg.color TODO
#' @param outer.bg.color TODO
#' @param frame TODO
#' @param frame.lwd TODO
#' @param frame.r TODO
#' @param frame.double.line TODO
#' @param outer.margins TODO
#' @param inner.margins TODO
#' @param inner.margins.extra TODO
#' @param meta.margins TODO
#' @param meta.auto.margins TODO
#' @param between.margin TODO
#' @param component.offset TODO
#' @param component.stack.margin TODO
#' @param grid.mark.height TODO
#' @param xylab.height TODO
#' @param coords.height TODO
#' @param xlab.show TODO
#' @param ylab.show TODO
#' @param xlab.pos TODO
#' @param ylab.pos TODO
#' @param grid.show TODO
#' @param grid.label.pos TODO
#' @param panel.type TODO
#' @param panel.wrap.pos TODO
#' @param panel.xtab.pos TODO
#' @param unit TODO
#' @param color.sepia.intensity TODO
#' @param color.saturation TODO
#' @param color.vision.deficiency.sim TODO
#' @param fontface TODO
#' @param fontfamily TODO
#' @param legend.show TODO
#' @param legend.design TODO
#' @param legend.orientation TODO
#' @param legend.position TODO
#' @param legend.width TODO
#' @param legend.height TODO
#' @param legend.stack TODO
#' @param legend.group.frame TODO
#' @param legend.resize.as.group TODO
#' @param legend.reverse TODO
#' @param legend.title.color TODO
#' @param legend.title.size TODO
#' @param legend.title.fontface TODO
#' @param legend.title.fontfamily TODO
#' @param legend.title.just TODO
#' @param legend.text.color TODO
#' @param legend.text.size TODO
#' @param legend.text.fontface TODO
#' @param legend.text.fontfamily TODO
#' @param legend.frame TODO
#' @param legend.frame.lwd TODO
#' @param legend.frame.r TODO
#' @param legend.bg.color TODO
#' @param legend.bg.alpha TODO
#' @param compass.type TODO
#' @param title.size TODO
#' @param title.color TODO
#' @param title.fontface TODO
#' @param title.fontfamily TODO
#' @param title.bg.color TODO
#' @param title.bg.alpha TODO
#' @param title.padding TODO
#' @param title.frame TODO
#' @param title.frame.lwd TODO
#' @param title.frame.r TODO
#' @param title.stack TODO
#' @param title.position TODO
#' @param title.width TODO
#' @param title.heigth TODO
#' @param title.group.frame TODO
#' @param title.resize.as.group TODO
#' @param panel.show TODO
#' @param panel.labels TODO
#' @param panel.label.size TODO
#' @param panel.label.color TODO
#' @param panel.label.fontface TODO
#' @param panel.label.fontfamily TODO
#' @param panel.label.bg.color TODO
#' @param panel.label.height TODO
#' @param panel.label.rot TODO
#' @rdname tm_layout
#' @example ./examples/tm_layout.R 
#' @export 
tm_layout = function(
		title = NULL,
	scale,
	asp,
	
	# background
	bg.color,
	outer.bg.color,
	
	# frame
	frame,
	frame.lwd,
	frame.r,
	frame.double.line,
	
	
	# margins	
	outer.margins,
	inner.margins,
	inner.margins.extra,
	meta.margins,
	meta.auto.margins,
	between.margin,
	component.offset,
	component.stack.margin,
	grid.mark.height,
	xylab.height,
	coords.height,
	
	# xlab, ylab, grid
	xlab.show,
	ylab.show,
	xlab.pos,
	ylab.pos,
	grid.show,
	grid.label.pos,
	
	# panel
	panel.type,
	panel.wrap.pos,
	panel.xtab.pos,
	
	# data
	unit,
	
	# colors
	color.sepia.intensity,
	color.saturation,
	color.vision.deficiency.sim,
	
	# text
	fontface,
	fontfamily,
	
	# legend		
	legend.show,
	legend.design,
	legend.orientation,
	legend.position,
	legend.width,
	legend.height,
	legend.stack,
	legend.group.frame,
	legend.resize.as.group,
	legend.reverse,
	legend.title.color,
	legend.title.size,
	legend.title.fontface,
	legend.title.fontfamily,
	legend.title.just,
	legend.text.color,
	legend.text.size,
	legend.text.fontface,
	legend.text.fontfamily,
	legend.frame,
	legend.frame.lwd,
	legend.frame.r,
	legend.bg.color,
	legend.bg.alpha,

	# components
	compass.type,
	title.size,
	title.color,
	title.fontface,
	title.fontfamily,
	title.bg.color,
	title.bg.alpha,
	title.padding,
	title.frame,
	title.frame.lwd,
	title.frame.r,
	title.stack,
	title.position,
	title.width,
	title.heigth,
	title.group.frame,
	title.resize.as.group,
	
	panel.show,
	panel.labels,
	panel.label.size,
	panel.label.color,
	panel.label.fontface,
	panel.label.fontfamily,
	panel.label.bg.color,
	panel.label.height,
	panel.label.rot) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!is.null(title)) {
		title.args = args[substr(names(args), 1, 5) == "title"]
		title.args$title = NULL
		names(title.args) = substr(names(title.args), 7, nchar(names(title.args)))
		warning("The 'title' argument of tm_layout is deprecated as of tmap 4.0. Please use tm_title instead.", call. = FALSE)
		args$title = NULL
		do.call(tm_options, args) + do.call(tm_title, c(list(title = title), title.args))
	} else {
		do.call(tm_options, args)
	}
}

tm_view = function(use.WebGL,
				   legend.position,
				   control.position, 
				   leaflet.options) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	do.call(tm_options, args)
}

tm_plot = function(use.gradient) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	do.call(tm_options, args)
}

