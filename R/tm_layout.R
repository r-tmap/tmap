#' Layout options
#' 
#' Set of tmap options that are directly related to the layout.
#' 
#' @param scale PARAM_DESCRIPTION
#' @param asp PARAM_DESCRIPTION
#' @param bg.color PARAM_DESCRIPTION
#' @param outer.bg.color PARAM_DESCRIPTION
#' @param frame PARAM_DESCRIPTION
#' @param frame.lwd PARAM_DESCRIPTION
#' @param frame.r PARAM_DESCRIPTION
#' @param frame.double.line PARAM_DESCRIPTION
#' @param outer.margins PARAM_DESCRIPTION
#' @param inner.margins PARAM_DESCRIPTION
#' @param inner.margins.extra PARAM_DESCRIPTION
#' @param meta.margins PARAM_DESCRIPTION
#' @param meta.auto.margins PARAM_DESCRIPTION
#' @param between.margin PARAM_DESCRIPTION
#' @param component.offset PARAM_DESCRIPTION
#' @param component.stack.margin PARAM_DESCRIPTION
#' @param grid.mark.height PARAM_DESCRIPTION
#' @param xylab.height PARAM_DESCRIPTION
#' @param coords.height PARAM_DESCRIPTION
#' @param xlab.show PARAM_DESCRIPTION
#' @param ylab.show PARAM_DESCRIPTION
#' @param xlab.pos PARAM_DESCRIPTION
#' @param ylab.pos PARAM_DESCRIPTION
#' @param grid.show PARAM_DESCRIPTION
#' @param grid.label.pos PARAM_DESCRIPTION
#' @param panel.type PARAM_DESCRIPTION
#' @param panel.wrap.pos PARAM_DESCRIPTION
#' @param panel.xtab.pos PARAM_DESCRIPTION
#' @param unit PARAM_DESCRIPTION
#' @param color.sepia.intensity PARAM_DESCRIPTION
#' @param color.saturation PARAM_DESCRIPTION
#' @param color.vision.deficiency.sim PARAM_DESCRIPTION
#' @param fontface PARAM_DESCRIPTION
#' @param fontfamily PARAM_DESCRIPTION
#' @param legend.show PARAM_DESCRIPTION
#' @param legend.design PARAM_DESCRIPTION
#' @param legend.orientation PARAM_DESCRIPTION
#' @param legend.position PARAM_DESCRIPTION
#' @param legend.width PARAM_DESCRIPTION
#' @param legend.height PARAM_DESCRIPTION
#' @param legend.stack PARAM_DESCRIPTION
#' @param legend.group.frame PARAM_DESCRIPTION
#' @param legend.resize.as.group PARAM_DESCRIPTION
#' @param legend.reverse PARAM_DESCRIPTION
#' @param legend.title.color PARAM_DESCRIPTION
#' @param legend.title.size PARAM_DESCRIPTION
#' @param legend.title.fontface PARAM_DESCRIPTION
#' @param legend.title.fontfamily PARAM_DESCRIPTION
#' @param legend.title.just PARAM_DESCRIPTION
#' @param legend.text.color PARAM_DESCRIPTION
#' @param legend.text.size PARAM_DESCRIPTION
#' @param legend.text.fontface PARAM_DESCRIPTION
#' @param legend.text.fontfamily PARAM_DESCRIPTION
#' @param legend.frame PARAM_DESCRIPTION
#' @param legend.frame.lwd PARAM_DESCRIPTION
#' @param legend.frame.r PARAM_DESCRIPTION
#' @param legend.bg.color PARAM_DESCRIPTION
#' @param legend.bg.alpha PARAM_DESCRIPTION
#' @param compass.type PARAM_DESCRIPTION
#' @param title.size PARAM_DESCRIPTION
#' @param title.color PARAM_DESCRIPTION
#' @param title.fontface PARAM_DESCRIPTION
#' @param title.fontfamily PARAM_DESCRIPTION
#' @param title.bg.color PARAM_DESCRIPTION
#' @param title.bg.alpha PARAM_DESCRIPTION
#' @param title.padding PARAM_DESCRIPTION
#' @param title.frame PARAM_DESCRIPTION
#' @param title.frame.lwd PARAM_DESCRIPTION
#' @param title.frame.r PARAM_DESCRIPTION
#' @param title.stack PARAM_DESCRIPTION
#' @param title.position PARAM_DESCRIPTION
#' @param title.width PARAM_DESCRIPTION
#' @param title.heigth PARAM_DESCRIPTION
#' @param title.group.frame PARAM_DESCRIPTION
#' @param title.resize.as.group PARAM_DESCRIPTION
#' @param panel.show PARAM_DESCRIPTION
#' @param panel.labels PARAM_DESCRIPTION
#' @param panel.label.size PARAM_DESCRIPTION
#' @param panel.label.color PARAM_DESCRIPTION
#' @param panel.label.fontface PARAM_DESCRIPTION
#' @param panel.label.fontfamily PARAM_DESCRIPTION
#' @param panel.label.bg.color PARAM_DESCRIPTION
#' @param panel.label.height PARAM_DESCRIPTION
#' @param panel.label.rot PARAM_DESCRIPTION
#' @rdname tm_layout
#' @export 
tm_layout = function(
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
	do.call(tm_options, args)
}


