#' Legend
#'
#' Legend specification
#'
#' @param title Legend title
#' @param show Show legend?
#' @param orientation Orientation of the legend: `"portrait"` or `"landscape"`
#' @param design PARAM_DESCRIPTION
#' @param reverse Should the legend be reversed?
#' @param na.show PARAM_DESCRIPTION
#' @param position PARAM_DESCRIPTION
#' @param width Width of the legend
#' @param height Height of the legend
#' @param stack PARAM_DESCRIPTION
#' @param z PARAM_DESCRIPTION
#' @param group.frame PARAM_DESCRIPTION
#' @param resize_as_group PARAM_DESCRIPTION
#' @param title.color Color of the legend title
#' @param title.size Size of the legend title
#' @param title.fontface Font face of the legend title
#' @param title.fontfamily Font family of the legend title
#' @param title.padding PARAM_DESCRIPTION
#' @param text.color Color of the legend text
#' @param text.size Size of the legend text
#' @param text.fontface Font face of the legend text
#' @param text.fontfamily Font family of the legend text
#' @param format PARAM_DESCRIPTION
#' @param frame PARAM_DESCRIPTION
#' @param frame.lwd PARAM_DESCRIPTION
#' @param frame.r PARAM_DESCRIPTION
#' @param bg.color Background color of the legend
#' @param bg.alpha Background transparency of the legend
#' @param item.height PARAM_DESCRIPTION
#' @param item.width PARAM_DESCRIPTION
#' @param item.space PARAM_DESCRIPTION
#' @param item.na.height PARAM_DESCRIPTION
#' @param item.na.width PARAM_DESCRIPTION
#' @param item.na.space PARAM_DESCRIPTION
#' @param item.shape PARAM_DESCRIPTION
#' @param ticks PARAM_DESCRIPTION
#' @param ticks.disable.na PARAM_DESCRIPTION
#' @param ticks.col PARAM_DESCRIPTION
#' @param ticks.lwd PARAM_DESCRIPTION
#' @param title.align PARAM_DESCRIPTION
#' @param margins PARAM_DESCRIPTION
#' @param margin.item.text PARAM_DESCRIPTION
#' @param ... passed on (?)
#' @param variable visual (or transformation) variable to combine the legend with: e.g. `"fill"` or `"size"`
#' @return A tm_legend component
#' @export
tm_legend = function(title,
					 show,
					 orientation,
					 design,
					 reverse,
					 na.show,
					 position,
					 width,
					 height,
					 stack,
					 z,
					 group.frame,
					 resize_as_group,
					 title.color,
					 title.size,
					 title.fontface,
					 title.fontfamily,
					 title.padding,
					 text.color,
					 text.size,
					 text.fontface,
					 text.fontfamily,
					 format,
					 frame,
					 frame.lwd,
					 frame.r,
					 bg.color,
					 bg.alpha,
					 item.height,
					 item.width,
					 item.space,
					 item.na.height,
					 item.na.width,
					 item.na.space,
					 item.shape,
					 ticks,
					 ticks.disable.na,
					 ticks.col,
					 ticks.lwd,
					 title.align,
					 margins,
					 margin.item.text,
					 ...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("title" %in% (names(args)))) args$title = NA
	if (!("xlab" %in% (names(args)))) args$xlab = NA
	if (!("ylab" %in% (names(args)))) args$ylab = NA
	if (!("z" %in% (names(args)))) args$z = NA_integer_
	structure(args, class = c("tm_legend", "tm_component", "list"))
}

#' @rdname tm_legend
#' @export
tm_legend_hide = function() {
	tm_legend(show = FALSE)
}

#' @rdname tm_legend
#' @export
tm_legend_combine = function(variable) {
	structure(list(FUN = "tmapLegend", title = NA, reverse = FALSE, show = FALSE, aes = variable), class = c("tm_legend", "tm_component", "list"))
}

tm_legend_bivariate = function(xlab,
							   ylab,
							   xlab.color,
							   xlab.size,
							   xlab.fontface,
							   xlab.fontfamily,
							   xlab.padding,
							   xlab.align,
							   ylab.color,
							   ylab.size,
							   ylab.fontface,
							   ylab.fontfamily,
							   ylab.padding,
							   ylab.align,
							   ...) {
	args = lapply(as.list(rlang::call_match()[-1]), eval, envir = parent.frame())

	if (!("title" %in% (names(args)))) args$title = NA
	if (!("xlab" %in% (names(args)))) args$xlab = NA
	if (!("ylab" %in% (names(args)))) args$ylab = NA
	if (!("z" %in% (names(args)))) args$z = NA_integer_
	args$orientation = "portrait"
	structure(args, class = c("tm_legend", "tm_component", "list"))
}
