#' Legend
#' 
#' Legend specification
#' 
#' @param title PARAM_DESCRIPTION
#' @param show PARAM_DESCRIPTION
#' @param orientation PARAM_DESCRIPTION
#' @param design PARAM_DESCRIPTION
#' @param reverse PARAM_DESCRIPTION
#' @param position PARAM_DESCRIPTION
#' @param width PARAM_DESCRIPTION
#' @param height PARAM_DESCRIPTION
#' @param stack PARAM_DESCRIPTION
#' @param z PARAM_DESCRIPTION
#' @param group.frame PARAM_DESCRIPTION
#' @param resize.as.group PARAM_DESCRIPTION
#' @param title.color PARAM_DESCRIPTION
#' @param title.size PARAM_DESCRIPTION
#' @param title.fontface PARAM_DESCRIPTION
#' @param title.fontfamily PARAM_DESCRIPTION
#' @param title.padding PARAM_DESCRIPTION
#' @param text.color PARAM_DESCRIPTION
#' @param text.size PARAM_DESCRIPTION
#' @param text.fontface PARAM_DESCRIPTION
#' @param text.fontfamily PARAM_DESCRIPTION
#' @param format PARAM_DESCRIPTION
#' @param frame PARAM_DESCRIPTION
#' @param frame.lwd PARAM_DESCRIPTION
#' @param frame.r PARAM_DESCRIPTION
#' @param bg.color PARAM_DESCRIPTION
#' @param bg.alpha PARAM_DESCRIPTION
#' @param item.height PARAM_DESCRIPTION
#' @param item.width PARAM_DESCRIPTION
#' @param item.space PARAM_DESCRIPTION
#' @param item.na.height PARAM_DESCRIPTION
#' @param item.na.width PARAM_DESCRIPTION
#' @param item.na.space PARAM_DESCRIPTION
#' @param ticks PARAM_DESCRIPTION
#' @param ticks.disable.na PARAM_DESCRIPTION
#' @param ticks.col PARAM_DESCRIPTION
#' @param ticks.lwd PARAM_DESCRIPTION
#' @param title.just PARAM_DESCRIPTION
#' @param margins PARAM_DESCRIPTION
#' @param margin.item.text PARAM_DESCRIPTION
#' @param aes aes
#' @return OUTPUT_DESCRIPTION
#' @rdname tm_legend
#' @export 
tm_legend = function(title,
					 show,
					 orientation,
					 design,
					 reverse,
					 position,
					 width,
					 height,
					 stack,
					 z,
					 group.frame,
					 resize.as.group,
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
					 ticks,
					 ticks.disable.na,
					 ticks.col,
					 ticks.lwd,
					 title.just,
					 margins,
					 margin.item.text) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	if (!("title" %in% (names(args)))) args$title = NA
	if (!("z" %in% (names(args)))) args$z = NA
	structure(args, class = c("tm_legend", "tm_component", "list"))
}


#' @name tm_legend_hide
#' @rdname tm_legend
#' @export 
tm_legend_hide = function() {
	tm_legend(show = FALSE)
}

#' @name tm_legend_combine
#' @rdname tm_legend
#' @export 
tm_legend_combine = function(aes) {
	structure(list(FUN = "tmapLegend", title = NA, reverse = FALSE, show = FALSE, aes = aes), class = c("tm_legend", "tm_component", "list"))
}
