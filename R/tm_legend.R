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
					 #group.just,
					 #block.just,
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
	structure(args, class = c("tm_legend", "list"))
}

#' @export
tm_legend_hide = function() {
	tm_legend(show = FALSE)
}

#' @export
tm_legend_combine = function(aes) {
	structure(list(FUN = "tmapLegend", title = NA, reverse = FALSE, show = FALSE, aes = aes), class = c("tm_legend", "list"))
}




#' @export
tm_lp_in = function(pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "in"
	structure(args, class = "tm_lp")
}

#' @export
tm_lp_cell = function(pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "cell"
	structure(args, class = "tm_lp")
}

#' @export
tm_lp_out = function(cell.h, cell.v, pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "out"
	structure(args, class = "tm_lp")
}

#' @export
tm_lp_auto = function(cell.h, cell.v, pos.h, pos.v, just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "auto"
	structure(args, class = "tm_lp")
}


#' @export
tm_lp_auto_in = function(just.h, just.v) {
	args = lapply(as.list(match.call()[-1]), eval, envir = parent.frame())
	args$type = "autoin"
	structure(args, class = "tm_lp")
}
