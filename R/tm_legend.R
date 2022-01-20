#' @export
tm_legend = function(title  = NA,
					 show = NA,
					 orientation = NA,
					 design = NA,
					 reverse = NA,
					 position = NA,
					 width = NA,
					 height = NA,
					 stack = NA,
					 z = NA,
					 group.frame = NA,
					 group.just = NA,
					 block.just = NA,
					 resize.as.group = NA,
					 title.color = NA,
					 title.size = NA,
					 title.fontface = NA,
					 title.fontfamily = NA,
					 title.padding = NA,
					 text.color = NA,
					 text.size = NA,
					 text.fontface = NA,
					 text.fontfamily = NA,
					 format = NA,
					 frame = NA,
					 frame.lwd = NA,
					 bg.color = NA,
					 bg.alpha = NA,
					 item.height = NA,
					 item.width = NA,
					 item.space = NA,
					 item.na.height = NA,
					 item.na.width = NA,
					 item.na.space = NA,
					 ticks = NA,
					 ticks.disable.na = NA,
					 ticks.col = NA,
					 ticks.lwd = NA,
					 title.just = NA,
					 margins = NA,
					 margin.item.text = NA) {
	arg.calls = names(match.call(expand.dots = TRUE)[-1])
	args = as.list(environment())
	cls = c("tm_legend", "list")
	structure(args, class = cls)
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
tm_lp_in = function(h = NA, v = NA) {
	structure(list(h = h, v = v, type = "in"), class = "tm_lp")
}

#' @export
tm_lp_cell = function(h = NA, v = NA) {
	structure(list(h = h, v = v, type = "cell"), class = "tm_lp")
}

#' @export
tm_lp_out = function(h = NA, v = NA) {
	structure(list(h = h, v = v, type = "out"), class = "tm_lp")
}

#' @export
tm_lp_auto = function(h = NA, v = NA) {
	structure(list(h = h, v = v, type = "auto"), class = "tm_lp")
}

