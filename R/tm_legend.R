tm_legend = function(title  = NA,
					 show = TRUE,
					 is.portrait = TRUE,
					 reverse = FALSE,
					 position = NA,
					 width = NA,
					 height = NA,
					 space = NA,
					 space.na = NA,
					 stack = NA,
					 z = NA,
					 title.color = NA,
					 title.size = NA,
					 title.fontface = NA,
					 title.fontfamily = NA,
					 text.color = NA,
					 text.size = NA,
					 text.fontface = NA,
					 text.fontfamily = NA,
					 format = NA,
					 frame = NA,
					 frame.lwd = NA,
					 bg.color = NA,
					 bg.alpha = NA) {
	structure(c(list(FUN = "tmapLegend"), as.list(environment())), class = "tm_legend")
}


tm_legend_hide = function() {
	tm_legend(show = FALSE)
}

tm_legend_share = function(aes) {
	structure(list(FUN = "tmapLegend", title = NA, reverse = FALSE, show = FALSE, aes = aes), class = "tm_legend")
}

tm_legend_portrait = function(...) {
	tm_legend(...)
}

tm_legend_landscape = function(is.portrait = FALSE, ...) {
	args = c(list(...), list(is.portrait = is.portrait))
	do.call(tm_legend, args)
}

tm_lp_inset = function(h = NA, v = NA) {
	structure(list(h = h, v = v, type = "inset"), class = "tm_lp")
}

tm_lp_cell = function(h = NA, v = NA) {
	structure(list(h = h, v = v, type = "cell"), class = "tm_lp")
}

tm_lp_out = function(h = NA, v = NA) {
	structure(list(h = h, v = v, type = "out"), class = "tm_lp")
}

tm_lp_auto = function(h = NA, v = NA) {
	structure(list(h = h, v = v, type = "auto"), class = "tm_lp")
}

