#' @export
tm_legend = function(title  = NA,
					 show = TRUE,
					 design = "portrait",
					 #is.portrait = tmap_option("legend.is.portrait"),
					 reverse = FALSE,
					 position = NA,
					 width = NA,
					 height = NA,
					 settings = tmap_option("legend.settings", design),
					 landscape.setup = tmap_option("legend.landscale.setup"),
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
					 bg.alpha = NA,
					 ...) {
	structure(c(list(FUN = "tmapLegend"), as.list(environment())), class = "tm_legend")
}

#' @export
tm_legend_hide = function() {
	tm_legend(show = FALSE)
}

#' @export
tm_legend_combine = function(aes) {
	structure(list(FUN = "tmapLegend", title = NA, reverse = FALSE, show = FALSE, aes = aes), class = "tm_legend")
}

#' @export
tm_legend_portrait = function(..., design = "portrait") {
	args = c(list(...), list(design = design))
	do.call(tm_legend, args)
	tm_legend(...)
}

#' @export
tm_legend_landscape = function(..., design = "landscape") {
	args = c(list(...), list(design = design))
	do.call(tm_legend, args)
	tm_legend(...)
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

