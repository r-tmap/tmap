#' @export
tm_legend = function(title  = NA,
					 show = TRUE,
					 design = "portrait",
					 reverse = FALSE,
					 position = NA,
					 width = NA,
					 height = NA,
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
	args = c(as.list(environment()), list(...))
	settings_name = paste0("legend.settings.", design)
	settings = tmap_option(settings_name)
	unset = setdiff(names(settings), names(args))
	if (length(unset)) args[unset] = settings[unset]
	cls = c(paste0("tm_legend_", design), "tm_legend", "list")
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
tm_legend_portrait = function(..., design = "portrait") {
	tm_legend(..., design = design)
}

#' @export
tm_legend_landscape = function(..., design = "landscape") {
	tm_legend(..., design = design)
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

