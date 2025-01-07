#' Deprecated: format
#'
#' In tmap < 4.0 it was possible to set shape-specific options, such as margins and legend position. However, this has become superfluous because in tmap > 4.0 legends are by default placed outside the map area. If needed, a shape-specific set of options can be stored as a style with tmap_options_save.
#'
#' @param format Name of the format
#' @param ... not used
#' @param name Name of the format
#' @keywords internal
#' @rdname tmap-deprecated
#' @export
tmap_format = function(format) {
	if (format == "World") {
		x = list(inner.margins=c(0, 0.05, 0.025, 0.01), legend.position=tm_pos_in("left", "bottom"), component.position=c("right", "bottom"), scale=.8, title.size = 1.3)
	} else if (format == "World_wide") {
		x = list(inner.margins=c(0, 0.2, 0.025, 0.01), legend.position=tm_pos_in("left", "bottom"), component.position=c("right", "bottom"), scale=.8)
	} else if (format == "NLD") {
		x = list(frame = FALSE, inner.margins = c(.02, .2, .06, .02), legend.position = tm_pos_in("left", "top"), component.position=c("left", "bottom"))
	} else if (format == "NLD_wide") {
		x = list(frame = FALSE, inner.margins = c(.02, .3, .06, .02), legend.position = tm_pos_in("left", "top"), component.position=c("left", "bottom"))
	} else {
		x = NULL
	}
	v3_tmap_format(format)
	x
}

#' @rdname tmap-deprecated
#' @export
tmap_format_add = function(..., name) {
	v3_tmap_format_add(name)
	invisible(NULL)
}


#' @rdname tmap-deprecated
#' @export
tm_format = function(format, ...) {
	if (format == "World") {
		x = tm_layout(inner.margins=c(0, 0.05, 0.025, 0.01), legend.position=tm_pos_in("left", "bottom"), component.position=c("right", "bottom"), scale=.8, title.size = 1.3)
		code = "tm_layout(inner.margins=c(0, 0.05, 0.025, 0.01), legend.position=tm_pos_in(\"left\", \"bottom\"), component.position=c(\"right\", \"bottom\"), scale=.8, title.size = 1.3)" } else if (format == "World_wide") {
			x = tm_layout(inner.margins=c(0, 0.2, 0.025, 0.01), legend.position=tm_pos_in("left", "bottom"), component.position=c("right", "bottom"), scale=.8)
			code = "tm_ayout(inner.margins=c(0, 0.2, 0.025, 0.01), legend.position=tm_pos_in(\"left\", \"bottom\"), component.position=c(\"right\", \"bottom\"), scale=.8)"
		} else if (format == "NLD") {
			x = tm_layout(frame = FALSE, inner.margins = c(.02, .2, .06, .02), legend.position = tm_pos_in("left", "top"), component.position=c("left", "bottom"))
			code = "tm_layout(frame = FALSE, inner.margins = c(.02, .2, .06, .02), legend.position = tm_pos_in(\"left\", \"top\"), component.position=c(\"left\", \"bottom\"))"
		} else if (format == "NLD_wide") {
			x = tm_layout(frame = FALSE, inner.margins = c(.02, .3, .06, .02), legend.position = tm_pos_in("left", "top"), component.position=c("left", "bottom"))
			code = "tm_layout(frame = FALSE, inner.margins = c(.02, .3, .06, .02), legend.position = tm_pos_in(\"left\", \"top\"), component.position=c(\"left\", \"bottom\"))"
		} else {
			x = NULL
			code = ""
		}
	v3_tm_format(format, code)
	x
}
