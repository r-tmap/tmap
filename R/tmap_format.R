#' Get or add format options
#'
#' Format options are tmap options that are shape dependent.
#' With `tmap_format()` the predefined formats can be retrieved.
#' The values for a specific format can be retrieved with `tmap_format(format)`,
#' where format is the name of the format. The function `tmap_format_add()` is used to add a format.
#'
#' @param format Name of the format. Run `tmap_format()` to see the choices.
#' @return The function `tmap_format()` returns the names of the available formats.
#'   When `format` is defined, it returns the option list corresponding the that format.
#' @seealso
#' * [tm_layout()] for predefined styles
#' * `tmap_style_catalogue` (not migrated to v4 yet) to create a style catalogue of all available styles.
#' * [tmap_options()] for tmap options
#' @example ./examples/tmap_format.R
#' @rdname tmap_format
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

#' @rdname tmap_format
#' @name tmap_format_add
#' @param ...  Options from [tm_layout()] or [tm_view()]. Can also be a list of those options.
#' @param name Name of the new format.
#' @export
tmap_format_add = function(..., name) {
	v3_tmap_format_add(name)
	invisible(NULL)
}



#' @rdname tm_layout
#' @order 2
#' @param format name of the format
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
