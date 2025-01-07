#' Set or get the default tmap style
#'
#' Set or get the default tmap style. Without arguments, the current style is returned.
#' Also the available styles are displayed. When a style is set, the corresponding tmap
#' options (see [tmap_options()]) will be set accordingly.
#' The default style (i.e. when loading the package) is `"white"`.
#'
#' Note that [tm_style()] is used within a plot call (so it only affects that plot),
#' whereas `tmap_style()` sets the style globally.
#'
#' After loading a style, the options that defined this style
#' (i.e. the difference with the default `"white"` style) can be obtained by [tmap_options_diff()].
#'
#' The documentation of [tmap_options()] (details and the examples) shows how to create a new style.
#'
#' @param style Name of the style. When omitted, `tmap_style()` returns the current style
#'   and also shows all available styles. When the style is specified,`tmap_style()`
#'   sets the style accordingly. Note that in that case, all tmap options (see [tmap_options()])
#'   will be reset according to the style definition.
#'   See [tm_layout()] for predefined styles, and `tmap_style_catalogue()` for creating a catalogue.
#' @return The style before changing
#' @seealso
#' * [tmap_options()] for tmap options
#' * [tmap_style_catalogue()]  to create a style catalogue of all available styles.
#' @example ./examples/tmap_style.R
#' @export
#' @rdname tmap_style
tmap_style = function(style) {
	current.style = getOption("tmap.style")
	show.messages = get("tmapOptions", envir = .TMAP)$show.messages

	if (missing(style) && show.messages) {
		message("current tmap style is ", style_names(current.style))
		message("other available styles are: ", print_text_vector(get_style_names(current.style, v3 = "no")))
		message("tmap v3 styles: ", print_text_vector(get_style_names(current.style, v3 = "only")))
	} else {
		.tmapOptions = .defaultTmapOptions
		check_style(style)
		options(tmap.style=style)
		if (style == "white") {
			.tmapOptions = .defaultTmapOptions
		} else {
			styleOptions = get("tmapStyles", envir = .TMAP)[[style]]
			.tmapOptions = complete_options(styleOptions, .tmapOptions)
			attr(.tmapOptions, "style") = style
		}

		assign("tmapOptions", .tmapOptions, envir = .TMAP)

		if (show.messages) {
			message("style set to ", style_names(style))
			message("other available styles are: ", print_text_vector(get_style_names(style, v3 = "no")))
			message("tmap v3 styles: ", print_text_vector(get_style_names(style, v3 = "only")))
		}
	}
	invisible(current.style)
}



print_text_vector = function(x) {
	x2 = style_names(x)

	paste(x2, collapse = ", ")
}

style_names = function(x) {
	x2 = paste0("\"", x, "\"")
	x2[x2 == "\"white\""] = "\"white\" (tmap default)"
	x2[x2 == "\"v3\""] = "\"v3\" (tmap v3 default)"
	x2
}

get_style_names = function(except_style = NULL, remove_grey = TRUE, v3 = "yes") {
	styles = c("white", names(get("tmapStyles", envir = .TMAP)))
	if (!is.null(except_style)) {
		styles = setdiff(styles, except_style)
	}

	# remove double name gray/grey
	if (remove_grey) {
		if (!is.null(except_style) && (except_style %in% c("gray", "grey", "gray_v3", "grey_v3"))) {
			styles = setdiff(styles, c("gray", "grey", "gray_v3", "grey_v3"))
		} else {
			styles = setdiff(styles, c("grey", "grey_v3"))
		}
	}

	is_v3 = substr(styles, nchar(styles) - 1, nchar(styles)) == "v3"

	if (v3 == "no") {
		styles = styles[!is_v3]
	} else if (v3 == "only") {
		styles = styles[is_v3]
	}

	styles
}




check_style = function(style) {
	styles = get_style_names(remove_grey = FALSE)
	if (!style %in% styles) {
		stop("style \"" , style, "\" unknown. The available styles are: ",
			 print_text_vector(get_style_names()), call. = FALSE)
	}
}
