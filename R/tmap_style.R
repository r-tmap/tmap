#' Set or get the default tmap style
#' 
#' Set or get the default tmap style. Without arguments, the current style is returned. Also the available styles are displayed. When a style is set, the corresponding tmap options (see [tmap_options()]) will be set accordingly. The default style (i.e. when loading the package) is `"white"`.
#' 
#' Note that [tm_style()] is used within a plot call (so it only affects that plot), whereas `tmap_style` sets the style globally.
#' 
#' After loading a style, the options that defined this style (i.e. the difference with the default `"white"` style) can be obtained by [tmap_options_diff()].
#' 
#' The documentation of [tmap_options()] (details and the examples) shows how a new style is created.
#' 
#' @param style Name of the style. When omitted, `tmap_style` returns the current style and also shows all available styles. When the style is specified, `tmap_style` sets the style accordingly. Note that in that case, all tmap options (see [tmap_options()]) will be reset according to the style definition. See [tm_layout()] for predefined styles, and `tmap_style_catalogue` (not migrated to v4 yet) for creating a catalogue.
#' @return The style before changing
#' @seealso [tmap_options()] for tmap options, and `tmap_style_catalogue` (not migrated to v4 yet) to create a style catalogue of all available styles.
#' @example ./examples/tmap_style.R
#' @export
#' @rdname tmap_style
tmap_style = function(style) {
	current.style = getOption("tmap.style")
	show.messages = get("tmapOptions", envir = .TMAP)$show.messages
	
	if (missing(style) && show.messages) {
		message("current tmap style is \"", current.style, "\"")
		message("other available styles are: ", print_text_vector(get_style_names(current.style)))
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
			message("tmap style set to \"", style, "\"")
			message("other available styles are: ", print_text_vector(get_style_names(style)))
		}
	}
	invisible(current.style)
}

print_text_vector = function(x) {
	paste0("\"", paste(x, collapse = "\", \""), "\" ")
}

get_style_names = function(except_style = NULL, remove_grey = TRUE) {
	styles = c("white", names(get("tmapStyles", envir = .TMAP)))
	if (!is.null(except_style)) {
		styles = setdiff(styles, except_style)
	}
	
	# remove double name gray/grey
	if (remove_grey) {
		if (!is.null(except_style) && (except_style %in% c("gray", "grey"))) {
			styles = setdiff(styles, c("gray", "grey"))
		} else {
			styles = setdiff(styles, "grey")
		}
	}
	
	styles
}

check_style = function(style) {
	styles = get_style_names(remove_grey = FALSE)
	if (!style %in% styles) stop("style \"" , style, "\" unknown. The available styles are: ", print_text_vector(get_style_names()), call. = FALSE)
}
