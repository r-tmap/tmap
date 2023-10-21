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
	.tmapFormats = get("tmapFormats", envir = .TMAP)
	
	formats = names(.tmapFormats)
	
	if (missing(format)) {
		if (get("tmapOptions", envir = .TMAP)$show.messages) message("available formats are: \"", paste(formats, collapse = "\", \""), "\" ")
		return(invisible(formats))
	} else if (format %in% formats) {
		return(.tmapFormats[[format]])
	} else {
		stop("format unknown")
	}
}

#' @rdname tmap_format
#' @name tmap_format_add
#' @param ...  Options from [tm_layout()] or [tm_view()]. Can also be a list of those options.
#' @param name Name of the new format.
#' @export
tmap_format_add = function(..., name) {
	args = list(...)
	if (length(args)==1 && is.list(args[[1]])) args = args[[1]]
	
	.tmapFormats = get("tmapFormats", envir = .TMAP)
	
	.tmapFormats[[name]] = args
	assign("tmapFormats", .tmapFormats, envir = .TMAP)
	
	if (get("tmapOptions", envir = .TMAP)$show.messages) message("format ", name, " succesfully added. Use this format with tm_format(\"", name, "\")")
	invisible(NULL)
}

