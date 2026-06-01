#' Popup specification for interactive maps
#'
#' `tm_popup()` specifies the popups that are shown in interactive (`"view"`)
#' mode when a feature is clicked. It is passed to the `popup` argument of the
#' layer functions ([tm_polygons()], [tm_symbols()], [tm_lines()], etc.). It
#' replaces the (now deprecated) layer arguments `popup.vars` and `popup.format`.
#'
#' @param vars Names of the data variables that are shown in the popup table.
#'   A (possibly named) character vector; when named, the names are used as
#'   labels in the popup table. Besides a character vector, the following
#'   special values are supported (identical to the former `popup.vars`
#'   argument):
#'   \describe{
#'     \item{`TRUE`}{show all variables of the shape object;}
#'     \item{`FALSE`}{disable popups;}
#'     \item{`NA` (default)}{automatic: if visual variables (e.g. `fill`) are
#'       used, only those are shown, otherwise all variables of the shape
#'       object are shown.}
#'   }
#' @param title Name of the data variable used as the popup title (the bold
#'   header shown above the popup table). This overrules the layer argument
#'   `id`, analogous to how `hover` overrules `id` for hover labels. The
#'   default (`NA`) means that the popup title is derived from `id` (the former,
#'   and still default, behaviour). A length-one character vector is expected;
#'   a named value is allowed and reserved for future use.
#' @param format A list of formatting options for the popup values, the output
#'   of [tm_label_format()]. Only applicable to numeric data variables. If one
#'   list of formatting options is provided, it is applied to all numeric
#'   variables of `vars`. A (named) list of lists can also be provided; in that
#'   case, each list of formatting options is applied to the named variable.
#' @param width Width of the popup content (view mode). A bare number is
#'   interpreted as pixels (e.g. `300` means `"300px"`); a character string is
#'   used as-is, so any CSS length is accepted (`"300px"`, `"20em"`, `"50%"`).
#'   The default `"auto"` lets the popup size to its content.
#' @param max.height Maximum height of the popup table before it becomes
#'   vertically scrollable (view mode). A bare number is interpreted as `em`
#'   (e.g. `5` means `"5em"`, roughly "show 5 lines"); a character string is
#'   used as-is. Default `"25em"`. Use `max.height = "none"` (or `NA`/`Inf`) to
#'   remove the cap, so the popup grows to fit its content and never scrolls.
#' @param label.color Color of the variable-name (label) column in the popup
#'   table. Default `"#888888"` (grey).
#' @param value.align Horizontal alignment of the value column in the popup
#'   table, one of `"right"` (default), `"left"`, or `"center"`.
#' @return A `tm_popup` object.
#' @seealso [tm_polygons()], [tm_symbols()], [tm_lines()]
#' @export
tm_popup = function(vars = NA,
					title = NA,
					format = tm_label_format(),
					width = "auto",
					max.height = "25em",
					label.color = "#888888",
					value.align = "right") {
	structure(
		list(vars = vars,
			 title = title,
			 format = format,
			 layout = list(width = width,
			 			   max.height = max.height,
			 			   label.color = label.color,
			 			   value.align = value.align)),
		class = c("tm_popup", "list")
	)
}

# Internal: default popup layout, and completion of a (possibly partial or
# NULL) layout list with these defaults. Used by view_format_popups() so that
# callers passing layout = NULL (e.g. extension layers, or the inset-frame and
# raster code paths) fall back to the documented defaults.
popup_layout_default = function() {
	list(width = "auto",
		 max.height = "25em",
		 label.color = "#888888",
		 value.align = "right")
}

complete_popup_layout = function(x) {
	d = popup_layout_default()
	if (is.null(x) || !length(x)) {
		out = d
	} else {
		x = x[!vapply(x, is.null, logical(1))]
		out = utils::modifyList(d, x)
	}
	# Allow bare numbers as CSS lengths. A unit-less number is interpreted with
	# a sensible default unit per property: pixels for width (e.g. width = 300
	# -> "300px") and em for max.height (e.g. max.height = 5 -> "5em", i.e.
	# roughly "show 5 lines"). Character values are passed through unchanged, so
	# any CSS length ("300px", "20em", "50%", "auto") still works.
	out$width = .as_css_length(out$width, "px")
	out$max.height = .as_css_length(out$max.height, "em")
	out
}

.as_css_length = function(x, unit = "px") {
	# NA / non-finite means "no cap" (used by max.height to disable scrolling).
	if (length(x) == 1L && is.na(x)) return("none")
	if (is.numeric(x)) {
		if (!is.finite(x)) return("none")
		return(paste0(x, unit))
	}
	x
}

#' @export
print.tm_popup = function(x, ...) {
	cli::cli_text("{.cls tm_popup} specification")
	invisible(x)
}

# Internal: resolve the popup specification of a layer function.
#
# It combines the new `popup` argument (a `tm_popup` object, or a bare value
# that is interpreted as `vars`) with the deprecated `popup.vars` /
# `popup.format` arguments, and returns a list with the three element fields
# that are stored on the tm_element: `vars`, `format`, and `title`.
#
# The `*.called` flags indicate whether the user explicitly supplied the
# corresponding argument (typically `"<arg>" %in% args_called`). They are used
# to decide (a) which source wins and (b) whether to emit a deprecation
# message. Because the convenience wrappers (e.g. `tm_dots()`, `tm_fill()`)
# forward these arguments via `...` rather than as their own formals, the
# `args_called` of the primitive layer function reliably reflects user intent.
process_popup = function(popup = tm_popup(),
						 popup.vars = NA,
						 popup.format = tm_label_format(),
						 popup.called = FALSE,
						 popup.vars.called = FALSE,
						 popup.format.called = FALSE,
						 layer_fun = NULL) {

	depr_called = popup.vars.called || popup.format.called

	if (popup.called) {
		# New API used.
		if (depr_called) message_popup_both(layer_fun)
		if (!inherits(popup, "tm_popup")) {
			# Permissive shorthand, e.g. popup = c("a", "b") or popup = FALSE.
			popup = tm_popup(vars = popup)
		}
	} else if (depr_called) {
		# Only the deprecated arguments were used.
		message_popup_deprecated(layer_fun)
		popup = tm_popup(vars = popup.vars, format = popup.format)
	}
	# else: keep the (default) `popup`, i.e. tm_popup().

	# Guard against a NULL format (e.g. tm_popup(format = NULL)).
	format = popup$format
	if (is.null(format)) format = tm_label_format()

	# Layout (scalar styling). It is present on tm_popup objects; for a bare
	# shorthand value it falls back to the defaults.
	layout = popup$layout
	if (is.null(layout)) layout = popup_layout_default()

	list(vars = popup$vars,
		 format = format,
		 title = popup$title,
		 layout = layout)
}

message_popup_deprecated = function(layer_fun = NULL) {
	fn = if (is.null(layer_fun)) "the layer function" else paste0(layer_fun, "()")
	cli::cli_inform(
		c("{.field [deprecated]} The arguments {.arg popup.vars} and {.arg popup.format} of {.code {fn}} are deprecated.",
		  "i" = "Use {.arg popup} with {.fn tm_popup} instead, e.g. {.code popup = tm_popup(vars = ..., format = ...)}."),
		.frequency = "regularly",
		.frequency_id = "popup-vars-deprecated"
	)
}

message_popup_both = function(layer_fun = NULL) {
	fn = if (is.null(layer_fun)) "the layer function" else paste0(layer_fun, "()")
	cli::cli_inform(
		c("{.field [popup]} Both {.arg popup} and the deprecated {.arg popup.vars}/{.arg popup.format} were supplied to {.code {fn}}.",
		  "i" = "The deprecated arguments are ignored in favour of {.arg popup}."),
		.frequency = "regularly",
		.frequency_id = "popup-both"
	)
}
