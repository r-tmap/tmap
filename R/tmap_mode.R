#' Set tmap mode to static plotting or interactive viewing
#'
#' @description
#' * `tmap_mode()` informs of the current mode (if called without argument).
#' * `ttm()` toggles between the most recent two modes.
#' * `ttmp()` same as `ttm()` and calls [tmap_last()] to display the last map in the new mode.
#' * `rtm()` rotate between between all modes
#' * `rtmp()` same as `rtm()` and calls [tmap_last()] to display the last map in the new mode.
#'
#' Set tmap mode to static plotting or interactive viewing.
#' The global option `tmap.mode` determines the whether thematic maps are plot
#' in the graphics device, or shown as an interactive leaflet map (see also [tmap_options()].
#' The function `tmap_mode()` is a wrapper to set this global option.
#' The convenient function `ttm()`, which stands for toggle thematic map,
#' is a toggle switch between the two modes. The function `ttmp()` stands for
#' toggle thematic map and print last map: it does the same as `ttm()` followed
#' by [tmap_last()]; in order words, it shows the last map in the other mode.
#' It is recommended to use `tmap_mode()` in scripts and `ttm()`/`ttmp()` in the console.
#'
#' @details
#' # `mode = "plot"`
#'
#' Thematic maps are shown in the graphics device.
#' This is the default mode, and supports all tmap's features,
#' such as small multiples (see [tm_facets()]) and extensive layout settings (see [tm_layout()]).
#' It is recommended to use [tmap_save()] for saving static maps.
#'
#' # `mode = "view"`
#'
#' Thematic maps are viewed interactively in the web browser or RStudio's Viewer pane.
#' Maps are fully interactive with tiles from OpenStreetMap or other map providers
#' (see [tm_tiles()]). See also [tm_view()] for options related to the `"view"` mode.
#' This mode generates a [leaflet::leaflet()] widget, which can also be directly
#' obtained with [tmap_leaflet()].
#' With R Markdown, it is possible to publish it to an HTML page.
#'
#' However, there are a couple of constraints in comparison to `"plot"`:
#'
#' @param mode One of `"plot"` or `"view"`. See Details for more info.
#' @return
#' * `tmap_mode()` returns the current tmap mode invisibly (when called without argument).
#'   Otherwise, returns the previous mode.
#' * `ttm()` switches mode and returns previous tmap mode invisibly.
#' The previous tmap mode before switching.
#' @example ./examples/tmap_mode.R
#' @seealso \href{https://r-tmap.github.io/tmap/articles/basics_modes}{vignette about modes}
#' * [tmap_last()] to show the last map
#' * [tm_view()] for viewing options
#' * [tmap_leaflet()] for obtaining a leaflet widget
#' * [tmap_options()] for tmap options
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R},
#' Journal of Statistical Software, 84(6), 1-39, \doi{10.18637/jss.v084.i06}
#' @export
tmap_mode = function(mode = NULL) {
	set_mode(mode, show.messages = get("tmapOptions", envir = .TMAP)$show.messages, type = "set")
}


#' @rdname tmap_mode
#' @export
ttm = function() {
	set_mode(show.messages = get("tmapOptions", envir = .TMAP)$show.messages, type = "toggle")
}

#' @rdname tmap_mode
#' @export
rtm = function() {
	set_mode(show.messages = get("tmapOptions", envir = .TMAP)$show.messages, type = "rotate")
}


set_mode = function(mode = NULL, show.messages = FALSE, type = "set") {
	current.mode = getOption("tmap.mode")

	tOpt = get("tmapOptions", envir = .TMAP)
	show.messages = tOpt$show.messages

	modes = get_modes()
	id = match(current.mode, modes)

	if (type == "toggle") {
		mode = .TMAP$mode_last
	} else if (type == "rotate") {
		id = id + 1L
		if (id > length(modes)) id = 1L
		mode = modes[id]
	}

	modes_str = paste0("{.val ", modes, "}")
	modes_str[id] = paste0("{.strong ", modes_str[id], "}")
	rotate_str = paste(modes_str, collapse = " -> ")


	if (is.null(mode)) {
		others = setdiff(modes, current.mode)
		tmode = .TMAP$mode_last
		if (length(modes) == 2) {
			cli::cli_inform(c(
				"i" = "Current tmap mode is {.val {current.mode}}.",
				"i" = "Call {.run tmap::ttm()} to switch to mode {.val {tmode}}."))
		} else {
			cli::cli_inform(c(
				"i" = "Current tmap mode is {.val {current.mode}}.",
				"i" = "Call {.run tmap::ttm()} to switch to mode {.val {tmode}}.",
				"i" = paste0("Call {.run tmap::rtm()} to rotate between all modes: ", rotate_str)
			))
		}

	} else {
		rlang::arg_match0(mode, modes)
		if (current.mode != mode) .TMAP$mode_last = current.mode
		options(tmap.mode = mode)
		fill_providers()
		if (show.messages) {
			if (type == "set") {
				cli::cli_inform(c(i = "tmap mode set to {.val {mode}}."))
			} else if (type == "toggle") {
				cli::cli_inform(c("i" = "tmap mode set to {.val {mode}}.",
								  "i" = "switch back to {.val {current.mode}} mode with {.run tmap::ttm()}"))
			} else {
				nid = id + 1L
				if (nid > length(modes)) nid = 1L
				nmode = modes[nid]
				str = paste0("rotate cycle ", rotate_str, " with {.run tmap::rtm()}")
				cli::cli_inform(c("i" = "tmap mode rotated to {.val {modes[id]}}.",
								  "i" = str))

			}
		}
	}
	invisible(current.mode)
}


# tmap_graphics = function(mode = NULL) {
# 	if (is.null(mode)) mode = getOption("tmap.mode")
# 	get("tmapOptions", envir = .TMAP)$graphics[[mode]]
# }
#
# tmap_graphics_name = function(mode = NULL) {
# 	tmap_graphics(mode = mode)$name
# }

get_modes = function() {
	names(get("tmapOptions", envir = .TMAP)$modes)
}

#' Set the design mode
#'
#' When the so-called "design mode" is enabled, inner and outer margins,
#' legend position, and aspect ratio are shown explicitly in plot mode.
#' Also, information about aspect ratios is printed in the console.
#' This function sets the global option `tmap.design.mode`.
#' It can be used as toggle function without arguments.
#'
#' @seealso [tmap_options()]
#' @param design.mode Logical value that determines the design mode.
#'   If omitted then the design mode is toggled.
#' @export
tmap_design_mode = function(design.mode) {
	dm = if (missing(design.mode)) {
		!getOption("tmap.design.mode")
	} else {
		if (!is.logical(design.mode)) stop("design.mode is not a logical")
		design.mode[1]
	}

	options(tmap.design.mode = dm)
	message(
		"design.mode: ", if (!dm) "OFF" else "ON",
		if (dm && getOption("tmap.mode") == "view") " (only effective in plot mode)" else "")
}

#' Set the development mode
#'
#' When the so-called "development mode" is enabled, helpful messages and timings
#' are printed in the console
#'
#' @param devel.mode logical value that determines the development mode.
#'   If omitted then the development mode is toggled.
#' @export
tmap_devel_mode = function(devel.mode) {
	dm = if (missing(devel.mode)) {
		!getOption("tmap.devel.mode")
	} else {
		if (!is.logical(devel.mode)) stop("devel.mode is not a logical")
		devel.mode[1]
	}

	options(tmap.devel.mode = dm)
	message("devel.mode: ", if (!dm) "OFF" else "ON")
}

pm = function(message) {
	cat("<><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><><>\n")
	cat(message, "\n")
}

po = function(...) {
	e = substitute(list(...))
	nms = sapply(e, deparse)[-1]

	x = list(...)

	for (i in seq_along(x)) {
		cat("<==================== ", nms[i], "===============>\n")
		print(x[[i]])
		if (i == length(x)) {
			cat("</============================================>\n")
		}
	}

	invisible()
}

so = function(...) {
	e = substitute(list(...))
	nms = sapply(e, deparse)[-1]

	x = list(...)

	for (i in seq_along(x)) {
		cat("<==================== ", nms[i], "===============>\n")
		str(x[[i]])
		if (i == length(x)) {
			cat("</============================================>\n")
		}
	}

	invisible()
}

#' @rdname tmap_mode
#' @export
ttmp = function() {
	ttm()
	tmap_last()
}

#' @rdname tmap_mode
#' @export
rtmp = function() {
	rtm()
	tmap_last()
}


check_unit = function(unit) {
	if (!unit %in% c("metric", "imperial", "km", "m", "mi", "miles", "ft", "feet")) stop("incorrect unit", call. = FALSE)
}
