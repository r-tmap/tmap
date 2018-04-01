#' Set tmap mode to static plotting or interactive viewing
#' 
#' Set tmap mode to static plotting or interactive viewing. The global option \code{tmap.mode} determines the whether thematic maps are plot in the graphics device, or shown as an interactive leaflet map (see also \code{\link{tmap_options}}. The function \code{tmap_mode} is a wrapper to set this global option. The convenient function \code{ttm} is a toggle switch between the two modes. Tip 1: use \code{tmap_mode} in scripts and \code{ttm} in the console. Tip 2: use \code{ttm} in combination with \code{\link{last_map}} to redraw the last map in the other mode.
#' 
#' @param mode one of
#' \describe{
#'    	\item{\code{"plot"}}{Thematic maps are shown in the graphics device. This is the default mode, and supports all tmap's features, such as small multiples (see \code{\link{tm_facets}}) and extensive layout settings (see \code{\link{tm_layout}}). It is recommended for saving static maps (see \code{\link{tmap_save}}).} 
#'    	\item{\code{"view"}}{Thematic maps are viewed interactively in the web browser or RStudio's Viewer pane. Maps are fully interactive with tiles from OpenStreetMap or other map providers. See \code{\link{tm_view}} for options related to the \code{"view"} mode. This mode generates a \code{\link[leaflet:leaflet]{leaflet}} widget, which can also be directly obtained with \code{\link{tmap_leaflet}}. With RMarkdown, it is possible to publish it to an HTML page. 
#'    	There are a couple of contraints in comparison to \code{"plot"}:
#'    	\itemize{
#'    	\item The map is always projected accoring to the Web Mercator projection. Although this projection is the de facto standard for interactive web-based mapping, it lacks the equal-area property, which is important for many thematic maps, especially choropleths (see examples from \code{\link{tm_shape}}).
#'    	\item Small multiples are not supported
#'    	\item The legend cannot be made for aesthetics regarding size, which are symbol size and line width.
#'    	\item Text labels are not supported (yet)
#'    	\item The layout options set with \code{\link{tm_layout}}) regarding map format are not used. However, the styling options still apply.}
#'    	}}
#' @return the mode before changing
#' @example ./examples/tmap_mode.R
#' @seealso \href{../doc/tmap-modes.html}{\code{vignette("tmap-modes")}}, \code{\link{last_map}} to show the last map, \code{\link{tm_view}} for viewing options, and \code{\link{tmap_leaflet}} for obtaining a leaflet widget, and \code{\link{tmap_options}} for tmap options.
#' @export
tmap_mode <- function(mode=c("plot", "view")) {
	current.mode <- getOption("tmap.mode")
	if (is.null(match.call(expand.dots = TRUE)[-1])) {
		message("current tmap mode is \"", current.mode, "\"")
	} else {
		mode <- match.arg(mode)
		options(tmap.mode=mode)
		if (mode=="plot") {
			message("tmap mode set to plotting")
		} else {
			message("tmap mode set to interactive viewing")
		}
	}
	invisible(current.mode)
}	

#' @rdname tmap_mode
#' @export
ttm <- function() {
	current.mode <- getOption("tmap.mode")
	tmap_mode(ifelse(current.mode=="plot", "view", "plot"))
	invisible(current.mode)
}

#' Set the default tmap style
#' 
#' Set the default tmap style, which is contained in the global option \code{tmap.style} (see also \code{\link{tmap_options}}. The default style (i.e. when loading the package) is \code{"white"}.
#' 
#' @param style name of the style. The function \code{tm_style_<style>} should exist and be a wrapper of \code{\link{tm_layout}}. The default style when loading the package is \code{"white"}, which corresponds to the function \code{\link{tm_style_white}}. See \code{\link{tm_layout}} for predefined styles, and \code{\link{style_catalogue}} for creating a catelogue.
#' @return the style before changing
#' @seealso \code{\link{tm_layout}} for predefined styles, \code{\link{tmap_style_catalogue}} to create a style catelogue of all available styles, and \code{\link{tmap_options}} for tmap options.
#' @example ./examples/tmap_style.R
#' @seealso \code{\link{tmap_options}} for tmap options
#' @export
tmap_style <- function(style) {
	current.style <- getOption("tmap.style")
	
	if (missing(style)) {
		message("current tmap style is \"", current.style, "\"")
	} else {
		.tmapOptions <- get(".tmapOptions", envir = .TMAP_CACHE)	
		check_style(style)
		options(tmap.style=style)
		if (style == "white") {
			.tmapOptions <- .defaultTmapOptions
		} else {
			styleOptions <- .tmapStyles[[style]]
			.tmapOptions[names(styleOptions)] <- styleOptions
		}
			
		assign(".tmapOptions", .tmapOptions, envir = .TMAP_CACHE)	
		message("tmap style set to \"", style, "\"")
	}
	invisible(current.style)
}






check_style <- function(style) {
	obs <- c(ls(envir = .GlobalEnv), ls("package:tmap"))
	fname <- paste("tm_style", style, sep="_")
	if (!fname %in% obs) stop("current style \"" , style, "\" unknown, i.e. the function \"" , fname, "\" does not exist.", call. = FALSE)
}

check_mode <- function(mode) {
	if (!mode %in% (c("plot", "view"))) stop("incorrect mode", call. = FALSE)
}

check_limits <- function(limits) {
	if (!is.vector(limits)) stop("limits should be a vector", call. = FALSE)
	if (!is.numeric(limits)) stop("limits should be a numeric vector", call. = FALSE)
	if (!(length(limits)==2)) stop("limits should consist of two numbers", call. = FALSE)
	if (is.null(names(limits))) {
		names(limits) <- c("facets.plot", "facets.view")
	} else {
		if (!setequal(names(limits), c("facets.plot", "facets.view"))) stop("incorrect limits names: should be facets.view and facets.plot")
		limits <- limits[c("facets.plot", "facets.view")]
	}
	limits
}

check_unit <- function(unit) {
	if (!unit %in% c("metric", "imperial", "km", "m", "mi", "miles", "ft", "feet")) stop("incorrect unit", call. = FALSE)
}