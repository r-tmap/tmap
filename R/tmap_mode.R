#' Set tmap mode to static plotting or interactive viewing
#' 
#' Set tmap mode to static plotting or interactive viewing. The global option \code{tmap.mode} determines the whether thematic maps are plot in the graphics device, or shown as an interactive leaflet map (see also \code{\link{tmap_options}}. The function \code{tmap_mode} is a wrapper to set this global option. The convenient function \code{ttm}, which stands for toggle thematic map, is a toggle switch between the two modes. The function \code{ttmp} stands for toggle thematic map and print last map: it does the same as \code{ttm} followed by \code{tmap_last}; in order words, it shows the last map in the other mode. It is recommended to use \code{tmap_mode} in scripts and \code{ttm}/\code{ttmp} in the console.
#' 
#' @param mode one of
#' \describe{
#'    	\item{\code{"plot"}}{Thematic maps are shown in the graphics device. This is the default mode, and supports all tmap's features, such as small multiples (see \code{\link{tm_facets}}) and extensive layout settings (see \code{\link{tm_layout}}). It is recommended for saving static maps (see \code{\link{tmap_save}}).} 
#'    	\item{\code{"view"}}{Thematic maps are viewed interactively in the web browser or RStudio's Viewer pane. Maps are fully interactive with tiles from OpenStreetMap or other map providers (see \code{\link{tm_tiles}}). See also \code{\link{tm_view}} for options related to the \code{"view"} mode. This mode generates a \code{\link[leaflet:leaflet]{leaflet}} widget, which can also be directly obtained with \code{\link{tmap_leaflet}}. With RMarkdown, it is possible to publish it to an HTML page. 
#'    	There are a couple of constraints in comparison to \code{"plot"}:
#'    	\itemize{
#'    	\item The map is always projected according to the Web Mercator projection. Although this projection is the de facto standard for interactive web-based mapping, it lacks the equal-area property, which is important for many thematic maps, especially choropleths (see examples from \code{\link{tm_shape}}).
#'    	\item Small multiples are not supported
#'    	\item The legend cannot be made for aesthetics regarding size, which are symbol size and line width.
#'    	\item Text labels are not supported (yet)
#'    	\item The layout options set with \code{\link{tm_layout}}) regarding map format are not used. However, the styling options still apply.}
#'    	}}
#' @return the mode before changing
#' @example ./examples/tmap_mode.R
#' @seealso \href{../doc/tmap-getstarted.html}{\code{vignette("tmap-getstarted")}}, \code{\link{tmap_last}} to show the last map, \code{\link{tm_view}} for viewing options, and \code{\link{tmap_leaflet}} for obtaining a leaflet widget, and \code{\link{tmap_options}} for tmap options.
#' @references Tennekes, M., 2018, {tmap}: Thematic Maps in {R}, Journal of Statistical Software, 84(6), 1-39, \href{https://doi.org/10.18637/jss.v084.i06}{DOI}
#' @export
tmap_mode <- function(mode=c("plot", "view")) {
	current.mode <- getOption("tmap.mode")
	show.messages <- get("tmapOptions", envir = .TMAP_CACHE)$show.messages
	
	if (is.null(match.call(expand.dots = TRUE)[-1])) {
		message("current tmap mode is \"", current.mode, "\"")
	} else {
		mode <- match.arg(mode)
		options(tmap.mode=mode)
		if (show.messages) {
			if (mode=="plot") {
				message("tmap mode set to plotting")
			} else {
				message("tmap mode set to interactive viewing")
			}
		}
	}
	invisible(current.mode)
}	

#' Toggle design mode
#' 
#' When the so-called "design mode" is enabled, inner and outer margins, legend position, and aspect ratio are shown explicitly in plot mode. Also, information about aspect ratios is printed in the console. This function toggles the tmap option `design.mode`.
#' 
#' @seealso \code{\link{tmap_options}}
#' @export
tmap_design_mode = function() {
	dm = get("tmapOptions", envir = .TMAP_CACHE)$design.mode
	tmap_options(design.mode = !dm)
	message("design.mode: ", if (dm) "OFF" else "ON", if ((!dm) && getOption("tmap.mode") == "view") " (only effective in plot mode)" else "")
}


#' @rdname tmap_mode
#' @export
ttm <- function() {
	current.mode <- getOption("tmap.mode")
	tmap_mode(ifelse(current.mode=="plot", "view", "plot"))
	invisible(current.mode)
}

#' @rdname tmap_mode
#' @export
ttmp <- function() {
	ttm()
	tmap_last()
}

check_mode <- function(mode) {
	if (!mode %in% (c("plot", "view"))) stop("incorrect mode", call. = FALSE)
}


check_unit <- function(unit) {
	if (!unit %in% c("metric", "imperial", "km", "m", "mi", "miles", "ft", "feet")) stop("incorrect unit", call. = FALSE)
}