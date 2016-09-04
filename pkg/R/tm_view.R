#' Options for the interactive tmap viewer
#' 
#' Set the options for the interactive tmap viewer. Some of these options can also be set with \code{\link{tm_layout}}, since they are style dependent (e.g., the choice of basemaps). The function \code{tm_view} overrides these options when specified.
#' 
#' @param alpha transparency parameter applied to whole map. By default, it is set to \code{0.7} if basemaps are used, and \code{1} otherwise.  
#' @param popup.all.data should only the aesthetic variables be shown in the popup windows, or all variables? By default \code{TRUE} unless aesthetics are used and the corresponding \code{id} arguement is specified.
#' @param colorNA default color for missing values (that is, in case \code{colorNA} is unspecified in layer functions such as \code{\link{tm_fill}}). The default value of \code{NULL} means transparent. It overrides the \code{na} value of the \code{aes.color} in \code{\link{tm_layout}}.
#' @param basemaps vector of one or more names of baselayer maps, or a logical value. See \url{http://leaflet-extras.github.io/leaflet-providers/preview}. Also supports URL's for tile servers, such as \code{"http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png"}. By default (\code{NA}), the choice of basemap(s) will be determined by the \code{basemaps} argument of \code{\link{tm_layout}}, which is style dependent. Use \code{TRUE} to select a large set of recommended basemaps. Use \code{FALSE} to omit basemap tiles. If a named vector is provided, the names are used in the layer control legend.
#' @param bg.overlay value that determines whether the background should be colored. By default (\code{NA}), it is set to \code{\link{tm_layout}}'s argument \code{bg.overlay} (which is style dependent).
#' @param bg.overlay.alpha alpha transparency of \code{bg.overlay}. If \code{0}, no background is used, if \code{1} a solid background is used (and therefore, basemaps will be omitted). By default, this value is  set to \code{\link{tm_layout}}'s argument \code{bg.overlay.alpha} (which is style dependent).
#' @param symbol.size.fixed should symbol sizes be fixed while zooming?
#' @param dot.size.fixed should dot sizes be fixed while zooming?
#' @param set.bounds logical that determines whether maximum bounds are set, or a numeric vector of four values that specify the lng1, lat1, lng2, and lat2 coordinates (see \code{\link[leaflet:setMaxBounds]{setMaxBounds}}).
#' @param set.view numeric vector or three that determines the view: lng, lat, and zoom (see \code{\link[leaflet:setView]{setView}}).
#' @param set.zoom.limits numeric vector of two that set the minimum and maximum zoom levels (see \code{\link[leaflet:tileOptions]{tileOptions}}).
#' @param legend.position Character vector of two values, specifing the position of the legend. Use "left" or "right" for the first value and "top" or "bottom" for the second value. It overrides the value of \code{legend.position} of \code{\link{tm_layout}}, unless set to \code{NA}.
#' @param control.position Character vector of two values, specifing the position of the layer control UI. Use "left" or "right" for the first value and "top" or "bottom" for the second value.
#' @example ../examples/tm_view.R
#' @seealso \code{\link{tmap_mode}} and \href{../doc/tmap-modes.html}{\code{vignette("tmap-modes")}}
#' @export
tm_view <- function(alpha=NA,
					popup.all.data=FALSE,
					colorNA=NULL,
					basemaps=NA,
					bg.overlay=NA,
					bg.overlay.alpha=NA,
					symbol.size.fixed=FALSE,
					dot.size.fixed=TRUE,
					set.bounds=FALSE,
					set.view=NA,
					set.zoom.limits=NA,
					legend.position=c("right", "top"),
					control.position=c("left", "top")) {
	g <- list(tm_view=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tm"
	g
}
