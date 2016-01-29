#' Options for the interactive tmap viewer
#' 
#' Set the options for the interactive tmap viewer. Some of these options can also be set with \code{\link{tm_layout}}, since they are style dependent (e.g., the choice of basemaps). The function \code{tm_view} overrides these options when specified.
#' 
#' @param alpha transparency parameter applied to whole map. By default, it is set to \code{0.7} if basemaps are used, and \code{1} otherwise.  
#' @param popup.all.data should only the aesthetic variables be shown in the popup windows, or all variables? By default \code{FALSE} unless no aesthetics are used.
#' @param na color for missing values. The default value of \code{NULL} means transparent. It overrides the \code{na} value of the \code{aes.color} in \code{\link{tm_layout}}.
#' @param basemaps vector of one or more names of baselayer maps, or a logical value. See \url{http://leaflet-extras.github.io/leaflet-providers/preview}. By default (\code{NA}), the choice of basemap(s) will be determined by the \code{basemaps} argument of \code{\link{tm_layout}}, which is style dependent. Use \code{TRUE} to select a large set of recommended basemaps. Use \code{FALSE} to omit basemap tiles.
#' @param bg.overlay value that determines whether the background should be colored. By default (\code{NA}), it is set to \code{\link{tm_layout}}'s argument \code{bg.overlay} (which is style dependent).
#' @param bg.overlay.alpha alpha transparency of \code{bg.overlay}. If \code{0}, no background is used, if \code{1} a solid background is used (and therefore, basemaps will be omitted). By default, this value is  set to \code{\link{tm_layout}}'s argument \code{bg.overlay.alpha} (which is style dependent).
#' @param bubble.size.fixed should bubble sizes be fixed while zooming?
#' @param dot.size.fixed should dot sizes be fixed while zooming?
#' @param set_bounds logical that determines whether maximum bounds are set, or a numeric vector of four values that specify the lng1, lat1, lng2, and lat2 coordinates (see \code{\link[leaflet:setMaxBounds]{setMaxBounds}}).
#' @example ../examples/tmap_mode.R
#' @export
tm_view <- function(alpha=NA,
					popup.all.data=FALSE,
					na=NA,
					basemaps=NA,
					bg.overlay=NA,
					bg.overlay.alpha=NA,
					bubble.size.fixed=FALSE,
					dot.size.fixed=TRUE,
					set_bounds=FALSE) {
	g <- list(tm_view=c(as.list(environment()), list(call=names(match.call(expand.dots = TRUE)[-1]))))
	class(g) <- "tm"
	g
}
