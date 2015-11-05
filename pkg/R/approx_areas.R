#' Approximate area sizes of the shapes
#' 
#' Approximate the area sizes of the polygons either in 1) absolute numbers based on the polygon coordinates, 2) proportional numbers, 3) normalized numbers and 4) squared units (e.g. kilometers).
#' 
#' To approximate the sizes in squared units, either the \code{total.area} or the \code{unit.size} is required. Note that this method is an approximation, since it depends on the used projection and the level of detail of the SpatialPolygons object. Projections with equal-area property are highly recommended.
#'
#' @param shp shape object, i.e., a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}
#' @param unit one of
#' \describe{
#' 	\item{\code{"abs"}:}{Absolute numbers based on polygon coordinates.}
#' 	\item{\code{"prop"}:}{Proportional numbers. In other words, the total of the area sizes equals one.}
#' 	\item{\code{"norm"}:}{Normalized numbers. All area sizes are normalized to the largest area, of which the area size equals one.}
#' 	\item{other:}{For instance, "km", "m", or "miles". For this method, \code{total.area} or \code{unit.size} is required. In this case, the area sizes are returned in squared units, e.g., in squared kilometers.}}
#' 	The default method is \code{"km"}.
#' @param unit.size size of the unit in terms of coordinate units. The coordinate system of many projections is approximately in meters while thematic maps typically range many kilometers, so by default \code{unit="km"} and \code{unit.size=1000} (meaning 1 kilometer equals 1000 coordinate units).
#' @param total.area total area size of \code{shp} in number of squared units (by default kilometers). Useful if the total area of the \code{shp} differs from a reference total area value. 
#' @return Numeric vector of area sizes.
#' @example  ../examples/approx_areas.R
#' @importFrom rgeos gArea
#' @export
approx_areas <- function(shp, unit="km", unit.size=1000, total.area=NA) {
	x <- suppressWarnings(gArea(shp, byid = TRUE))
	if (is.na(total.area)) total.area <- sum(x)/(unit.size^2)
    denom <- switch(unit, norm=max(x), prop=sum(x), abs=1, sum(x)/total.area)
    x / denom
}