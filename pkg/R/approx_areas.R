#' Approximate area sizes of the shapes
#' 
#' Approximate the area sizes of the polygons either in 1) squared kilometers, 2) absolute numbers based on the polygon coordinates, 3) proportional numbers, 4) normalized numbers.
#' 
#' To approximate the sizes in squared kilometer, \code{total.area.km2} is required. Note that this method is an approximation, since it depends on the used projection and the level of detail of the SpatialPolygons object. Projections with equal-area property are highly recommended.
#'
#' @param shp shape object, i.e. a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}
#' @param unit one of
#' \describe{
#' 	\item{\code{"abs"}:}{Absolute numbers based on polygon coordinates. Only usefull if the projection satisfies the equal-area property. Note: these are equal to the \code{area} slots of the polygons, where the \code{area} slots of the holes are subtracted. Also note that for many projections, the coordinate units are meters, so the area sizes correspond to squared meters (rather than squared kilometers).}
#' 	\item{\code{"prop"}:}{Proportional numbers. In other words, the total of the area sizes equals one.}
#' 	\item{\code{"norm"}:}{Normalized numbers. All area sizes are normalized to the largest area, of which the area size equals one.}
#' 	\item{other:}{For instance, "km", "m", or "miles". For this method, \code{total.area} and \code{unit.size} are required. In this case, the area sizes are returned in squared units, e.g., in squared kilometers.}}
#' 	The default method is \code{"abs"}, unless \code{total.area} is specified (in that case, it is \code{"km"}).
#' @param unit.size size in the coordinate system that corresponds to one \code{unit}. The coordinate system of many projections is in meters while thematic maps typically scan many kilometers, so by default \code{unit="km"} and \code{unit.size=1000} (meaning 1 kilometer equals 1000 meters).
#' @param total.area total area size of \code{shp} in number of squared units (by default kilometers). Useful if the total area of the \code{shp} deviates a reference total area value. 
#' @return Numeric vector of area sizes.
#' @example  ../examples/approx_areas.R
#' @export
approx_areas <- function(shp, unit="km", unit.size=1000, total.area=NA) {
	x <- gArea(shp, byid = TRUE)
	if (is.na(total.area)) total.area <- sum(x)/(unit.size^2)
    denom <- switch(unit, norm=max(x), prop=sum(x), abs=1, sum(x)/total.area)
    x / denom
}