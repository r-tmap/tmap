#' Approximate area sizes of the shapes.
#' 
#' This function approximates the area sizes of the polygons either in 1) squared kilometers, 2) absolute numbers based on the polygon coordinates, 3) proportional numbers, 4) normalized numbers.
#' 
#' To approximate the sizes in squared kilometer, \code{total.area.km2} is required. Note that this method is an approximation, since it depends on the used projection and the level of detail of the SpatialPolygons object. Projections with equal-area property are highly recommended.
#'
#' @param shp shape object, i.e. a SpatialPolygons(DataFrame)
#' @param total.area.km2 total area size of \code{shp} in number of squared kilometers. If \code{NA}, and \code{unit="km2"}, then the polygon coordinates are assumed to be in meters.
#' @param units one of
#' \describe{
#' 	\item{\code{"km2"}:}{Squared kilometers. For this method, \code{total.area.km2} is required. In this case, the area sizes are also returned in squared kilometers.}
#' 	\item{\code{"abs"}:}{Absolute numbers based on polygon coordinates. Only usefull if the projection satisfies the equal-area property. Note: these are equal to the \code{area} slots of the polygons, where the \code{area} slots of the holes are subtracted. Also note that for many projections, the coordinate units are meters, so the area sizes correspond to squared meters (rather than squared kilometers).}
#' 	\item{\code{"prop"}:}{Proportional numbers. In other words, the total of the area sizes equals one.}
#' 	\item{\code{"norm"}:}{Normalized numbers. All area sizes are normalized to the largest area, of which the area size equals one.}}
#' 	The default method is \code{"abs"}, unless \code{total.area.km2} is specified (in that case, it is \code{"km2"}).
#' @return numeric vector of area sizes
#' @example  ../examples/approx_areas.R
#' @export
approx_areas <- function(shp, total.area.km2=NA, units=NULL) {
	x <- sapply(slot(shp, "polygons"),
		   function(x) {
		   	sum(sapply(slot(x, "Polygons"), slot, "area") *
		   			ifelse(sapply(slot(x, "Polygons"), slot, "hole"), -1, 1))
	})
    #x <- sapply(slot(shp, "polygons"), function(x) x@area)
    if (missing(units)) units <- ifelse(is.na(total.area.km2), "abs", "km2")
	if (is.na(total.area.km2)) total.area.km2 <- sum(x)/1e6
    denom <- switch(units, norm=max(x), prop=sum(x), km2=sum(x)/total.area.km2, 1)
    x / denom
}