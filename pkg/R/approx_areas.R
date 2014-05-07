#' Approximate area sizes of the shapes.
#' 
#' This function approximates the area sizes of the polygons either in 1) squared kilometers, 2) absolute numbers based on the polygon coordinates, 3) proportional numbers, 4) normalized numbers.
#' 
#' To approximate the sizes in squared kilometer, \code{total.area.km2} is required. Note that this method is an approximation, since it depends on the used projection and the level of detail of the SpatialPolygons object. Projections with equal-area property are highly recommended.
#'
#' @param shp shape object, i.e. a SpatialPolygons(DataFrame)
#' @param total.area.km2 total area size of \code{shp} in number of squared kilometers
#' @param units one of
#' \describe{
#' 	\item{\code{"km2"}:}{Squared kilometers. For this method, \code{total.area.km2} is required.}
#' 	\item{\code{"abs"}:}{Absolute numbers based on polygon coordinates. Note: these are just the \code{area} slots of the polygons.}
#' 	\item{\code{"prop"}:}{Proportional numbers. In other words, the total of the area sizes equals one.}
#' 	\item{\code{"norm"}:}{Normalized numbers. All area sizes are normalized to the largest area, of which the area size equals one.}}
#' 	The default method is \code{"abs"}, unless \code{total.area.km2} is specified (in that case, it obviously is \code{"km2"}).
#' @return numeric vector of area sizes
#' @export
approx_areas <- function(shp, total.area.km2=NA, units=NULL) {
	
	x <- sapply(slot(shp, "polygons"),
		   function(x) {
		   	sum(sapply(slot(x, "Polygons"), slot, "area") *
		   			ifelse(sapply(slot(x, "Polygons"), slot, "hole"), -1, 1))
	})
    #x <- sapply(slot(shp, "polygons"), function(x) x@area)
    if (missing(units)) units <- ifelse(is.na(total.area.km2), "abs", "km2")
    denom <- switch(units, norm=max(x), prop=sum(x), km2=sum(x)/total.area.km2, 1)
    x / denom
}