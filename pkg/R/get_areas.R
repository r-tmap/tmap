#' Determine area sizes of the shapes.
#' 
#' This function calculates the area sizes of the shapes. This is done by summing all polygons area sizes of the shape object that are not a hole, and substracting the area sized of shape objects that are holes.
#'
#' @param shp shape object, i.e. a SpatialPolygons(DataFrame)
#' @return numeric vector of area sizes
#' @export
get_areas <- function(shp) {
    sapply(slot(shp, "polygons"),
           function(x) {
           	x@area
#                sum(sapply(slot(x, "Polygons"), slot, "area")*
#                        (.5-sapply(slot(x, "Polygons"), slot, "hole"))*2)
           })
}