#' Convert shape data
#' 
#' Convert numeric data from one polygon shape to another. It uses an intersection matrix, which stores the intersection ratios of the two shape objects per polygon (see \code{\link{intersection_shapes}).
#' 
#' @param shp.from the shape object to be converted. It should contain data.
#' @param shp.to the shape object to be converted into.
#' @param variables.from names of the numeric variables of \code{shp.from} to be converted. If missing, all numeric variables are taken.
#' @param variables.to variable names to be used. Should be the same number of variable names as \code{variables.from}
#' @import rgeos
#' @return shape object \code{shp.to} with converted data from \code{shp.from}
#' @export
convert_shape_data <- function(shp.from, shp.to, variables.from=NULL, variables.to=NULL) {
	x <- intersection_shapes(shp.from, shp.to)

	data.from <- shp.from@data
	if (missing(variables.from)) variables.from <- names(data.from)[sapply(data.from, is.numeric)]
	if (missing(variables.to)) variables.to <- variables.from
	y <- x %*% as.matrix(data.from[, variables.from])
	dimnames(y)[[2]] <- variables.to
	shp.to <- append_data(shp.to, y, fixed.order=TRUE)
	shp.to
}

#' Calculate intersection matrix between two polygon shapes
#' 
#' The intersection matrix contains all intersection ratios from the polygons of the first shape object to the polygons of the second shape object
#' 
#' @param shp.from the shape object to be converted. It should contain data.
#' @param shp.to the shape object to be converted into.
#' @import rgeos
#' @return intersection matrix
#' @export
intersection_shapes <- function(shp.from, shp.to) {
	polys.to <- shp.to@polygons
	polys.from <- shp.from@polygons
	sapply(polys.from, function(p.from) {
		sapply(polys.to, function(p.to) {
			gInt <- gIntersection(SpatialPolygons(list(p.to)), SpatialPolygons(list(p.from)))
			if (is.null(gInt)) return(0) else {
				if (inherits(gInt, "SpatialPolygons")) {
					area.from <- p.from@area
					area.int <- gInt@polygons[[1]]@area
					return(area.int/area.from)
				}
			}
			return(0)
		})
	})
}
