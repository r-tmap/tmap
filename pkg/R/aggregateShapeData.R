#' Aggregete shape data
#' 
#' Aggregates numeric data from a detailed shape object to a global shape object. It uses an intersection matrix, which stores the intersection ratios of each detailed polygon and each global polygon.
#' 
#' @param shp.from the (detailed) shape object to be aggregated.
#' @param shp.to the (global) shape object that defines the aggregates
#' @param variables variable names of \code{shp.to}. If missing, all numeric variables are taken.
#' 
#' @import rgeos
#' @export
aggregateShapeData <- function(shp.from, shp.to, variables=NULL) {
	polys.to <- shp.to@polygons
	polys.from <- shp.from@polygons
	x <- sapply(polys.from, function(p.from) {
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
	data.from <- shp.from@data
	if (missing(variables)) variables <- which(sapply(data.from, is.numeric))
	y <- x %*% as.matrix(data.from[, variables])
	shp.to <- appendData(shp.to, y, fixed.order=TRUE)
	shp.to
}