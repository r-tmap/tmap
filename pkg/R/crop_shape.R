#' Crop shape file
#' 
#' Crops a shape file, i.e. SpatialPolygons(DataFrame), to a rectangle which is by default its bounding box.
#' 
#' @param shp shape object. Either a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} or a  \code{\link[sp:SpatialPolygons]{SpatialPolygons}}
#' @param rectangle to crop the \code{shp} with. It is represented by a 2x2 matrix in which the x and y coordiantes are respectively row 1 and 2, and the minimum and maximum values are respectively column 1 and 2. By default the bounding box of \code{shp} is taken.
#' @return a cropped shape object. Its bounding box is set to \code{bb}. Data is retained in case \code{shp} is a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}. A vector of matched ID's is stored as a attribute \code{matchID}. This vector contains for each polygon in the returned shape object the number of its orignal polygon in \code{shp}.
#' @import rgeos
#' @export
crop_shape <- function(shp, bbox=shp@bbox) {

	bbcoords <- cbind(x=bbox[1,][c(1, 1, 2, 2, 1)], y=bbox[2,][c(1, 2, 2, 1, 1)])
	BB <- SpatialPolygons(list(Polygons(list(Polygon(bbcoords)), "1")),
						  proj4string=CRS(proj4string(shp)))
	shp2 <- gIntersection(shp, BB, byid=TRUE)
	
	shp2@bbox <- bbox
	
	ids <- get_IDs(shp)
	ids2 <- gsub(" [0-9]+$", "", get_IDs(shp2))
	indices <- match(ids2, ids)
	if (inherits(shp, "SpatialPolygonsDataFrame")) {
		shp2 <- SpatialPolygonsDataFrame(shp2, shp@data[indices, ], match.ID = FALSE)
	}
	attr(shp2, "matchID") <- indices
	shp2
}