#' Split lines by polygons
#' 
#' Split a lines shape object by a polygon shape object. Data of the corresponding polygons is appended to the line segments (Experimental, see note)
#' 
#' @param shp.lines The shape object that contains the lines, i.e. a \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}}
#' @param shp.poly The shape object that contains the polygons, i.e. a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}
#' @param variables.lines Names of the variables of \code{shp.lines} that are appended to the split lines shape object.
#' @param variables.poly Names of the variables of \code{shp.poly} that are appended to the split lines shape object.
#' @export
#' @return Shape object with splitted lines, a \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}}
#' @note This function is still in experimental phase, which means that it may not be stable and it may be changed significantly in future versions. Moreover, it is unsure if it will stay in tmap; instead, it may be put in a different package, along with functions of similar tasks.
#' @import rgeos
split_lines_poly <- function(shp.lines, shp.poly, variables.lines, variables.poly) {
	shp <- gIntersection(shp.lines, shp.poly, byid=TRUE)
	y_id <- get_IDs(shp)
	lines_id <- get_IDs(shp.lines)
	poly_id <- get_IDs(shp.poly)
	
	y_spl <- strsplit(y_id, split=" ", fixed=TRUE)
	y_lines_id <- as.character(sapply(y_spl, function(x)x[1]))
	y_poly_id <- as.character(sapply(y_spl, function(x)x[2]))
	
	if (missing(variables.lines)) variables.lines <- names(shp.lines)
	if (missing(variables.poly)) variables.poly <- names(shp.poly)
	
	ydata <- cbind(shp.lines@data[match(y_lines_id, lines_id), variables.lines, drop=FALSE],
				   shp.poly@data[match(y_poly_id, poly_id), variables.poly, drop=FALSE])
	
	append_data(ydata, shp, fixed.order=TRUE)
}
# 
# #' Split lines by lines
# #' 
# #' Split a lines shape object by another lines shape object.
# #' 
# #' @param shp.lines The shape object that contains the lines
# #' @param shp.lines2 The shape object that contains the polygons
# #' @export
# #' @return shape object with splitted lines
# #' @import rgeos
# split_lines_lines <- function(shp.lines, shp.lines2) {
# 	shp.points <- gIntersection(shp.lines, shp.lines2, byid=TRUE)
# 	
# 	split_lines_points(shp.lines, shp.points)
# 	
# }
# 
# split_lines_points <- function(shp.lines, shp.points) {
# 	length(shp.lines)
# 	
# 	tm(shp.lines)
# 	
# 	shp.lines@data
# }
