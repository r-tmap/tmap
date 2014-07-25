#' Create a double line
#' 
#' Create a double line from a single line.
#' 
#' @param shp The shape object that contains the lines (\code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}})
#' @param width Width between the double lines
#' @return SpatialLinesDataFrame
#' @export
double_line <- function(shp, width) {
	co <- coordinates(shp)
	
	lines <- mapply(function(x, id) {
		x2 <- lapply(x, function(y) {
			z <- cbind(y[-nrow(y),, drop=FALSE], y[-1,,drop=FALSE])
			
			as <- atan2(z[,4]-z[,2], z[,3]-z[,1])
			
			y <- as.data.frame(y)
			
			y$mangle <- c(as[1], pmean_angle(as[-length(as)], as[-1]), as[length(as)])
			
			y$x1 <- y[,1] + sin(y$mangle) * width
			y$y1 <- y[,2] - cos(y$mangle) * width
			y$x2 <- y[,1] - sin(y$mangle) * width
			y$y2 <- y[,2] + cos(y$mangle) * width
			
			list(Line(as.matrix(y[, c("x1", "y1")])),
				 Line(as.matrix(y[, c("x2", "y2")])))
		})
		
		lines1 <- lapply(x2, function(i)i[[1]])
		lines2 <- lapply(x2, function(i)i[[2]])
		
		list(Lines(lines1, ID=id),
			 Lines(lines2, ID=id))
	}, co, get_IDs(shp), SIMPLIFY=FALSE)
	
	
	lines1 <- lapply(lines, function(i)i[[1]])
	lines2 <- lapply(lines, function(i)i[[2]])
	
	list(SpatialLinesDataFrame(SpatialLines(lines1, proj4string=shp@proj4string), data=shp@data, match.ID=FALSE),
		 SpatialLinesDataFrame(SpatialLines(lines2, proj4string=shp@proj4string), data=shp@data, match.ID=FALSE))
}

mean_angle <- function(a1, a2) {
	x <- cos(a1) + cos(a2)
	y <- sin(a1) + sin(a2)
	atan2(y,x)
}

pmean_angle <- function(a1, a2) {
	if (!length(a1) || !length(a2)) return(NULL)
	mapply(mean_angle, a1, a2, SIMPLIFY=TRUE)
}

