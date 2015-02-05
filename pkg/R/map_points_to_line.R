#' Map points to line
#' 
#' Map points to a polyline. For each point, the closest point on the line and the corresponding distance is determined. At the moment, it only considers the begin- joint-, and endpoint coordinates of a line, not their interpolations. To take the interpolations into account, use \code{\link{split_lines_equal}}. (Experimental, see note)
#'
#' @param shp.points shape object containing the points, i.e. a \code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}
#' @param shp.lines shape object containing the polylines, i.e. a \code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}
#' @param by.key should the points be mapped to the polylines by class (determined by \code{key.points} and \code{key.lines})?
#' @param key.points name of a variable contained in \code{shp.points} that determined the classes of the points. The points are mapped to the polylines class-wise.
#' @param key.lines name of a variable contained in \code{shp.lines} that determined the classes of the polylines. The points are mapped to the polylines class-wise.
#' @return data.frame, where the rows correspond to the points. The columns are: 1) id1, which is the Lines identifier in which the coordination point is contained, 2) id2, which is the Line identifier in which the coordination point is contained, 3) id3, which is the number of the closest coordination point in the polyline, 4) the distance with the closest coordination point.
#' @note This function is still in experimental phase, which means that it may not be stable and it may be changed significantly in future versions. Moreover, it is unsure if it will stay in tmap; instead, it may be put in a different package, along with functions of similar tasks.
#' @export
map_points_to_line <- function(shp.points, shp.lines, by.key=FALSE, key.points=NULL, key.lines=NULL) {
	
	if (by.key) {
		if (!is.factor(shp.points[[key.points]])) shp.points[[key.points]] <- factor(shp.points[[key.points]])
		
		shp.lines[[key.lines]] <- factor(as.character(shp.lines[[key.lines]]), levels=levels(shp.points[[key.points]]))
	}
	
	co <- coordinates(shp.lines)
	cp <- coordinates(shp.points)
	
	res3 <- lapply(1:length(shp.points), function(i) {
		
		coi <- cp[i,]
		
		if (by.key) {
			lev <- shp.points[[key.points]][i]
			sel <- which(shp.lines[[key.lines]]==lev)
		} else sel <- 1:length(shp.lines)
		col <- co[sel]
		
		res2 <- lapply(col, function(x) {
			res <- lapply(x, function(y) {
				y <- as.data.frame(cbind(y, x1=coi[1], x2=coi[2]))
				y$d <- sqrt((y[,3]-y[,1])^2 + (y[,4]-y[,2])^2)
				wm <- which.min(y$d)[1]
				data.frame(id3=wm, d=y$d[wm])
			})
			res <- do.call("rbind", res)
			res$id2 <- 1:length(x)
			res[which.min(res$d)[1], ]
		})
		pdat <- do.call("rbind", res2)
		
		pdat$id1 <- sel
		pdat[which.min(pdat$d)[1], c("id1", "id2", "id3", "d")]
	})
	res3 <- do.call("rbind", res3)
	row.names(res3) <- NULL
	res3
}
