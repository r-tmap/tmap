#' Get ranges of the polygons
#' 
#' Get the ranges of the polyons in the current projection. Also the total range of the shape object is returned.
#' 
#' @param shp shape object, i.e. a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}
#' @param key variable name in the shape data that identifies the polygons. If not specified, the default ID is used (see \code{\link{get_IDs}}).
#' @return List containing a data.frame of ranges per polygon, and a range vector of the total shape object.
#' @export
get_polygon_ranges <- function(shp, key=NULL) {
	co <- lapply(shp@polygons, function(p) {
		co2 <- lapply(p@Polygons, function(pp) pp@coords)
		co2all <- do.call("rbind", co2)
		c(range(co2all[,1]), range(co2all[,2]))
	})
	
	lbs <- if (missing(key))  {
		get_IDs(shp)
	} else {
		stopifnot(key %in% names(shp))
		as.character(shp[[key]])
	}
	rng_dat <- as.data.frame(matrix(unlist(co), ncol=4, byrow=TRUE, dimnames=list(lbs, c("xmin", "xmax", "ymin", "ymax"))))
	rng <- c(xmin=min(rng_dat[[1]]),
			 xmax=max(rng_dat[[2]]),
			 ymin=min(rng_dat[[3]]),
			 ymax=max(rng_dat[[4]]))
	list(polygon.ranges=rng_dat,
		 total.range=rng)
}
