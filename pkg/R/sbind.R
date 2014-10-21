#' Combine shape objects
#' 
#' Combine shape objects into one shape object. It works analogous to \code{\link{rbind}}.
#' 
#' @param ... shape objects. The objects should be of one of the following classes:
#' \itemize{
#'  \item{"1)"}\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}
#'  \item{"2)"}\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}
#'  \item{"3)"}\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}
#' }
#' @return shape object
#' @import sp
#' @export
sbind <- function(...) {
	xlist <- list(...)
	k <- length(xlist)
	
	x <- xlist[[1]]
	
	if (!all(sapply(xlist, class)==class(x))) stop("Objects have inconsistent classes")
	
	
	if (inherits(x, "SpatialPoints")) {
		crds <- lapply(xlist, slot, "coords")
		coords <- unname(do.call("rbind", crds))
		shp <- SpatialPoints(coords, proj4string = x@proj4string)
	} else if (inherits(x, "SpatialPolygons")) {
		polys <- lapply(xlist, slot, "polygons")
		polygons <- unname(do.call("c", polys))
		plotOrders <- lapply(xlist, slot, "plotOrder")
		add <- cumsum(c(0, sapply(plotOrders, length)[-k]))
		plotOrder <- as.integer(unlist(mapply("+", plotOrders, add, SIMPLIFY = FALSE)))
		shp <- SpatialPolygons(polygons, pO = plotOrder, 
							   proj4string = x@proj4string)
	} else if (inherits(x, "SpatialLines")) {
		lns <- lapply(xlist, slot, "lines")
		lines <- unname(do.call("c", lns))
		shp <- SpatialLines(lines, proj4string = x@proj4string)
	} else stop("Only spatial polygons, points, and lines are accepted.")
	
	if ("data" %in% slotNames(x)) {
		dfs <- lapply(xlist, slot, "data")
		if (!all(sapply(dfs[-1], function(d)identical(names(d), names(dfs[[1]]))))) {
			stop("Inconsistent data columns")
		}
		data <- do.call("rbind", dfs)
		
		if (inherits(x, "SpatialPolygonsDataFrame")) {
			shp <- SpatialPolygonsDataFrame(shp, data = data, match.ID = FALSE)		
		} else if (inherits(x, "SpatialPointsDataFrame")) {
			shp <- SpatialPointsDataFrame(shp, data = data, match.ID = FALSE)		
		} else if (inherits(x, "SpatialLinesDataFrame")) {
			shp <- SpatialLinesDataFrame(shp, data = data, match.ID = FALSE)		
		}
	}
	shp
}
