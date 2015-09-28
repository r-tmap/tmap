#' Combine shape objects
#' 
#' Combine shape objects into one shape object. It works analogous to \code{\link{rbind}}.
#' 
#' @param ... shape objects. Each shape object is one of
#' \enumerate{
#'  \item{\code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygons(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}}}
#'  \item{\code{\link[sp:SpatialLinesDataFrame]{SpatialLines(DataFrame)}}}
#' }
#' @return shape object
#' @import sp
#' @export
sbind <- function(...) {
	xlist <- list(...)
	xlist <- xlist[!sapply(xlist, is.null)]

	k <- length(xlist)
	x <- xlist[[1]]
	if (!length(xlist)) stop("No shape objects specified.")
	if (!all(sapply(xlist, class)==class(x))) stop("Objects have inconsistent classes")
	
    ids <- make.unique(unlist(lapply(xlist, get_IDs)), sep="_")
    
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
		polygons <- mapply(function(p, id){
		    p@ID <- id
		    p
		}, polygons, ids, SIMPLIFY=FALSE)
		shp <- SpatialPolygons(polygons, pO = plotOrder, 
							   proj4string = x@proj4string)
	} else if (inherits(x, "SpatialLines")) {
		lns <- lapply(xlist, slot, "lines")
		lines <- unname(do.call("c", lns))
        lines <- mapply(function(l, id){
            l@ID <- id
            l
        }, lines, ids, SIMPLIFY=FALSE)
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
