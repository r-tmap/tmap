#' Read Open Street Map data (prototype)
#' 
#' Read Open Street Map data, either vector or raster based.
#' 
#' @param x bounding box or \code{\link[osmar:osmar]{osmar}} object
#' @param raster logical that determines whether a raster or a vector shapes are returned
#' @param poly specifies the polygons selection}}
#' @param point specifies the points selection}}
#' @param line specifies the lines selection}}
#' @param ... arguments passed on to \code{\link[OpenStreetMap:openmap]{openmap}}, most importantly, \code{zoom} and \code{type}
#' @import osmar
#' @import OpenStreetMap
#' @import raster
#' @import sp
#' @import rgdal
#' @export
read_osm <- function(x, raster=FALSE, poly=NULL, point=NULL, line=NULL, ...) {
	if (raster) {
		require(rgdal)
		require(raster)
		om <- openmap(x[c(4,1)], x[c(2,3)])
		omr <- raster::raster(om)
		oms <- as(omr, "SpatialGridDataFrame")
		oms@data <- data.frame(PIXEL__COLOR = rgb(oms$layer.1, oms$layer.2, oms$layer.3, maxColorValue=255))
		return(oms)
	} else {
		if (inherits(x,  "osmar")) {
			osm_obj <- x
		} else {
			src <- osmsource_api()
			osm_bb <- do.call("corner_bbox", as.list(as.vector(x)))
			osm_obj <- get_osm(osm_bb, source = src)
		}

		if (missing(poly) && missing(point) && missing(line)) stop("Please define poly, point, or line. Alternatively, set raster=TRUE")
		
		if (!missing(poly)) {
			ids <- find(osm_obj, way(tags(k==poly)))
			idLst <- find_down(osm_obj, way(ids))
			sbs <- subset(osm_obj, ids = idLst)
			sp_poly <- as_sp(sbs, "polygons")
		} else {
			sp_poly <- NULL
		}
		
		if (!missing(line)) {
			ids <- find(osm_obj, way(tags(k==line)))
			idLst <- find_down(osm_obj, way(ids))
			sbs <- subset(osm_obj, ids = idLst)
			sp_line <- as_sp(sbs, "lines")
		} else {
			sp_line <- NULL
		}
		
		if (!missing(point)) {
			ids <- find(osm_obj, node(tags(k == point)))
			sbs <- subset(osm_obj, node_ids = ids)
			sp_point <- as_sp(sbs, "points")
		} else {
			sp_point <- NULL
		}
		
	}
	shps <- list(poly=sp_poly, line=sp_line, point=sp_point)
	shps <- shps[!sapply(shps, is.null)]
	if (length(shps)==1) {
		shps[[1]]
	} else shps
}