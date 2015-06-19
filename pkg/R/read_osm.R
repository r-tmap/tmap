#' Read Open Street Map data (prototype)
#' 
#' Read Open Street Map data, either vector or raster based.
#' 
#' @param x bounding box or \code{\link[osmar:osmar]{osmar}} object
#' @param raster logical that determines whether a raster or vector shapes are returned. In the latter case, specify the vector selections (see argument \code{...})
#' @param zoom passed on to \code{\link[OpenStreetMap:openmap]{openmap}}. Only applicable when \code{raster=TRUE}.
#' @param type passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param minNumTiles passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param mergeTiles passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param ... arguments that specify polygons, lines, and/or points queries, created with \code{osm_poly}, \code{osm_line}, and \code{osm_point}.
#' @name read_osm
#' @rdname read_osm
#' @import osmar
#' @import OpenStreetMap
#' @import raster
#' @import sp
#' @import rgdal
#' @export
#' @example ../examples/read_osm.R
#' @return The output of \code{read_osm} is a \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}} if \code{raster=TRUE}, and otherwise a named list of \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}, \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}}, and/or \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} objects. The names of this list are the names of arguments defined at \code{...}.
read_osm <- function(x, raster=FALSE, zoom=NULL, type=NULL, minNumTiles=NULL, mergeTiles=NULL, ...) {
	if (raster) {
		require(rgdal)
		require(raster)
		optionalArgs <- list(zoom=zoom, type=type, minNumTiles=minNumTiles, mergeTiles=mergeTiles)
		optionalArgs <- optionalArgs[!sapply(optionalArgs, is.null)]
		om <- do.call("openmap", args = c(list(upperLeft=x[c(4,1)], lowerRight=x[c(2,3)]), optionalArgs))
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
		
		args <- list(...)
		
		if (length(args)==0) stop("Please specify at least one vector query")
		
		shps <- lapply(args, function(a) {
			if (a$unit=="poly") {
				if (a$key.only) {
					ids <- find(osm_obj, way(tags(k==a$k)))	
				} else {
					ids <- find(osm_obj, way(tags(k==a$k & v==a$v)))	
				}
				if (is.na(ids[1])) return(NULL)
				idLst <- find_down(osm_obj, way(ids))
				sbs <- subset(osm_obj, ids = idLst)
				as_sp(sbs, "polygons")
			} else if (a$unit=="line") {
				if (a$key.only) {
					ids <- find(osm_obj, way(tags(k==a$k)))	
				} else {
					ids <- find(osm_obj, way(tags(k==a$k & v==a$v)))	
				}
				if (is.na(ids[1])) return(NULL)
				idLst <- find_down(osm_obj, way(ids))
				sbs <- subset(osm_obj, ids = idLst)
				as_sp(sbs, "lines")
			} else {
				if (a$key.only) {
					ids <- find(osm_obj, node(tags(k==a$k)))	
				} else {
					ids <- find(osm_obj, node(tags(k==a$k & v==a$v)))	
				}
				if (is.na(ids[1])) return(NULL)
				sbs <- subset(osm_obj, node_ids = ids)
				as_sp(sbs, "points")
			}
		})
		names(shps) <- names(args)
		shps
	}
}

#' @param query query to select polygons, lines, or points. Currently, two formats are supported: 1) key, 2) key=value. See \url{http://wiki.openstreetmap.org/wiki/Map_Features} for Open Street Map keys and values.
#' @name osm_poly
#' @rdname read_osm
#' @export
osm_poly <- function(query) osm_type(query, "poly")

#' @name osm_line
#' @rdname read_osm
#' @export
osm_line <- function(query) osm_type(query, "line")

#' @name osm_point
#' @rdname read_osm
#' @export
osm_point <- function(query) osm_type(query, "point")

osm_type <- function(query, unit) {
	items <- strsplit(query, "=")[[1]]
	if (length(items)==1) {
		list(unit=unit, key.only=TRUE, k=items[1])
	} else {
		list(unit=unit, key.only=FALSE, k=items[1], v=items[2])
	}
}
