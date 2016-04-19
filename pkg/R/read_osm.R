#' Read Open Street Map data
#' 
#' Read Open Street Map data. Either OSM tiles are read and returned as a spatial raster, or vectorized OSM data are queried and returned as spatial polygons, lines, and/or points.
#' 
#' @param x shape, bounding box, or \code{\link[osmar:osmar]{osmar}} object. If a shape (from class \code{\link[sp:Spatial]{Spatial}} or \code{\link[raster:Raster-class]{Raster}}) is specified, the bounding box of it is taken. See also \code{...} (other arguments). If a bounding box is specified, it should be in longitude latitude coordinates.
#' @param raster logical that determines whether a raster or vector shapes are returned. In the latter case, specify the vector selections (see argument \code{...}). By default, \code{raster=TRUE} if no vector selections are made, and \code{raster=FALSE} otherwise.
#' @param zoom passed on to \code{\link[OpenStreetMap:openmap]{openmap}}. Only applicable when \code{raster=TRUE}.
#' @param type passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param minNumTiles passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param mergeTiles passed on to \code{\link[OpenStreetMap:openmap]{openmap}} Only applicable when \code{raster=TRUE}.
#' @param ... arguments passed on to \code{\link{bb}} in case \code{x} is a shape, or arguments that specify polygons, lines, and/or points queries, created with \code{osm_poly}, \code{osm_line}, and \code{osm_point} respectively.
#' @name read_osm
#' @rdname read_osm
#' @import sp
#' @importFrom osmar osmsource_api get_osm corner_bbox find way tags find_down node as_sp
#' @importFrom raster raster
#' @export
#' @example ../examples/read_osm.R
#' @return The output of \code{read_osm} is a \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}} if \code{raster=TRUE}, and otherwise a named list of \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}, \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}}, and/or \code{\link[sp:SpatialPointsDataFrame]{SpatialPointsDataFrame}} objects. The names of this list are the names of arguments defined at \code{...}.
read_osm <- function(x, raster=NA, zoom=NULL, type=NULL, minNumTiles=NULL, mergeTiles=NULL, ...) {
	if (!working_internet()) stop("No internet connection found.")
	
	# @importFrom OpenStreetMap openmap
	k <- v <- NULL
	args <- list(...)
	
	args_bb <- args[intersect(names(args), c("ext", "cx", "cy", "width", "height", "xlim", "ylim", "relative"))]
	args_other <- args[setdiff(names(args), names(args_bb))]
	if (is.na(raster)) raster <- (length(args_other)==0)
	if (inherits(x, c("Spatial", "Raster"))) x <- do.call("bb", c(list(x=x, projection = "longlat"), args_bb))

	if (raster) {
		if (!requireNamespace("OpenStreetMap", quietly = TRUE)) {
			stop("OpenStreetMap package needed for this function to work. Please install it.",
				 call. = FALSE)
		} else {
			openmap <- get("openmap", envir=asNamespace("OpenStreetMap"), mode="function")
			optionalArgs <- list(zoom=zoom, type=type, minNumTiles=minNumTiles, mergeTiles=mergeTiles)
			optionalArgs <- optionalArgs[!sapply(optionalArgs, is.null)]
			om <- do.call("openmap", args = c(list(upperLeft=x[c(4,1)], lowerRight=x[c(2,3)]), optionalArgs))
			omr <- raster(om)
			oms <- as(omr, "SpatialGridDataFrame")
			oms@data <- raster_colors(oms)
			attr(oms, "is.OSM") <- TRUE
			return(oms)
		}
	} else {
		if (inherits(x,  "osmar")) {
			osm_obj <- x
		} else {
			src <- osmsource_api()
			osm_bb <- do.call("corner_bbox", as.list(as.vector(x)))
			osm_obj <- get_osm(osm_bb, source = src)
		}
		
		
		if (length(args_other)==0) stop("Please specify at least one vector query", call. = FALSE)
		
		shps <- lapply(args_other, function(a) {
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
		names(shps) <- names(args_other)
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
