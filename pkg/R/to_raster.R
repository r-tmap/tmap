#' Bin spatial points to a raster
#' 
#' Bin spatial points to a raster. For each raster cell, the number of points are counted. Optionally, a factor variable can be specified by which the points are counts are split.
#' 
#' This function is a wrapper around \code{\link[raster:rasterize]{rasterize}}.
#' 
#' @param shp shape object. Either a \code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}} or a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}}.
#' @param nrow number of raster rows. If \code{NA}, it is automatically determined by \code{N} and the aspect ratio of \code{shp}.
#' @param ncol number of raster columns. If \code{NA}, it is automatically determined by \code{N} and the aspect ratio of \code{shp}.
#' @param N preferred number of raster cells.
#' @param by name of a data variable which should be a factor. The points are split and counted according to the levels of this factor.
#' @param to.Raster logical; should the output be a \code{\link[raster:Raster-class]{Raster}} object (\code{TRUE}), or a \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}} (\code{FALSE}). If \code{TRUE}, a \code{RasterBrick} is returned when \code{by} is specified, and a \code{RasterLayer} when \code{by} is unspecified.
#' @return A \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}}, or a \code{\link[raster:Raster-class]{Raster}} object when (\code{to.Raster=TRUE})
#' @export
#' @import raster
#' @example  ../examples/points_to_raster.R
#' @seealso \code{\link{poly_to_raster}}
points_to_raster <- function(shp, nrow=NA, ncol=NA, N=250000, by=NULL, to.Raster=FALSE) {
	if (!inherits(shp, "SpatialPoints")) stop("shp should be a SpatialPoints/Pixels(DataFrame)")
	
	# get shp metadata
	bbx <- bb(shp)
	prj <- shp@proj4string
	np <- length(shp)
	asp <- get_asp_ratio(shp)
	hasData <- "data" %in% names(attributes(shp))
	
	# convert to points if necessary
	if (inherits(shp, "SpatialPixels")) {
		shp <- as(shp, ifelse(hasData, "SpatialPointsDataFrame", "SpatialPoints"))
	}
	
	# determine grid size
	if (is.na(nrow) || is.na(ncol)) {
		nrow <- round(sqrt(N/asp))
		ncol <- round(N / nrow)
	}
	N <- nrow * ncol
	
	# create empty raster	
	r <- raster(extent(bbx), nrows=nrow, ncols=ncol, crs=prj)
	
	# process by variable
	if (missing(by)) {
		var <- factor(rep(1L, np), labels="count")
	} else {
		var <- shp[[by]]
		if (!is.factor(var)) stop("by variable is not a factor")
	}
	lvls <- make.names(levels(var))
	names(lvls) <- lvls
	levels(var) <- lvls
	
	shps <- split(shp, f=var)
	
	res <- as.data.frame(lapply(shps, function(s) {
		s$ones <- 1
		rst <- rasterize(s, r, field="ones", fun='count')
		rst@data@values
	}))
	rshp <- SpatialGridDataFrame(as(r, "SpatialGrid"), data=res)

	# return Raster object or SGDF
	if (to.Raster) {
		if (ncol(rshp@data)==1) {
			as(rshp, "RasterLayer")
		} else {
			as(rshp, "RasterBrick")
		}
	} else rshp
}

#' Convert spatial polygons to a raster
#' 
#' Convert spatial polygons to a raster. For each raster cell, the data of the corresponding polygon is copied.
#' 
#' @param shp shape object. Either a \code{\link[sp:SpatialPointsDataFrame]{SpatialPoints(DataFrame)}} or a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}}.
#' @param nrow number of raster rows. If \code{NA}, it is automatically determined by \code{N} and the aspect ratio of \code{shp}.
#' @param ncol number of raster columns. If \code{NA}, it is automatically determined by \code{N} and the aspect ratio of \code{shp}.
#' @param N preferred number of raster cells.
#' @param use.cover logical; should the cover method be used? This method determines per raster cell which polygon has the highest cover fraction. This method is better, but very slow, since N times the number of polygons combinations are processed (using the \code{getCover} argument of \code{\link[raster:rasterize]{rasterize}}). By default, when a raster cell is covered by multiple polygons, the last polygon is taken (see \code{fun} argment of \code{\link[raster:rasterize]{rasterize}}))
#' @param to.Raster logical; should the output be a \code{\link[raster:Raster-class]{Raster}} object (\code{TRUE}), or a \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}} (\code{FALSE}). If \code{TRUE}, a \code{RasterBrick} is returned when \code{by} is specified, and a \code{RasterLayer} when \code{by} is unspecified.
#' @param arguments passed on to \code{\link[raster:rasterize]{rasterize}}
#' @return A \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}}, or a \code{\link[raster:Raster-class]{Raster}} object when (\code{to.Raster=TRUE})
#' @export
#' @import raster
#' @example  ../examples/poly_to_raster.R
#' @seealso \code{\link{points_to_raster}}
poly_to_raster <- function(shp, nrow=NA, ncol=NA, N=250000, use.cover=FALSE, to.Raster=FALSE, ...) {
	if (!inherits(shp, "SpatialPolygons")) stop("shp should be a SpatialPolygons(DataFrame)")
	
	# get shp metadata
	bbx <- bb(shp)
	np <- length(shp)
	asp <- get_asp_ratio(shp)
	hasData <- "data" %in% names(attributes(shp))
	
	# determine grid size
	if (is.na(nrow) || is.na(ncol)) {
		nrow <- round(sqrt(N/asp))
		ncol <- round(N / nrow)
	}
	N <- nrow * ncol
	
	# create empty raster	
	r <- raster(extent(bbx), nrows=nrow, ncols=ncol)
	
	# add ID data variable
	if (!hasData) {
		shp <- SpatialPolygonsDataFrame(shp, data=data.frame(ID__UNITS = 1:np), match.ID=FALSE)
	} else {
		shp$ID__UNITS <- 1:np
	}
	
	# get shape data (including ID variable)
	d <- shp@data
	
	# create raster of ID values
	if (use.cover) {
		res <- do.call("cbind", lapply(1:5, function(i) {
			s <- shp[i, ]
			rst <- rasterize(s, r, field="ID__UNITS", getCover=TRUE, ...)
			rst@data@values
		}))
		IDs <- apply(res, MARGIN=1, which.max)
	} else {
		rst <- rasterize(shp, r, field="ID__UNITS", getCover=FALSE, ...)
		IDs <- rst@data@values
	}
	
	
	# convert to SGDF and append data
	rshp <- as(rst, "SpatialGridDataFrame")
	rshp@data <- d[match(IDs, d$ID__UNITS),]
	
	# remove temp variable, or rename it to ID
	if (hasData) {
		rshp$ID__UNITS <- NULL
	} else {
		names(rshp) <- "ID"
	}
	
	# return Raster object or SGDF
	if (to.Raster) {
		if (ncol(rshp@data)==1) {
			as(rshp, "RasterLayer")
		} else {
			as(rshp, "RasterBrick")
		}
	} else rshp
}
