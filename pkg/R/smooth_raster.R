#' Create a smooth spatial grid
#' 
#' Create a smooth spatial grid from a shape object, which can be spatial points, spatial polygons or another spatial grid. For this purpose, a 2D kernal density estimator is applied. This function is used by \code{\link{iso_dasymetric}}.
#' 
#' @param shp shape object of class \code{\link[sp:Spatial]{Spatial}} or \code{\link[raster:Raster-class]{Raster}}. Spatial points, polygons, and grids are supported. Spatial lines are not.
#' @param var variable name. Not needed for \code{\link[sp:SpatialPoints]{SpatialPoints}}. If missing, the first variable name is taken.
#' @param nrow number of rows in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param ncol number of rows in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param N preferred number of points in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param bandwidth single numeric value or vector of two numeric values that specifiy the bandwidth of the kernal density estimator. By default, it is determined by this formula: (3 * ncol / bounding_box_width, 3 * nrow / bounding_box_height).
#' @param weight single number that specifies the weight of a single point. Only applicable if \code{shp} is a \code{\link[sp:SpatialPoints]{SpatialPoints}} object.
#' @param bbx bounding box
#' @param cover.type character value that specifies the type of raster cover, in other words, how the boundaries are specified. Options: \code{"original"} uses the same boundaries as \code{shp} (default for polygons), \code{"smooth"} calculates a smooth boundary based on the 2D kernal density (determined by \code{\link{raster_cover}}), \code{"rect"} uses the bounding box of \code{shp} as boundaries (default for spatial points and grids).
#' @param output character value defining the output. Options: \code{"RasterLayer"}, \code{"SpatialGridDataFrame"}, and \code{"list"}. The latter is the same output as \code{\link[KernSmooth:bkde2D]{bkde2D}}. If a character vector of two or three of these values is specified, a list is returned.
#' @return see \code{output} argument.
#' @example ../examples/smooth_raster.R
#' @import raster
#' @import maptools
#' @import rgeos
#' @import KernSmooth
#' @export
smooth_raster <- function(shp, var=NULL, nrow=NA, ncol=NA, N=250000, bandwidth=NA, weight=1, bbx=NULL, cover.type=NA, output="RasterLayer") {
	if (missing(bbx)) bbx <- bb(shp)
	prj <- get_projection(shp)
	asp <- get_asp_ratio(shp)
	if (inherits(shp, c("SpatialPoints", "SpatialPolygons"))) {
		bbx <- bb(bbx, ext=-1.05)
		shp@bbox <- bbx
	}
	if (inherits(shp, "Raster")) {
		shp <- as(shp, "SpatialGridDataFrame")
	}

	## determine grid size
	if (inherits(shp, c("SpatialPoints", "SpatialPolygons"))) {
		if (is.na(nrow) || is.na(ncol)) {
			nrow <- round(sqrt(N/asp))
			ncol <- round(N / nrow)
		}
	} else {
		ncol <- shp@grid@cells.dim[1]
		nrow <- shp@grid@cells.dim[2]
	}
	N <- nrow * ncol

	if (is.na(bandwidth[1])) {
		bandwidth <- 3 * (bbx[,2] - bbx[,1]) / c(ncol, nrow)
	} else {
		# make sure bandwith is a vector of 2
		bandwidth <- rep(bandwidth, length.out=2)
	}
	
	r <- raster(extent(bbx), nrows=nrow, ncols=ncol, crs=prj)

	cover <- r
	
	if (is.na(cover.type)) cover.type <- ifelse(inherits(shp, "SpatialPolygons"), "original", "rect")
	if (cover.type=="original") {
		if (inherits(shp, "SpatialGrid")) {
			cover[] <- shp[[var]]
		} else {
			if (inherits(shp, "SpatialPoints")) {
				coverPoly <- gConvexHull(shp)
			} else if (inherits(shp, "SpatialPolygons")) {
				coverPoly <- gUnaryUnion(shp)
			}
			coverPoly@bbox <- bbx
			cover <- poly_to_raster(coverPoly, nrow = nrow, ncol = ncol, to.Raster = TRUE)
		}
	}  else if (cover.type=="smooth") {
		cover <- raster_cover(shp, var=var, bandwidth = bandwidth, output="RasterLayer")	
		cover[!cover[]] <- NA
	}
	
		
	if (inherits(shp, "SpatialPoints")) {
		co <- coordinates(shp)
		x <- bkde2D(co, bandwidth=bandwidth, gridsize=c(ncol, nrow), range.x=list(bbx[1,], bbx[2,]))
		
		# normalize
		x$fhat <- x$fhat * (length(shp) * weight / sum(x$fhat, na.rm=TRUE))
		var <- "count"
	} else {
		if (missing(var)) var <- names(shp)[1]
		
		if (inherits(shp, "SpatialPolygons")){
			shp <- poly_to_raster(shp, nrow = nrow, ncol=ncol)
		}
		
		m <- as.matrix(raster(shp, layer=var))
		x <- kde2D(m, bandwidth = bandwidth, gridsize=c(ncol, nrow), range.x=list(bbx[1,], bbx[2,]))
		
		# normalize
		x$fhat <- x$fhat * (sum(shp[[var]], na.rm=TRUE) / sum(x$fhat, na.rm=TRUE))
		
	}
	
	# fill raster values
	r[] <- as.vector(x$fhat[, ncol(x$fhat):1])
	names(r) <- var
	
	# apply cover
	if (cover.type!="rect") {
		r[is.na(cover[])] <- NA
	}

	names(output) <- output
	res <- lapply(output, function(out) {
		if (out == "RasterLayer") {
			r
		} else if (out == "SpatialGridDataFrame") {
			as(r, "SpatialGridDataFrame")
		} else if (out=="list") {
			x
		} else {
			warning("unknown output format")
			NULL
		}
	})
	if (length(output)==1) res[[1]] else res
}

