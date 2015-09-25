#' Create contour lines (isolines) and the corresponding polygons
#' 
#' Create contour lines (isolines) from a shape object. To make the iso lines smooth, a 2D kernal density estimator is applied on the shape object. These lines are used to draw an isopleth. Also, the polygons between the countour lines are returned. They can be used to create a dasymetric map.
#' 
#' @param shp shape object of class \code{\link[sp:Spatial]{Spatial}} or \code{\link[raster:Raster-class]{Raster}}. Spatial points, polygons, and grids are supported. Spatial lines are not.
#' @param var variable name. Not needed for \code{\link[sp:SpatialPoints]{SpatialPoints}}. If missing, the first variable name is taken.
#' @param nrow number of rows in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param ncol number of rows in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param N preferred number of points in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param nlevels preferred number of levels
#' @param style method to cut the color scale: e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param bandwidth single numeric value or vector of two numeric values that specifiy the bandwidth of the kernal density estimator. By default, it is determined by this formula: (3 * ncol / bounding_box_width, 3 * nrow / bounding_box_height).
#' @param cover \code{\link[sp:SpatialPolygons]{SpatialPolygons}} shape that determines the covered area in which the contour lines are placed. By default (\code{NA}), it is based on \code{shp}: if \code{shp} are spatial points, the bounding box is taken, if \code{shp} are spatial polygons, the union of those polygons are taken, and if \code{shp} is a spatial grid, then a 2D kernal density estimator is applied to all non-missing values are taken. The latter uses the function \code{\link{raster_cover}}. Set \code{cover=NULL} to disable the cover, i.e. the contour lines are placing inside the bounding box of \code{shp}.
#' @param cover.type character value that specifies the type of raster cover, in other words, how the boundaries are specified. Options: \code{"original"} uses the same boundaries as \code{shp} (default for polygons), \code{"smooth"} calculates a smooth boundary based on the 2D kernal density (determined by \code{\link{raster_cover}}), \code{"rect"} uses the bounding box of \code{shp} as boundaries (default for spatial points and grids).
#' @param weight single number that specifies the weight of a single point. Only applicable if \code{shp} is a \code{\link[sp:SpatialPoints]{SpatialPoints}} object.
#' @param ... argument passed on to other functions (such as \code{\link{raster_cover}})
#' @return list of two items: \code{iso}, a \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}} containing the contour lines, and \code{dasy}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} containing the polygons between the contour lines.
#' @import raster
#' @import maptools
#' @import rgeos
#' @export
smooth_shape <- function(shp, var=NULL, nrow=NA, ncol=NA, N=250000, nlevels=5, style = ifelse(is.null(breaks), "pretty", "fixed"), breaks = NULL, bandwidth=NA, cover=NA, cover.type=NA, weight=1, ...) {
	bbx <- bb(shp)
	prj <- get_projection(shp)
#	asp <- get_asp_ratio(shp)

	if (!inherits(shp, c("SpatialPoints", "SpatialPolygons", "SpatialGrid", "Raster"))) {
		stop("shp is not a Raster nor a SpatialPoints, -Polygons, or -Grid object")
	}
		
	## determine bounding box and grid size
	if (inherits(shp, c("SpatialPoints", "SpatialPolygons"))) {
		bbx <- bb(bbx, ext=-1.05)
		shp@bbox <- bbx
		asp <- get_asp_ratio(shp)
		if (is.na(nrow) || is.na(ncol)) {
			nrow <- round(sqrt(N/asp))
			ncol <- round(N / nrow)
		}
		
	} else {
		if (inherits(shp, "Raster")) shp <- as(shp, "SpatialGridDataFrame")
		ncol <- shp@grid@cells.dim[1]
		nrow <- shp@grid@cells.dim[2]
	}
	N <- nrow * ncol

	# edit bandwidth
	if (is.na(bandwidth[1])) {
		bandwidth <- 3 * (bbx[,2] - bbx[,1]) / c(ncol, nrow)
	} else {
		# make sure bandwith is a vector of 2
		bandwidth <- rep(bandwidth, length.out=2)
	}

	cover_r <- raster(extent(bbx), nrows=nrow, ncols=ncol, crs=prj)
	
		
	## process cover
	if (is.na(cover.type)) cover.type <- ifelse(inherits(shp, "SpatialPolygons"), "original", "rect")
	if (missing(cover)) {
			
		if (cover.type=="rect") {
			cover <- as(extent(bbx), "SpatialPolygons")
			cover <- set_projection(cover, current.projection = prj)
			cover_r[] <- TRUE
		} else if (cover.type=="original") {
			if (inherits(shp, "SpatialGrid")) {
				cover_r[] <- shp[[var]]
			} else {
				if (inherits(shp, "SpatialPoints")) {
					cover <- gConvexHull(shp)
				} else if (inherits(shp, "SpatialPolygons")) {
					cover <- gUnaryUnion(shp)
				}
				cover@bbox <- bbx
				cover_r <- poly_to_raster(cover, nrow = nrow, ncol = ncol, to.Raster = TRUE)
			}
		}  else if (cover.type=="smooth") {
			cover_list <- raster_cover(shp, var=var, bandwidth = bandwidth, output=c("RasterLayer", "SpatialPolygons"))	
			cover_r <- cover_list$RasterLayer
			cover_r[!cover_r[]] <- NA
			cover <- cover_list$SpatialPolygons
		}
	} else {
		cover <- gUnaryUnion(cover)
		cover_r <- poly_to_raster(cover, nrow = nrow, ncol = ncol, to.Raster = TRUE)
		bbc <- bb(cover)
		bbx[, 1] <- pmin(bbx[, 1], bbc[, 1])
		bbx[, 2] <- pmin(bbx[, 2], bbc[, 2])
	}
	
	

	## create a smooth raster (using 2D-KDE)
	rlist <- smooth_raster(shp, var=var, nrow=nrow, ncol=ncol, bandwidth = bandwidth, weight=weight, bbx=bbx, output=c("RasterLayer", "list"))
	

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
	r <- raster(extent(bbx), nrows=nrow, ncols=ncol, crs=prj)
	r[] <- as.vector(x$fhat[, ncol(x$fhat):1])
	names(r) <- var
	
	# apply cover
	r[is.na(cover_r[])] <- NA

	lvls <- num2breaks(x$fhat, n=nlevels, style=style, breaks=breaks)$brks
	brks <- fancy_breaks(lvls, intervals=TRUE)
	
	cl <- contourLines(x$x1, x$x2, x$fhat, levels=lvls) 
	if (length(cl) > 10000) stop(paste("Number of iso lines over 10000:", length(cl)))
	cl2 <- ContourLines2SLDF(cl, proj4string = CRS(prj))
	#cl2$levelNR <- as.numeric(as.character(cl2$level))
	
	# make sure lines are inside poly
	cp <- lines2polygons(ply = cover, lns = cl2, rst = r, lvls=lvls, method="full")
	
	lns <- SpatialLinesDataFrame(gIntersection(cover, cl2, byid = TRUE), data=cl2@data, match.ID = FALSE)
	
	list(iso=lns, dasy=cp, raster=r)
}