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
#' @param weight single number that specifies the weight of a single point. Only applicable if \code{shp} is a \code{\link[sp:SpatialPoints]{SpatialPoints}} object.
#' @param ... argument passed on to other functions (such as \code{\link{raster_cover}})
#' @return list of two items: \code{iso}, a \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}} containing the contour lines, and \code{dasy}, a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}} containing the polygons between the contour lines.
#' @import raster
#' @import maptools
#' @import rgeos
#' @export
iso_dasymetric <- function(shp, var=NULL, nrow=NA, ncol=NA, N=250000, nlevels=5, style = ifelse(is.null(breaks), "pretty", "fixed"), breaks = NULL, bandwidth=NA, cover=NA, weight=1, ...) {
	bbx <- bb(shp)
	prj <- get_projection(shp)
	asp <- get_asp_ratio(shp)
	
	## determine grid size
	if (inherits(shp, c("SpatialPoints", "SpatialPolygons"))) {
		if (is.na(nrow) || is.na(ncol)) {
			nrow <- round(sqrt(N/asp))
			ncol <- round(N / nrow)
		}
	} else {
		if (inherits(shp, "SpatialGrid")) {
			ncol <- shp@grid@cells.dim[1]
			nrow <- shp@grid@cells.dim[2]
		} else {
			ncol <- shp@ncols	
			nrow <- shp@nrows	
		}
	}
	N <- nrow * ncol

	## process cover
	if (missing(cover)) {
		cover <- as(extent(bbx), "SpatialPolygons")
		cover <- set_projection(cover, current.projection = prj)
	} else if (is.na(cover)) {
		if (inherits(shp, "SpatialPoints")) {
			cover <- as(extent(bbx), "SpatialPolygons")
			cover <- set_projection(cover, current.projection = prj)
		} else if (inherits(shp, "SpatialPolygons")){
			if (missing(var)) var <- names(shp)[1]
			cover <- unionSpatialPolygons(shp, IDs = rep(1, length(shp)))
		} else {
			if (missing(var)) var <- names(shp)[1]
			cover <- raster_cover(shp, var = var, bandwidth = bandwidth, output = "SpatialPolygons")
		}
	} else {
		width <- buffer_width(bbx)
		suppressWarnings(cover <- gBuffer(cover, width = width))
		bbc <- bb(cover)
		bbx[, 1] <- pmin(bbx[, 1], bbc[, 1])
		bbx[, 2] <- pmin(bbx[, 2], bbc[, 2])
	}
	
	## create a smooth raster (using 2D-KDE)
	rlist <- smooth_raster(shp, var=var, nrow=nrow, ncol=ncol, bandwidth = bandwidth, weight=weight, bbx=bbx, output=c("RasterLayer", "list"))
	r <- rlist$RasterLayer
	x <- rlist$list

	lvls <- num2breaks(x$fhat, n=nlevels, style=style, breaks=breaks)$brks
	brks <- fancy_breaks(lvls, intervals=TRUE)
	
	cl <- contourLines(x$x1, x$x2, x$fhat, levels=lvls) 
	if (length(cl) > 10000) stop(paste("Number of iso lines over 10000:", length(cl)))
	cl2 <- ContourLines2SLDF(cl, proj4string = CRS(prj))
	#cl2$levelNR <- as.numeric(as.character(cl2$level))

	# make sure lines are inside poly
	cp <- lines2polygons(ply = cover, lns = cl2, rst = r, lvls=lvls, method="full")

	lns <- SpatialLinesDataFrame(gIntersection(cover, cl2, byid = TRUE), data=cl2@data, match.ID = FALSE)
	
	list(iso=lns, dasy=cp)
}


buffer_width <- function(bbx) {
	sum(bbx[,2] - bbx[,1]) / 1e9
}


lines2polygons <- function(ply, lns, rst=NULL, lvls, method="grid") {
	prj <- get_projection(ply)
	
	# add a little width to lines
	width <- buffer_width(bb(ply))
	suppressWarnings(blpi <- gBuffer(lns, width = width))
	suppressWarnings(ply <- gBuffer(ply, width = width))
	
	# cut the poly with isolines
	dpi <- gDifference(ply, blpi)
	
	if (missing(rst)) {
		dpi
	} else {
		# place each polygon in different SpatialPolygon
		ps <- lapply(dpi@polygons[[1]]@Polygons, function(poly) {
			SpatialPolygons(list(Polygons(list(poly), ID = "1")), proj4string = CRS(prj))	
		})
		
		# find holes
		holes <- sapply(dpi@polygons[[1]]@Polygons, function(poly) poly@hole)
		
		if (all(holes)) stop("All polygons are holes.")
		
		ps_holes <- do.call("sbind", ps[holes])
		ps_solid <- do.call("sbind", ps[!holes])
		
		is_parent <- gContains(ps_solid, ps_holes, byid=TRUE)
		suppressWarnings(areas <- gArea(ps_solid, byid = TRUE))
		parents <- apply(is_parent, MARGIN=1, function(rw) {
			id <- which(rw)
			id[which.min(areas[id])]
		})
		parents <- which(!holes)[parents]
		
		# create poly id (put each polygon in different feature, and append all holes)
		polyid <- cumsum(!holes)
		polyid[holes] <- polyid[parents]
		m <- max(polyid)
		
		dpi2 <- SpatialPolygons(lapply(1:m, function(i) {
			Polygons(dpi@polygons[[1]]@Polygons[which(polyid==i)], ID=i)
		}), proj4string = CRS(prj))

		if (method=="single") {
			pnts <- gPointOnSurface(dpi2, byid = TRUE)
			values <- extract(rst, pnts)
		} else if (method=="grid") {
			values <- sapply(1:m, function(i) {
				p <- dpi2[i,]
				rs <- as(raster(extent(p), nrows=10, ncols=10), "SpatialPoints")
				rs@proj4string <- CRS(prj)
				rs <- gIntersection(rs, p)
				if (is.null(rs)) rs <- gPointOnSurface(p) else {
					rs <- sbind(rs, gPointOnSurface(p))	
				}
				mean(extract(rst, rs))
			})
		} else {
			# method=="full"
			values <- sapply(extract(rst, dpi2), mean, na.rm=TRUE)
		}
		
		
		if (length(lvls)==1) {
			lvls <- c(-Inf, lvls, Inf)
		}

		# just in case...
		values[is.na(values) | is.nan(values)] <- lvls[1]
		
		brks <- fancy_breaks(lvls, intervals=TRUE)
		
		ids <- cut(values, lvls, include.lowest=TRUE, right=FALSE, labels = FALSE)
		
		res <- lapply(1:(length(lvls)-1), function(i) {
			if (any(ids==i)) {
				s <- gUnaryUnion(dpi2[ids==i,])
				SpatialPolygonsDataFrame(s, data.frame(level=factor(brks[i], levels=brks)), match.ID = FALSE)
			} else NULL
		})
		res <- res[!sapply(res, is.null)]
		
		x <- do.call("sbind", res)
	}
}
