#' Create a smooth map in various formats: smooth raster, contour lines, and dasymetric polygons.
#' 
#' Create contour lines (isolines) from a shape object. To make the iso lines smooth, a 2D kernal density estimator is applied on the shape object. These lines are used to draw an isopleth. Also, the polygons between the countour lines are returned. They can be used to create a dasymetric map.
#' 
#' For the estimation of the 2D kernal density, code is borrowed from \code{\link[KernSmooth:bkde2D]{bkde2D}}. This implemention is slightly different: \code{\link[KernSmooth:bkde2D]{bkde2D}} takes point coordinates and applies linear binning, whereas in this function, the data is already binned, with values 1 if the values of \code{var} are not missing and 0 if values of \code{var} are missing.
#' 
#' @param shp shape object of class \code{\link[sp:Spatial]{Spatial}} or \code{\link[raster:Raster-class]{Raster}}. Spatial points, polygons, and grids are supported. Spatial lines are not.
#' @param var variable name. Not needed for \code{\link[sp:SpatialPoints]{SpatialPoints}}. If missing, the first variable name is taken.
#' @param nrow number of rows in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param ncol number of rows in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param N preferred number of points in the raster that is used to smooth the shape object. Only applicable if shp is not a \code{\link[sp:SpatialGridDataFrame]{SpatialGrid(DataFrame)}} or \code{\link[raster:Raster-class]{Raster}}
#' @param unit unit specification. Needed when calculating density values. When set to \code{NA}, the densities values are based on the dimensions of the raster (defined by \code{nrow} and \code{ncol}). See also \code{unit.size}.
#' @param unit.size size of the unit in terms of coordinate units. The coordinate system of many projections is approximately in meters while thematic maps typically range many kilometers, so by default \code{unit="km"} and \code{unit.size=1000} (meaning 1 kilometer equals 1000 coordinate units).
#' @param smooth.raster logical that determines whether 2D kernel density smoothing is applied to the raster shape object. Not applicable when \code{shp} is a \code{\link[sp:SpatialPoints]{SpatialPoints}} object.
#' @param nlevels preferred number of levels
#' @param style method to cut the color scale: e.g. "fixed", "equal", "pretty", "quantile", or "kmeans". See the details in \code{\link[classInt:classIntervals]{classIntervals}}.
#' @param breaks in case \code{style=="fixed"}, breaks should be specified
#' @param bandwidth single numeric value or vector of two numeric values that specifiy the bandwidth of the kernal density estimator. By default, it is 1/50th of the shortest side in units (specified with \code{unit.size}).
#' @param cover.type character value that specifies the type of raster cover, in other words, how the boundaries are specified. Options: \code{"original"} uses the same boundaries as \code{shp} (default for polygons), \code{"smooth"} calculates a smooth boundary based on the 2D kernal density (determined by \code{\link{smooth_raster_cover}}), \code{"rect"} uses the bounding box of \code{shp} as boundaries (default for spatial points and grids).
#' @param cover \code{\link[sp:SpatialPolygons]{SpatialPolygons}} shape that determines the covered area in which the contour lines are placed. If specified, \code{cover.type} is ignored.
#' @param cover.threshold numeric value between 0 and 1 that determines which part of the estimated 2D kernal density is returned as cover. Only applicable when \code{cover.type="smooth"}.
#' @param weight single number that specifies the weight of a single point. Only applicable if \code{shp} is a \code{\link[sp:SpatialPoints]{SpatialPoints}} object.
#' @param extracting.method Method of how coordinates are extracted from the dasymetric polygons. Options are: \code{"full"} (default), \code{"grid"}, and \code{"single"}. See details. For the slowest method \code{"full"}, \code{\link[raster:extract]{extract}} is used. For \code{"grid"}, points on a grid layout are selected that intersect with the polygon. For \code{"simple"}, a simple point is generated with \code{\link[rgeos:gPointOnSurface]{gPointOnSurface}}.
#' @param buffer.width Buffer width of the iso lines to cut dasymetric polygons. Should be small enough to let the polygons touch each other without space in between. However, too low values may cause geometric errors.
#' @param to.Raster should the "raster" output (see \code{output}) be a \code{\link[raster:Raster-class]{RasterLayer}}? By default, it is returned as a \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}}
#' @return List with the following items:
#' \describe{
#' \item{\code{"raster"}}{A smooth raster, which is either a \code{\link[sp:SpatialGridDataFrame]{SpatialGridDataFrame}} or a \code{\link[raster:Raster-class]{RasterLayer}} (see \code{to.Raster})}
#' \item{\code{"iso"}}{Contour lines, which is a \code{\link[sp:SpatialLinesDataFrame]{SpatialLinesDataFrame}}}
#' \item{\code{"dasy"}}{Dasymetric polygons, which is a \code{\link[sp:SpatialPolygonsDataFrame]{SpatialPolygonsDataFrame}}}
#' \item{\code{"bbox"}}{Bounding box of the used raster}
#' \item{\code{"ncol"}}{Number of rows in the raster}
#' \item{\code{"nrow"}}{Number of columns in the raster}
#' }
#' @importFrom raster raster extent couldBeLonLat extract extend rasterToContour
#' @importMethodsFrom raster as.vector
#' @importFrom rgeos gConvexHull gUnaryUnion gPointOnSurface gContains gIsValid gIntersection gArea gBuffer gDifference
#' @importFrom KernSmooth bkde2D
#' @importFrom grDevices contourLines
#' @example ../examples/smooth_map.R
#' @export
smooth_map <- function(shp, var=NULL, nrow=NA, ncol=NA, N=250000, unit="km", unit.size=1000, smooth.raster=TRUE, nlevels=5, style = ifelse(is.null(breaks), "pretty", "fixed"), breaks = NULL, bandwidth=NA, cover.type=NA, cover=NULL, cover.threshold=.6, weight=1, extracting.method="full", buffer.width=NA, to.Raster=FALSE) {
	bbx <- bb(shp)
	prj <- get_projection(shp)
#	asp <- get_asp_ratio(shp)

	pb <- txtProgressBar(style=3)
	
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

	if (!is_projected(shp) || is.na(unit)) {
		warning("shp is not projected; therefore density values cannot be calculated", call. = FALSE)
		cell.area <- 1
	} else {
		cell.width <- (bbx[1,2] - bbx[1,1]) / (unit.size * ncol)
		cell.height <- (bbx[2,2] - bbx[2,1]) / (unit.size * nrow)
		cell.area <- cell.width * cell.height
	}
	
	
	
	# edit bandwidth
	if (is.na(bandwidth[1])) {
		#bandwidth <- 3 * (bbx[,2] - bbx[,1]) / c(ncol, nrow)
		short_side <- min((bbx[,2] - bbx[,1]) / unit.size)
		bandwidth <- rep(short_side/100, 2)
	} else {
		# make sure bandwith is a vector of 2
		bandwidth <- rep(bandwidth, length.out=2)
	}

	cover_r <- raster(extent(bbx), nrows=nrow, ncols=ncol, crs=prj)
	
	setTxtProgressBar(pb, .1)
		
	## process cover
	if (is.na(cover.type)) cover.type <- ifelse(inherits(shp, "SpatialPolygons"), "original", "rect")
	if (missing(cover)) {
			
		if (cover.type=="rect") {
			cover <- as(extent(bbx), "SpatialPolygons")
			if (!is.na(prj)) cover <- set_projection(cover, current.projection = prj)
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
				if (!gIsValid(cover)) cover <- gBuffer(cover, width=0)
				cover@bbox <- bbx
				cover_r <- poly_to_raster(cover, nrow = nrow, ncol = ncol, to.Raster = TRUE)
			}
		}  else if (cover.type=="smooth") {
			cover_list <- smooth_raster_cover(shp, var=var, bandwidth = bandwidth*unit.size, threshold = cover.threshold, output=c("RasterLayer", "SpatialPolygons"))	
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
	setTxtProgressBar(pb, .3)
	

	if (inherits(shp, "SpatialPoints")) {
		co <- coordinates(shp)
		x <- bkde2D(co, bandwidth=bandwidth*unit.size, gridsize=c(ncol, nrow), range.x=list(bbx[1,], bbx[2,]))
		
		# normalize
		#x$fhat <- x$fhat * (length(shp) * weight / sum(x$fhat, na.rm=TRUE))
		var <- "count"
	} else {
		if (missing(var)) var <- names(shp)[1]
		if (inherits(shp, "SpatialPolygons")){
			shp@data <- shp@data[,var, drop=FALSE]
			shp <- poly_to_raster(shp, nrow = nrow, ncol=ncol, copy.data = TRUE)
		}
		shpr <- raster(shp, layer=var) #if (nlayers(shp)>1) raster(shp, layer=var) else shp
		if (smooth.raster) {
			m <- as.matrix(shpr)
			x <- kde2D(m, bandwidth = bandwidth*unit.size, gridsize=c(ncol, nrow), range.x=list(bbx[1,], bbx[2,]))
			
			# normalize
			#x$fhat <- x$fhat * (sum(shp[[var]], na.rm=TRUE) / sum(x$fhat, na.rm=TRUE))
		} else {
			r <- shpr
			lvls <- num2breaks(r[], n=nlevels, style=style, breaks=breaks)$brks
		}
	}
	setTxtProgressBar(pb, .5)
	
	if (inherits(shp, "SpatialPoints") || smooth.raster) {
		# fill raster values
		r <- raster(extent(bbx), nrows=nrow, ncols=ncol, crs=prj)
		r[] <- as.vector(x$fhat[, ncol(x$fhat):1])
		names(r) <- var
		
		# apply cover
		r[is.na(cover_r[])] <- NA
		
		# normalize r and x$fhat
		if (inherits(shp, "SpatialPoints")) {
			norm_weight <- length(shp) * weight / sum(r[], na.rm=TRUE)
		} else {
			norm_weight <- sum(shp[[var]], na.rm=TRUE) / sum(r[], na.rm=TRUE)
		}
		r[] <- r[] * norm_weight / cell.area
		x$fhat <- x$fhat * norm_weight / cell.area

		lvls <- num2breaks(x$fhat, n=nlevels, style=style, breaks=breaks)$brks
		#brks <- fancy_breaks(lvls, intervals=TRUE)
		
		cl <- contourLines(x$x1, x$x2, x$fhat, levels=lvls) 
		if (length(cl) < 1L) stop("No iso lines found")
		if (length(cl) > 10000) stop(paste("Number of iso lines over 10000:", length(cl)))
		cl2 <- contour_lines_to_SLDF(cl, proj4string = CRS(prj))
		#cl2$levelNR <- as.numeric(as.character(cl2$level))
	} else {
		bbr <- bb(r)
		
		# extend raster to prevent bleeding (due to isolines that do not reach the boundary)
		rxtra <- (floor(nrow(r) / 10) + 1) * 2
		cxtra <- (floor(ncol(r) / 10) + 1) * 2
		bbr2 <- bb(bbr, width=(ncol(r)+cxtra)/ncol(r),
				   height=(nrow(r)+rxtra)/nrow(r),
				   relative=TRUE)
		
		r2 <- extend(r, extent(bbr2))
		r2[1:(rxtra/2),(cxtra/2+1):(ncol(r2)-cxtra/2)] <- r[1,]
		r2[(nrow(r2)-(rxtra/2)+1):nrow(r2),(cxtra/2+1):(ncol(r2)-cxtra/2)] <- r[nrow(r),]
		
		r2[,1:(cxtra/2)] <- r2[,cxtra/2+1]
		r2[,(ncol(r2)-cxtra/2+1):ncol(r2)] <- r2[,ncol(r2)-cxtra/2]
		
		cl2 <- rasterToContour(r2, maxpixels = length(r2), levels=lvls)
	}
	setTxtProgressBar(pb, .7)
	
	# make sure lines are inside poly
	cp <- lines2polygons(ply = cover, lns = cl2, rst = r, lvls=lvls, extracting.method="full", buffer.width = buffer.width)
	attr(cp, "dasymetric") <- TRUE
	
	setTxtProgressBar(pb, .9)
	
	lns <- SpatialLinesDataFrame(gIntersection(cover, cl2, byid = TRUE), data=cl2@data, match.ID = FALSE)
	attr(lns, "isolines") <- TRUE
	setTxtProgressBar(pb, 1)
	
	list(raster = if(to.Raster) r else as(r, "SpatialGridDataFrame"),
		 iso = lns,
		 dasy = cp,
		 bbox = bbx,
		 nrow = nrow,
		 ncol = ncol,
		 cell.area=cell.area,
		 bandwidth = bandwidth)
}

contour_lines_to_SLDF <- function (cL, proj4string = CRS(as.character(NA))) 
{
	.contourLines2LineList <- function (cL) 
	{
		n <- length(cL)
		res <- vector(mode = "list", length = n)
		for (i in 1:n) {
			crds <- cbind(cL[[i]][[2]], cL[[i]][[3]])
			res[[i]] <- Line(coords = crds)
		}
		res
	}
	cLstack <- tapply(1:length(cL), sapply(cL, function(x) x[[1]]), 
					  function(x) x, simplify = FALSE)
	df <- data.frame(level = factor(names(cLstack), ordered=TRUE))
	m <- length(cLstack)
	res <- vector(mode = "list", length = m)
	IDs <- paste("C", 1:m, sep = "_")
	row.names(df) <- IDs
	for (i in 1:m) {
		res[[i]] <- Lines(.contourLines2LineList(cL[cLstack[[i]]]), 
						  ID = IDs[i])
	}
	SL <- SpatialLines(res, proj4string = proj4string)
	SpatialLinesDataFrame(SL, data = df)
}


buffer_width <- function(bbx) {
	prod(bbx[,2] - bbx[,1]) / 1e12
}


lines2polygons <- function(ply, lns, rst=NULL, lvls, extracting.method="full", buffer.width=NA) {
	prj <- get_projection(ply)
	
	# add a little width to lines
	if (is.na(buffer.width)) buffer.width <- buffer_width(bb(ply))
	suppressWarnings(blpi <- gBuffer(lns, width = buffer.width))
	suppressWarnings(ply <- gBuffer(ply, width = 0))
	
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

		# create poly id (put each polygon in different feature, and append all holes)
		polyid <- cumsum(!holes)
		
		if (any(holes)) {
			ps_holes <- do.call("sbind", ps[holes])
			ps_solid <- do.call("sbind", ps[!holes])
			
			is_parent <- gContains(ps_solid, ps_holes, byid=TRUE)
			suppressWarnings(areas <- gArea(ps_solid, byid = TRUE))
			parents <- apply(is_parent, MARGIN=1, function(rw) {
				id <- which(rw)
				id[which.min(areas[id])]
			})
			parents <- which(!holes)[parents]
			
			polyid[holes] <- polyid[parents]
		}
		
		m <- max(polyid)
		
		dpi2 <- SpatialPolygons(lapply(1:m, function(i) {
			Polygons(dpi@polygons[[1]]@Polygons[which(polyid==i)], ID=i)
		}), proj4string = CRS(prj))
		
		if (extracting.method=="single") {
			pnts <- gPointOnSurface(dpi2, byid = TRUE)
			values <- extract(rst, pnts)
		} else if (extracting.method=="grid") {
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
			# extracting.method=="full"
			values <- sapply(extract(rst, dpi2), function(x)if (is.null(x)) NA else mean(x, na.rm=TRUE))
		}
		
		
		if (length(lvls)==1) {
			lvls <- c(-Inf, lvls, Inf)
		}
		
		# just in case...
		values[is.na(values) | is.nan(values)] <- lvls[1]
		
		brks <- fancy_breaks(lvls, intervals=TRUE)
		
		ids <- cut(values, lvls, include.lowest=TRUE, right=FALSE, labels = FALSE)
		
		if (any(is.na(ids))) stop("raster values not in range")
		
		
		res <- lapply(1:(length(lvls)-1), function(i) {
			if (any(ids==i)) {
				s <- gUnaryUnion(dpi2[ids==i,])
				SpatialPolygonsDataFrame(s, data.frame(level=factor(brks[i], levels=brks, ordered = TRUE)), match.ID = FALSE)
			} else NULL
		})
		res <- res[!sapply(res, is.null)]
		
		x <- do.call("sbind", res)
	}
}
