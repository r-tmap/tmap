#' Print tm object
#' 
#' Print tm object
#' 
#' @param x tm object
#' @param ... not used
#' @import sp
#' @import RColorBrewer
#' @import grid
#' @import gridBase
#' @import classInt
#' @export
#' @method print tmap
print.tmap <- function(x, ...) {
	
	## get shapes
	shape.id <- which(names(x)=="tm_shape")
	
	nshps <- length(shape.id)
	if (!nshps) stop("Required tm_shape layer missing.")
	
	shps <- lapply(x[shape.id], function(y) {
		shp <- y$shp
		data <- data.frame(ID=get_IDs(shp))
		if (class(shp)=="SpatialPolygons") shp <- SpatialPolygonsDataFrame(shp, data=data, match.ID=FALSE)
		if (class(shp)=="SpatialPoints") shp <- SpatialPointsDataFrame(shp, data=data, match.ID=FALSE)
		if (class(shp)=="SpatialLines") shp <- SpatialLinesDataFrame(shp, data=data, match.ID=FALSE)
		if (class(shp)=="SpatialPolygonsDataFrame") shp$SHAPE_AREAS <- approx_areas(shp, units="abs") / 1e6
		shp
	})

	## to do: update code since shps are spatialXdataframes
	datasets <- lapply(shps, function(x)x@data)
	
	x[shape.id] <- mapply(function(y, dataset){
		y$data <- dataset
		y$shp <- NULL
		y
	}, x[shape.id], datasets, SIMPLIFY=FALSE)
	
	result <- process_tm(x)
	
	gmeta <- result$gmeta
	gps <- result$gps
	nx <- result$nx

	
	grid.newpage()	
	
	margins <- gmeta$outer.margins
	dw <- convertWidth(unit(1-sum(margins[c(2,4)]),"npc"), "inch", valueOnly=TRUE)
	dh <- convertHeight(unit(1-sum(margins[c(1,3)]),"npc"), "inch", valueOnly=TRUE)
	
	shps_lengths <- sapply(shps, length)
	shps <- process_shapes(shps, x[shape.id], gmeta, dw, dh)

	
	## unify projections and set bounding box
	matchIDs <- lapply(shps, function(s)s@matchID)
	gps <- lapply(gps, function(gp) {
		gp[1:nshps] <- mapply(function(gpl, indices, l) {
			gpl$npol <- length(indices)
			lapply(gpl, function(gplx) {
				if ((is.vector(gplx) || is.factor(gplx)) && length(gplx)==l) {
					gplx <- gplx[indices]	
				} else {
					gplx
				}
			})
		},  gp[1:nshps], matchIDs, shps_lengths, SIMPLIFY=FALSE)
		gp
	})
	
	#grid.newpage()
	dasp <- attr(shps, "dasp")
	sasp <- attr(shps, "sasp")

	shps.env <- environment()#new.env()
	#assign("shps", shps, envir=shps.env)
	gridplot(gmeta$nrow, gmeta$ncol, "plot_all", nx, gps, shps.env, dasp, sasp)
	
	invisible()
}


