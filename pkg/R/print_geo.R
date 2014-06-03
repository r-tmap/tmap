#' Print geo object
#' 
#' Print geo object
#' 
#' @param x geo object
#' @param ... not used
#' @import sp
#' @import RColorBrewer
#' @import grid
#' @import gridBase
#' @import classInt
#' @export
#' @method print geo
print.geo <- function(x, ...) {
	
	## get shapes
	shape.id <- which(names(x)=="geo_shape")
	
	nshps <- length(shape.id)
	if (!nshps) stop("Required geo_shape layer missing.")
	
	shps <- lapply(x[shape.id], function(y)y$shp)

	
	x[shape.id] <- lapply(x[shape.id], function(y){
		data <- y$shp@data
		if (inherits(y$shp, "SpatialPolygons")) data$SHAPE_AREAS <- approx_areas(y$shp, units="prop")
		y$data <- data
		y$shp <- NULL
		y
	})
	
	result <- process_geo(x)
	
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


