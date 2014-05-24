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

	
	# backup par settings
	grid.newpage()	
	dw <- convertWidth(unit(1,"npc"), "inch", valueOnly=TRUE)
	dh <- convertHeight(unit(1,"npc"), "inch", valueOnly=TRUE)
	
	shps_lengths <- sapply(shps, length)
	shps <- process_shapes(shps, x[shape.id], gmeta, dw, dh)
	
	## unify projections and set bounding box
	matchIDs <- lapply(shps, function(s)s@matchID)

	gps <- lapply(gps, function(gp) {
		gp[1:nshps] <- mapply(function(gpl, indices, l) {
			if (length(gpl$fill)==l) gpl$fill <- gpl$fill[indices]
			if (length(gpl$bubble.size)==l) gpl$bubble.size <- gpl$bubble.size[indices]
			if (length(gpl$bubble.col)==l) gpl$bubble.col <- gpl$bubble.col[indices]
			if (length(gpl$bubble.xmod)==l) gpl$bubble.xmod <- gpl$bubble.xmod[indices]
			if (length(gpl$bubble.ymod)==l) gpl$bubble.ymod <- gpl$bubble.ymod[indices]
			if (length(gpl$text.xmod)==l) gpl$text.xmod <- gpl$text.xmod[indices]
			if (length(gpl$text.ymod)==l) gpl$text.ymod <- gpl$text.ymod[indices]
			gpl
		},  gp[1:nshps], matchIDs, shps_lengths, SIMPLIFY=FALSE)
		gp
	})
	
	#grid.newpage()
	shps.env <- new.env()
	assign("shps", shps, envir=shps.env)
	gridplot(gmeta$geo_grid$nrow, gmeta$geo_grid$ncol, "plot_all", nx, gps, shps.env)
}


