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
print.geo <- function(x, ...) {
	## fill meta info
	meta_layers <- c("geo_theme", "geo_grid")
	for (m in meta_layers) {
		ids <- which(names(x)==m)
		if (length(ids)==0) {
			x <- x + do.call(m, args=list())
		} else if (length(ids)>1){
			for (i in 2:length(ids)) {
				x[[ids[1]]][x[[ids[i]]]$call] <- x[[ids[i]]][x[[ids[i]]]$call]
			}
			x <- x[-(ids[-1])]
		}
	}
	
	## split x into gmeta and gbody
	gmeta <- x[meta_layers]
	gbody <- x[!(names(x) %in% meta_layers)]
	
	n <- length(gbody)
	
	## split x into clusters
	shape.id <- which(names(gbody)=="geo_shape")
	if (!length(shape.id)) stop("Required geo_shape layer missing.")
	if (shape.id[1] != 1) stop("First layers should be a geo_shape layer.")
	x <- rep(0, n); x[shape.id] <- 1
	cluster.id <- cumsum(x)
	
	## unify projections
	gbody[shape.id] <- process_projection(gbody[shape.id])
	
	gs <- split(gbody, cluster.id)
	
	nlx <- sapply(gs, length)
	if (any(nlx==1)) warning("Specify at least one layer next to geo_shape")
	
	
	
	#gs <- lapply(gs, function(gx) if (is.null(gx[["geo_borders"]])) gx + geo_borders() else gx)
	
	## convert clusters to layers
	gp <- lapply(gs, FUN=process_layers, 
				 free.scales.choro=gmeta$geo_grid$free.scales.choro,
				 free.scales.bubble.size=gmeta$geo_grid$free.scales.bubble.size,
				 free.scales.bubble.col=gmeta$geo_grid$free.scales.bubble.col,
				 legend.digits=gmeta$geo_theme$legend.digits)
	
	## determine maximal number of variables
	nx <- max(sapply(gp, function(x) {
		max(ifelse(is.matrix(x$fill), ncol(x$fill), 1),
			ifelse(is.matrix(x$bubble.size), ncol(x$bubble.size), 1),
			ifelse(is.matrix(x$bubble.col), ncol(x$bubble.col), 1),
			length(x$text))
	}))
	names(gp) <- paste0("geoLayer", 1:length(gp))
	
	## get variable names (used for titles)
	varnames <- process_varnames(gp, nx)
	
	## process grid
	gmeta <- process_meta(gmeta, nx, varnames)
	
	## split into small multiples
	gps <- split_geo(gp, nx)
	
	gps$multiples <- mapply(function(x, i){
		x$geo_theme <- gmeta$geo_theme
		x$geo_theme$title <- x$geo_theme$title[i]
		x$geo_theme$legend.choro.title <- x$geo_theme$legend.choro.title[i]
		x$geo_theme$legend.bubble.size.title <- x$geo_theme$legend.bubble.size.title[i]
		x$geo_theme$legend.bubble.col.title <- x$geo_theme$legend.bubble.col.title[i]
		x
	}, gps$multiples, 1:nx, SIMPLIFY=FALSE)
	
	# backup par settings
	#opar <- par("mai", "xaxs", "yaxs")
	opar <- par(no.readonly=TRUE)
	
	plot.new()
	#grid.newpage()
	gridplot(gmeta$geo_grid$nrow, gmeta$geo_grid$ncol, "plot_all", nx, gps$shps, gps$multiples)
	do.call("par", opar)
	
}