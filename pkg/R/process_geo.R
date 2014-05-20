process_geo <- function(x) {
	## get shapes
	shape.id <- which(names(x)=="geo_shape")
	if (!length(shape.id)) stop("Required geo_shape layer missing.")
	shps <- vector("list", length(shape.id))
	for (i in 1:length(shape.id)) {
		j <- shape.id[i]
		shp <- x[[j]]$shp
		data <- shp@data
		if (inherits(shp, "SpatialPolygons")) {
			data$SHAPE_AREAS <- approx_areas(shp, units="prop")
		}
		shps[[i]] <- shp
		x[[j]]$data <- data
	}	
	
	## fill meta info
	meta_layers <- c("geo_theme", "geo_grid")
	gmeta <- x[names(x) %in% meta_layers]
	for (m in meta_layers) {
		ids <- which(names(gmeta)==m)
		if (length(ids)==0) {
			gmeta <- gmeta + do.call(m, args=list())
		} else if (length(ids)>1){
			extraCall <- character(0)
			for (i in 2:length(ids)) {
				gmeta[[ids[1]]][gmeta[[ids[i]]]$call] <- gmeta[[ids[i]]][gmeta[[ids[i]]]$call]
				extraCall <- c(extraCall, gmeta[[ids[i]]]$call)
			}
			gmeta <- gmeta[-(ids[-1])]
			gmeta[[ids[1]]]$call <- c(gmeta[[ids[1]]]$call, extraCall)
		}
	}

	## split x into gmeta and gbody
	x <- x[!(names(x) %in% meta_layers)]
	
	
	n <- length(x)
	
	## split x into clusters
	shape.id <- which(names(x)=="geo_shape")
	if (shape.id[1] != 1) stop("First layers should be a geo_shape layer.")
	y <- rep(0, n); y[shape.id] <- 1
	cluster.id <- cumsum(y)
	
	## unify projections
	shps <- process_projection(shps, x[shape.id])
	gs <- split(x, cluster.id)
	
	nlx <- sapply(gs, length)
	if (any(nlx==1)) warning("Specify at least one layer next to geo_shape")
	
	
	#gs <- lapply(gs, function(gx) if (is.null(gx[["geo_borders"]])) gx + geo_borders() else gx)
	## convert clusters to layers
	gp <- lapply(gs, FUN=process_layers, 
				 free.scales.fill=gmeta$geo_grid$free.scales.fill,
				 free.scales.bubble.size=gmeta$geo_grid$free.scales.bubble.size,
				 free.scales.bubble.col=gmeta$geo_grid$free.scales.bubble.col,
				 free.scales.line.col=gmeta$geo_grid$free.scales.line.col,
				 legend.digits=gmeta$geo_theme$legend.digits,
				 legend.NA.text=gmeta$geo_theme$legend.NA.text)

	## determine maximal number of variables
	nx <- max(sapply(gp, function(x) {
		max(ifelse(is.matrix(x$fill), ncol(x$fill), 1),
			ifelse(is.matrix(x$bubble.size), ncol(x$bubble.size), 1),
			ifelse(is.matrix(x$bubble.col), ncol(x$bubble.col), 1),
			ifelse(is.matrix(x$line.col), ncol(x$line.col), 1),
			length(x$text))
	}))
	names(gp) <- paste0("geoLayer", 1:length(gp))
	
	## get variable names (used for titles)
	varnames <- process_varnames(gp, nx)
	## process grid
	gmeta <- process_meta(gmeta, nx, varnames)
	
	## split into small multiples
	gps <- split_geo(gp, nx)
	gps <- mapply(function(x, i){
		x$geo_theme <- gmeta$geo_theme
		x$geo_theme$title <- x$geo_theme$title[i]
		x$geo_theme$legend.choro.title <- x$geo_theme$legend.choro.title[i]
		x$geo_theme$legend.bubble.size.title <- x$geo_theme$legend.bubble.size.title[i]
		x$geo_theme$legend.bubble.col.title <- x$geo_theme$legend.bubble.col.title[i]
		x
	}, gps, 1:nx, SIMPLIFY=FALSE)
	
	list(gmeta=gmeta, gps=gps, shps=shps, nx=nx)
}
