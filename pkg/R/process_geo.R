process_geo <- function(x) {
	## fill meta info
	
	if (!("geo_theme" %in% names(x))) {
		gt <- geo_theme()
	} else {
		gts <- x[names(x)=="geo_theme"]
		gtsn <- length(gts)
		gt <- gts[[1]]
		if (gtsn>1) {
			extraCall <- character(0)
			for (i in 2:gtsn) {
				gt[gts[[i]]$call] <- gts[[i]][gts[[i]]$call]
				extraCall <- c(extraCall, gts[[i]]$call)
			}
			gt$call <- c(gt$call, extraCall)
		}
	}
	
	## split x into gmeta and gbody
	x <- x[names(x)!="geo_theme"]

	n <- length(x)
	
	## split x into clusters
	shape.id <- which(names(x)=="geo_shape")
	if (shape.id[1] != 1) stop("First layers should be a geo_shape layer.")
	y <- rep(0, n); y[shape.id] <- 1
	cluster.id <- cumsum(y)
	gs <- split(x, cluster.id)
	
	nlx <- sapply(gs, length)
	if (any(nlx==1)) warning("Specify at least one layer next to geo_shape")
	
	
	#gs <- lapply(gs, function(gx) if (is.null(gx[["geo_borders"]])) gx + geo_borders() else gx)
	## convert clusters to layers
	gp <- lapply(gs, FUN=process_layers, gt)

	## determine maximal number of variables
	nx <- max(sapply(gp, function(x) {
		max(ifelse(is.matrix(x$fill), ncol(x$fill), 1),
			ifelse(is.matrix(x$bubble.size), ncol(x$bubble.size), 1),
			ifelse(is.matrix(x$bubble.col), ncol(x$bubble.col), 1),
			ifelse(is.matrix(x$line.col), ncol(x$line.col), 1),
			ifelse(is.matrix(x$line.lwd), ncol(x$line.lwd), 1),
			ifelse(is.matrix(x$text), ncol(x$text), 1))
	}))
	names(gp) <- paste0("geoLayer", 1:length(gp))
	
	## get variable names (used for titles)
	varnames <- process_varnames(gp, nx)
	## process grid
	facetID <- which(sapply(gp, function(gpl) gpl$facets_defined))[1]
	if (!length(facetID)) facetID <- 1
	gf <- gs[[facetID]]$geo_facets
	gmeta <- process_meta(gt, gf, nx, varnames)
	browser()
	## split into small multiples
	gps <- split_geo(gp, nx)
	gps <- mapply(function(x, i){
		x$geo_theme <- gmeta
		x$geo_theme$title <- x$geo_theme$title[i]
		x$geo_theme$legend.titles <- sapply(x$geo_theme$legend.titles, function(x)x[i])
		x
	}, gps, 1:nx, SIMPLIFY=FALSE)
	
	list(gmeta=gmeta, gps=gps, nx=nx)
}
