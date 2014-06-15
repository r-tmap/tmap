process_geo <- function(x) {
	fill <- NULL; xfill <- NULL
	## fill meta info
	
	## get geo_theme elements
	if (!("geo_theme" %in% names(x))) {
		gt <- geo_theme()$geo_theme
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
	
	## get grid element
	gridid <- which(names(x)=="geo_grid")[1]
	gg <- x[[gridid]]
	
		
	## split x into gmeta and gbody
	x <- x[!(names(x) %in% c("geo_theme", "geo_grid"))]

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
	gf <- if (is.na(facetID[1])) geo_facets()$geo_facets else gs[[facetID]]$geo_facets
	gmeta <- process_meta(gt, gf, gg, nx, varnames)
	## split into small multiples
	gps <- split_geo(gp, nx)
	scale <- gmeta$scale
	gps <- mapply(function(x, i){
		x <- lapply(x, function(xx) {
			within(xx, {
				lwd <- lwd * scale
				
				if (!is.null(fill)) {
					if (!is.na(xfill[1])) fill.legend.misc$lwd <- fill.legend.misc$lwd * scale
				}

				if (!is.null(bubble.size)) {
					bubble.size <- bubble.size * scale
					bubble.border.lwd <- bubble.border.lwd * scale
					bubble.col.legend.misc$bubble.max.size <- bubble.col.legend.misc$bubble.max.size * scale
					bubble.col.legend.misc$bubble.border.lwd <- bubble.col.legend.misc$bubble.border.lwd * scale
					
					bubble.size.legend.misc$legend.sizes <- bubble.size.legend.misc$legend.sizes * scale
					bubble.size.legend.misc$bubble.border.lwd <- bubble.size.legend.misc$bubble.border.lwd * scale
				}
				
				if (!is.null(line.lwd)) {
					line.lwd <- line.lwd * scale
					line.col.legend.misc$line.legend.lwd <- line.col.legend.misc$line.legend.lwd * scale
					line.lwd.legend.misc$legend.lwds <- line.lwd.legend.misc$legend.lwds * scale
				}
				
				if (!is.null(text)) {
					text.cex <- text.cex * scale
				}
			})
		})
		
		x$geo_theme <- gmeta
		x$geo_theme$title <- x$geo_theme$title[i]
		x$geo_theme$legend.titles <- sapply(x$geo_theme$legend.titles, function(x)x[i])
		x
	}, gps, 1:nx, SIMPLIFY=FALSE)
	
	list(gmeta=gmeta, gps=gps, nx=nx)
}
