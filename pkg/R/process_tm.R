process_tm <- function(x) {
	fill <- NULL; xfill <- NULL
	## fill meta info
	
	## get tm_layout elements
	if (!("tm_layout" %in% names(x))) {
		gt <- tm_layout()$tm_layout
	} else {
		gts <- x[names(x)=="tm_layout"]
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
	gridid <- which(names(x)=="tm_grid")[1]
	gg <- x[[gridid]]
	
	## get facets element
	facetid <- which(names(x)=="tm_facets")[1]
	if (is.na(facetid)) {
		gf <- tm_facets()$tm_facets 
		gf$shp_nr <- 0
		gf$shp_name <- ""
	} else {
		shape.id.orig <- which(names(x)[1:facetid]=="tm_shape")
		gf.shp.id <- tail(shape.id.orig, 1)
		gf <- x[[facetid]]
		gf$shp_nr <- length(shape.id.orig)
		gf$shp_name <- x[[gf.shp.id]]$shp_name
	}
	
	## split x into gmeta and gbody
	x <- x[!(names(x) %in% c("tm_layout", "tm_grid", "tm_facets"))]

	n <- length(x)
	
	## split x into clusters
	shape.id <- which(names(x)=="tm_shape")
	if (shape.id[1] != 1) stop("First layers should be a tm_shape layer.")
	y <- rep(0, n); y[shape.id] <- 1
	cluster.id <- cumsum(y)
	gs <- split(x, cluster.id)
	
	nlx <- sapply(gs, length)
	if (any(nlx==1)) warning("Specify at least one layer next to tm_shape")
	
	
	#gs <- lapply(gs, function(gx) if (is.null(gx[["tm_borders"]])) gx + tm_borders() else gx)
	## convert clusters to layers
	gp <- lapply(gs, FUN=process_layers, gt, gf)
	
	
	## get by vector
	if (gf$shp_nr == 0) {
		data_by <- NULL
	} else {
		data_by <- gp[[gf$shp_nr]]$data_by
	}

	## determine maximal number of variables
	nx <- max(sapply(gp, function(x) {
		max(ifelse(is.matrix(x$fill), ncol(x$fill), 1),
			ifelse(is.matrix(x$bubble.size), ncol(x$bubble.size), 1),
			ifelse(is.matrix(x$bubble.col), ncol(x$bubble.col), 1),
			ifelse(is.matrix(x$line.col), ncol(x$line.col), 1),
			ifelse(is.matrix(x$line.lwd), ncol(x$line.lwd), 1),
			ifelse(is.matrix(x$text), ncol(x$text), 1))
	}))
	names(gp) <- paste0("tmLayer", 1:length(gp))
	
	## get variable names (used for titles)
	varnames <- process_varnames(gp, nx)

	## process grid
	gmeta <- process_meta(gt, gf, gg, nx, varnames)
	## split into small multiples
	gps <- split_tm(gp, nx)
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
		
		x$tm_layout <- gmeta
		x$tm_layout$title <- x$tm_layout$title[i]
		x$tm_layout$legend.titles <- sapply(x$tm_layout$legend.titles, function(x)x[i])
		x
	}, gps, 1:nx, SIMPLIFY=FALSE)
	
	list(gmeta=gmeta, gps=gps, nx=nx, data_by=data_by)
}
