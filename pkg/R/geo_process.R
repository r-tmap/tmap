print.geo <- function(g) {
	
	## fill meta info
	if (is.null(g$geo_theme)) g <- g + geo.theme()
	if (is.null(g$geo_grid)) g <- g + geo.grid()
	if (is.null(g$geo_zoom)) g <- g + geo.zoom()
	
	## split g into gmeta and gbody
	gmeta <- g[c("geo_theme", "geo_grid", "geo_zoom")]
	gbody <- g[!(names(g) %in% c("geo_theme", "geo_grid", "geo_zoom"))]
	
	shp.names <- sapply(gbody, function(l)ifelse(is.null(l$shp), l$coor, l$shp))
	shp.names.unique <- unique(shp.names)
# 	shps <- lapply(shp.names.unique, get)
# 	names(shps) <- shp.names.unique
	

	
	## split g into layers, and process them
	cluster.id <- c(1, cumsum(shp.names[-1] != shp.names[-(length(shp.names))]) + 1)
	gs <- split(gbody, cluster.id)
	gp <- lapply(gs, FUN=process.layers, free.scales=gmeta$geo_grid$free.scales)
	nx <- max(sapply(gp, function(x) {
		max(ifelse(is.matrix(x$fill), ncol(x$fill), 1),
			ifelse(is.matrix(x$bubble.size), ncol(x$bubble.size), 1),
			ifelse(is.matrix(x$bubble.col), ncol(x$bubble.col), 1),
			length(x$text))
	}))
	names(gp) <- paste0("geoLayer", 1:length(gp))

	
	varnames <- process.varnames(gp, nx)
	
	## process grid
	gmeta <- process.meta(gmeta, nx, varnames)
	
	
	
	## split into small multiples
	gps <- split_geo(gp, nx)
	
	names(gps) <- paste0("plot", 1:nx)
	gps <- mapply(function(x, i){
		x$geo_theme <- gmeta$geo_theme
		x$geo_theme$title <- x$geo_theme$title[i]
		x$geo_zoom <- gmeta$geo_zoom
		x
	}, gps, 1:nx, SIMPLIFY=FALSE)
	
	plot.new()
	gridplot2(gmeta$geo_grid$nrow, gmeta$geo_grid$ncol, "plotAll", nx, gps)
}

process.varnames <- function(gp, nx) {
	varnamesList <- lapply(gp, function(x) x$varnames)
	fillVar <- lapply(varnamesList, function(x) x$choro.fill)
	sizeVar <- lapply(varnamesList, function(x) x$bubble.size)
	colVar <- lapply(varnamesList, function(x) x$bubble.col)
	
	fillId <- which(sapply(fillVar, function(x)!is.na(x[1])))
	sizeId <- which(sapply(sizeVar, function(x)!is.na(x[1])))
	colId <- which(sapply(colVar, function(x)!is.na(x[1])))
	
	list(choro.fill={if (length(fillId)) rep(fillVar[[fillId[1]]], 
											   length.out=nx) else NA},
		 bubble.size={if (length(sizeId)) rep(sizeVar[[sizeId[1]]], 
		 									   length.out=nx) else NA},
		 bubble.col={if (length(colId)) rep(colVar[[colId[1]]], 
		 									 length.out=nx) else NA})
}



process.meta <- function(g, nx, varnames) {
	ggrid <- g$geo_grid
	if (is.null(ggrid$ncol) && is.null(ggrid$nrow)) {
		## default setting: place next to each other, or in grid
		if (nx <= 3) {
			ggrid$ncol <- nx
			ggrid$nrow <- 1
		} else {
			ggrid$ncol <- ceiling(sqrt(nx))
			ggrid$nrow <- ceiling(nx / ggrid$ncol)
		}
	} else {
		if (is.null(ggrid$ncol)) ggrid$ncol <- 1
		if (is.null(ggrid$nrow)) ggrid$nrow <- 1
	}
	g$geo_grid <- ggrid

	gzoom <- g$geo_zoom
	gzoom$units <- ifelse(gzoom$units %in% c("r", "rel", "relative"), "rel", "abs")
	g$geo_zoom <- gzoom
	
	gtheme <- g$geo_theme
	if (is.null(gtheme$title)) {
		id <- which(as.logical(sapply(varnames, function(x)sum(!is.na(x[1])))))[1]
		gtheme$title <- if (!is.na(id)) varnames[[id]] else rep("", nx)
	} else {
		gtheme$title <- if (gtheme$title[1]=="choro.fill") {
			varnames[[1]]	
		} else if (gtheme$title[1]=="bubble.size") {
			varnames[[2]]	
		} else if (gtheme$title[1]=="bubble.col") {
			varnames[[3]]	
		} else gtheme$title
	}
	if (length(gtheme$title) < nx) gtheme$title <- rep(gtheme$title, length.out=nx)
	if (gzoom$xlim[1] > 0 || gzoom$xlim[2] < 1 || gzoom$ylim[1] > 0 || gzoom$ylim[2] < 1) {
		if (is.na(gtheme$margins[1])) gtheme$margins <- c(0.05, 0.05, 0.2, 0.05)
		if (is.na(gtheme$draw.frame)) gtheme$draw.frame <- TRUE
	} else {
		if (is.na(gtheme$margins[1])) gtheme$margins <- rep(0, 4)
		if (is.na(gtheme$draw.frame)) gtheme$draw.frame <- FALSE
	}
	if (is.na(gtheme$legend.plot.size[1])) {gtheme$legend.plot.size <- if (gtheme$legend.only) c(0.4, 0.9) else c(0.2,0.35)}
		g$geo_theme <- gtheme
	
	
	g
}

split_geo <- function(gp, nx) {
	gps <- lapply(1:nx, function(i){
		g <- gp
		g <- lapply(g, function(x) {
			x$fill <- get_ids(x$fill, i)
			x$choro.values <- get_ids(x$choro.values, i)
			x$choro.legend.labels <- get_ids(x$choro.legend.labels, i)
			x$choro.legend.palette <- get_ids(x$choro.legend.palette, i)
			x$choro.breaks <- get_ids(x$choro.breaks, i)
			x$bubble.size <- get_ids(x$bubble.size, i)
			x$bubble.col <- get_ids(x$bubble.col, i)
			x$bubble.legend.labels <- get_ids(x$bubble.legend.labels, i)
			x$bubble.legend.palette <- get_ids(x$bubble.legend.palette, i)
			x$text <- if(length(x$text) >= i) x$text[i] else x$text[1]
			x
		})
	})
	gps
}

get_ids <- function(x, i) {
	if (is.matrix(x)) {
		if (ncol(x)>=i) x[,i] else x[,1]
	} else if(is.list(x)) {
		if (length(x)>=i) x[[i]] else x[[1]]
	} else x
}

