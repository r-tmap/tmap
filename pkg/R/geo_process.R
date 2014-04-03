#' Print geo object
#' 
#' Print geo object
#' 
#' @param g geo object
#' @import sp
#' @import RColorBrewer
#' @import grid
#' @import gridBase
#' @import classInt
#' @export
print.geo <- function(g) {
	## fill meta info
	meta_layers <- c("geo_theme", "geo_grid")
	for (m in meta_layers) if (is.null(g[[m]])) g <- g + do.call(m, args=list())
	
	## split g into gmeta and gbody
	gmeta <- g[meta_layers]
	gbody <- g[!(names(g) %in% meta_layers)]
	
	
	n <- length(gbody)
	
	## split g into clusters
	shape.id <- which(names(gbody)=="geo_shape")
	if (!length(shape.id)) stop("Required geo_shape layer missing.")
	if (shape.id[1] != 1) stop("First layers should be a geo_shape layer.")
	x <- rep(0, n); x[shape.id] <- 1
	cluster.id <- cumsum(x)
	
	## unify projections
	gbody[shape.id] <- process_projection(gbody[shape.id])
	
	gs <- split(gbody, cluster.id)

	## convert clusters to layers
	gp <- lapply(gs, FUN=process_layers, free.scales=gmeta$geo_grid$free.scales,
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
	
	names(gps) <- paste0("plot", 1:nx)
	gps <- mapply(function(x, i){
		x$geo_theme <- gmeta$geo_theme
		x$geo_theme$title <- x$geo_theme$title[i]
		x
	}, gps, 1:nx, SIMPLIFY=FALSE)
	
	plot.new()
	gridplot(gmeta$geo_grid$nrow, gmeta$geo_grid$ncol, "plot_all", nx, gps)
}

process_varnames <- function(gp, nx) {
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

process_projection <- function(g) {
	# find master
	masterID <- which(sapply(g, function(x)!is.null(x$projection) || !is.null(x$xlim) || !is.null(x$ylim) || !is.null(x$bbox)))
	
	if (length(masterID)>1) {
		warning("Multiple projections or bounding boxes defined. First one is taken.")
		masterID <- masterID[1]
	}
	if (!length(masterID)) masterID <- 1

	# get master shape and info
	shp <- g[[masterID]]$shp
	projection <- g[[masterID]]$projection
	xlim <- g[[masterID]]$xlim
	ylim <- g[[masterID]]$ylim
	relative <- g[[masterID]]$relative
	bbox <- g[[masterID]]$bbox
	shp.proj <- proj4string(shp)
	
	# edit and set projection
	if (!is.null(projection)) {
		projection <- switch(projection,
					   longlat="+proj=longlat +datum=WGS84",
					   wintri="+proj=wintri",
					   robin="+proj=robin",
					   eck4="+proj=eck4",
					   hd=,
					   merc=,
					   mill=,
					   eqc0="+proj=eqc",
					   eqc30="+proj=cea +lat_ts=30",
					   eqc45="+proj=cea +lat_ts=45",
					   rd="+init=epsg:28992 +towgs84=565.237,50.0087,465.658,-0.406857,0.350733,-1.87035,4.0812",
							 projection)
		
		if (is.na(shp.proj)) {
			warning("Currect projection of shape object unknown. Long-lat (WGS84) is assumed.")
			shp@proj4string <- CRS("+proj=longlat +datum=WGS84")
		}
		shp <- spTransform(shp, CRS(projection))
		g[[masterID]]$shp <- shp
		g[-masterID] <- lapply(g[-masterID], function(x) {
			if (is.na(shp.proj)) {
				x$shp@proj4string <- CRS("+proj=longlat +datum=WGS84")
			}
			x$shp <- spTransform(x$shp, CRS(projection))
			x
		})
	}	
	
	# define bounding box
	shp.bbox <- bbox(shp)
	if (!is.null(bbox)) {
		bbox <- bbox
	} else {
		if (relative) {
			steps <- shp.bbox[, 2] - shp.bbox[, 1]
			xlim <- if (is.null(xlim)) {
				shp.bbox[1, ]
			} else {
				shp.bbox[1,1] + xlim * steps[1]
			}
			ylim <- if (is.null(ylim)) {
				shp.bbox[2, ]
			} else {
				shp.bbox[2,1] + ylim * steps[2]
			}
		}
		bbox <- matrix(c(xlim, ylim), ncol = 2, byrow=TRUE, 
					   dimnames=list(c("x", "y"), c("min", "max")))
	}
	
	## set bounding box
	g <- lapply(g, function(x){
		x$shp@bbox <- bbox
		x
	})
	
	g
}

process_meta <- function(g, nx, varnames) {
	
	g$geo_grid <- within(g$geo_grid, {
		if (is.null(ncol) && is.null(nrow)) {
			## default setting: place next to each other, or in grid
			if (nx <= 3) {
				ncol <- nx
				nrow <- 1
			} else {
				ncol <- ceiling(sqrt(nx))
				nrow <- ceiling(nx / ncol)
			}
		} else {
			if (is.null(ncol)) ncol <- 1
			if (is.null(nrow)) nrow <- 1
		}
	})
	
	g$geo_theme <- within(g$geo_theme, {
		if (is.null(title)) {
			id <- which(as.logical(sapply(varnames, function(x)sum(!is.na(x[1])))))[1]
			title <- if (!is.na(id)) varnames[[id]] else rep("", nx)
		} else title <- switch(title[1],
							   choro.fill=varnames[[1]],
							   bubble.size=varnames[[2]],
							   bubble.col=varnames[[3]],
							   title)
		if (length(title) < nx) title <- rep(title, length.out=nx)
	
		if (is.null(show.legend.text)) show.legend.text <- (!is.na(varnames$choro.fill[1]) || !is.na(varnames$bubble.col[1]))
		if (is.null(type.legend.plot)) type.legend.plot <- ifelse(!is.na(varnames$choro.fill[1]), "hist", 
																				ifelse(!is.na(varnames$bubble.size[1]), "bubble", "none"))
		
		if (is.na(legend.plot.size[1])) legend.plot.size <- if (legend.only) c(0.4, 0.9) else c(0.2,0.35)
	})	
	g
}

split_geo <- function(gp, nx) {
	lapply(1:nx, function(i){
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
			x$bubble.legend.sizes <- get_ids(x$bubble.legend.sizes, i)
			x$text <- if(length(x$text) >= i) x$text[i] else x$text[1]
			x
		})
	})
}

get_ids <- function(x, i) {
	if (is.matrix(x)) {
		if (ncol(x)>=i) x[,i] else x[,1]
	} else if(is.list(x)) {
		if (length(x)>=i) x[[i]] else x[[1]]
	} else x
}

