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
	shape.id.orig <- which(names(x)=="tm_shape")
	facet.id.orig <- which(names(x)=="tm_facets")

	nshps <- length(shape.id.orig)
	
	facet.shp.id <- sapply(facet.id.orig, function(i){tail(which(shape.id.orig<i), 1)})
	
	facet.ids <- rep(0, nshps)
	if (length(facet.shp.id)) facet.ids[facet.shp.id] <- facet.id.orig
		
		
	
	gfs <- lapply(1:nshps, function(i){
		gf <- if (facet.ids[i]==0) tm_facets()$tm_facets else x[[facet.ids[i]]]
		gf$shp_name <- x[[shape.id.orig[i]]]$shp_name
		gf$shp_nr <- ifelse(!is.null(gf$by), i, 0)
		gf$by <- ifelse(is.null(gf$by), "", gf$by)
		gf
	})
	
	gf <- gfs[[1]]
	x[[shape.id.orig[1]]]$by <- gf$by
	if (nshps>1) {
		for (i in 2:nshps) {
			gf$shp_name <- c(gf$shp_name, gfs[[i]]$shp_name)
			gf$shp_nr <- c(gf$shp_nr, gfs[[i]]$shp_nr)
			gf_args <- setdiff(gfs[[i]]$call, "by")
			gf[gf_args] <- gfs[[i]][gf_args]
			x[[shape.id.orig[i]]]$by <- gfs[[i]]$by
		}
	}
	
# 	
# 	facetids <- which(names(x)=="tm_facets")
# 	if (length(facetids)) {
# 		shape.id.orig <- which(names(x)[1:facetid]=="tm_shape")
# 		gf.shp.id <- tail(shape.id.orig, 1)
# 		gf <- x[[facetid]]
# 		gf$shp_nr <- ifelse(!is.null(gf$by) && gf$free.coords, length(shape.id.orig), 0)
# 		gf$shp_name <- x[[gf.shp.id]]$shp_name
# 	} else {
# 		gf <- tm_facets()$tm_facets 
# 		gf$shp_nr <- 0
# 		gf$shp_name <- ""
# 	}
	
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
	data_by <- lapply(1:nshps, function(i) {
		if (gf$shp_nr[i]==0) {
			NULL
		} else {
			gp[[gf$shp_nr[i]]]$data_by
		}
	})

	## for each 'grouped by' shape, where split=TRUE, get order ids (used by split_tm) 
	order_by <- lapply(data_by, function(d) {
		if (is.null(d) || !gf$split) {
			NULL
		} else {
			lapply(1:nlevels(d), function(i)which(as.numeric(d)==i))
		}
	})
	

	by_counts <- sapply(data_by, nlevels)
	if (sum(by_counts>0)>1) {
		by_counts_pos <- by_counts[by_counts!=0]
		if (any(by_counts_pos[-1]!=by_counts_pos[1])) stop("Number of facets defined by the 'by' argument of tm_facets are different for the groups.")
	}


# 	if (gf$shp_nr == 0) {
# 		data_by <- NULL
# 	} else {
# 		data_by <- gp[[gf$shp_nr]]$data_by
# 	}

	## determine maximal number of variables
	
	nx <- max(sapply(gp, function(x) {
		max(length(x$varnames$by),
			ifelse(is.matrix(x$fill), ncol(x$fill), 1),
			ifelse(is.matrix(x$bubble.size), ncol(x$bubble.size), 1),
			ifelse(is.matrix(x$bubble.col), ncol(x$bubble.col), 1),
			ifelse(is.matrix(x$line.col), ncol(x$line.col), 1),
			ifelse(is.matrix(x$line.lwd), ncol(x$line.lwd), 1),
			ifelse(is.matrix(x$text), ncol(x$text), 1))
	}))
	
	nx2 <- max(sapply(gp, function(x){
		max(sapply(x$varnames, length))
	}))
	cat("nx:", nx, " nx2:", nx2, "\n")
	
	
	names(gp) <- paste0("tmLayer", 1:length(gp))
	
	## get variable names (used for titles)
	varnames <- process_varnames(gp, nx)

	## process grid
	gmeta <- process_meta(gt, gf, gg, nx, varnames)
	## split into small multiples

	gps <- split_tm(gp, nx, order_by)
	scale <- gmeta$scale
	gps <- mapply(function(x, i){
		x <- lapply(x, function(xx) {
			within(xx, {
				lwd <- lwd * scale
				
				if (!is.null(fill)) {
					#if (!is.null(data_by)) fill <- fill[i]
					if (!is.na(xfill[1])) fill.legend.misc$lwd <- fill.legend.misc$lwd * scale
				}

				if (!is.null(bubble.size)) {
# 					if (!is.null(data_by)) {
# 						bubble.size <- bubble.size[i]
# 						bubble.col <- bubble.col[i]
# 					}
					
					bubble.size <- bubble.size * scale
					bubble.border.lwd <- bubble.border.lwd * scale
					bubble.col.legend.misc$bubble.max.size <- bubble.col.legend.misc$bubble.max.size * scale
					bubble.col.legend.misc$bubble.border.lwd <- bubble.col.legend.misc$bubble.border.lwd * scale
					
					bubble.size.legend.misc$legend.sizes <- bubble.size.legend.misc$legend.sizes * scale
					bubble.size.legend.misc$bubble.border.lwd <- bubble.size.legend.misc$bubble.border.lwd * scale
				}
				
				if (!is.null(line.lwd)) {
# 					if (!is.null(data_by)) {
# 						line.lwd <- line.lwd[i]
# 						line.col <- line.col[i]
# 					}
					
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
