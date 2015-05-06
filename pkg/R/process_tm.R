process_tm <- function(x, asp_ratio) {
	fill <- NULL; xfill <- NULL; xraster <- NULL
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
	
	
	## split x into gmeta and gbody
	x <- x[!(names(x) %in% c("tm_layout", "tm_grid", "tm_facets"))]

	n <- length(x)
	
	## split x into clusters
	shape.id <- which(names(x)=="tm_shape")
	if (shape.id[1] != 1) stop("First element should be a tm_shape element.")
	y <- rep(0, n); y[shape.id] <- 1
	cluster.id <- cumsum(y)
	gs <- split(x, cluster.id)
	
	nlx <- sapply(gs, length)
	if (any(nlx==1)) warning("Specify at least one layer after each tm_shape")
	
	
	## convert clusters to layers
	cnlx <- if (nshps==1) 0 else c(0, cumsum(nlx[1:(nshps-1)]-1))
	gp <- mapply(FUN=process_layers, gs, cnlx, MoreArgs = list(gt=gt, gf=gf), SIMPLIFY = FALSE)
	
	## get by vector
	data_by <- lapply(gp, function(i)i$data_by)

	## for each 'grouped by' shape, where drop.shapes=TRUE, get order ids (used by split_tm) 
	order_by <- lapply(data_by, function(d) {
		if (levels(d)[1]=="_NA_" || !gf$drop.shapes) {
			NULL
		} else {
			lapply(1:nlevels(d), function(i)which(as.numeric(d)==i))
		}
	})
	

	by_counts <- sapply(data_by, nlevels)
	if (sum(by_counts>1)>1) {
		by_counts_pos <- by_counts[by_counts>1]
		if (any(by_counts_pos[-1]!=by_counts_pos[1])) stop("Number of facets defined by the 'by' argument of tm_facets are different for the groups.")
	}



	nx <- max(sapply(gp, function(x){
		max(sapply(x$varnames, length))
	}))
	#cat("nx:", nx, " nx2:", nx2, "\n")
	
	
	names(gp) <- paste0("tmLayer", 1:length(gp))
	
	## get variable names (used for titles)
	#varnames <- process_varnames(gp, nx)
	
	
	## get by names
	by_names_list <- lapply(gp, function(gpl) gpl$varnames$by)
	by_names_specified <- !sapply(by_names_list, is.na)

	by_names <- if (any(by_names_specified)) by_names_list[[which(by_names_specified)[1]]] else NA
	## process grid
	gmeta <- process_meta(gt, gf, gg, nx, by_names, asp_ratio)
	
	## split into small multiples
	gps <- split_tm(gp, nx, order_by)
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
					text.size <- text.size * scale
				}


				if (!is.null(raster)) {
					if (!is.na(xraster[1])) raster.legend.misc$lwd <- raster.legend.misc$lwd * scale
				}

			})
		})
		
		x$tm_layout <- gmeta
		x$tm_layout$title <- x$tm_layout$title[i]
		#x$tm_layout$legend.titles <- sapply(x$tm_layout$legend.titles, function(x)x[i])
		x
	}, gps, 1:nx, SIMPLIFY=FALSE)
	
	list(gmeta=gmeta, gps=gps, nx=nx, data_by=data_by)
}
