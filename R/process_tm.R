process_tm <- function(x, gt, gm, interactive) {
	fill <- NULL; xfill <- NULL; xraster <- NULL; text <- NULL; raster <- NULL
	## fill meta info
	
	## get grid element
	gridid <- which(names(x)=="tm_grid")[1]
	if (length(gridid)>1) gridid <- gridid[length(gridid)]
	gg <- x[[gridid]]
	
	## get credits, scale_bar, compass, and logo element
	sc_compIDs <- match(c("tm_scale_bar", "tm_compass"), names(x))
	gsb <- x[[sc_compIDs[1]]]
	gcomp <- x[[sc_compIDs[2]]]
	if (!is.null(gsb)) gsb$scale.id <- sc_compIDs[1]
	if (!is.null(gcomp)) gcomp$compass.id <- sc_compIDs[2]
	gcIDs <- which(names(x)=="tm_credits")
	if (is.na(gcIDs[1])) {
		gc <- NULL
	} else {
		gc <- do.call("mapply", c(x[gcIDs], list(nm=names(x[gcIDs][[1]]), FUN=function(..., nm) {
			if (nm %in% c("credits.text", "credits.position", "credits.just")) {
				unname(list(...))
			} else unname(c(...))
		}, SIMPLIFY=FALSE)))  
		#gc$credits.position <- unname(lapply(x[gcIDs], "[[", "credits.position"))
		gc$credits.id <- gcIDs
	}
	glIDs <- which(names(x)=="tm_logo")
	if (is.na(glIDs[1])) {
		gl <- NULL
	} else {
		if (any(vapply(glIDs, function(i) is.null(x[[i]]$logo.file), logical(1)))) stop("Logo file missing", call.=FALSE)
		gl <- do.call("mapply", c(x[glIDs], list(nm=names(x[glIDs][[1]]), FUN=function(..., nm) {
			if (nm %in% c("logo.file", "logo.position", "logo.just", "logo.height")) {
				unname(list(...))
			} else unname(c(...))
		}, SIMPLIFY=FALSE)))  
		gl$logo.file <- lapply(gl$logo.file, function(lf) if (!is.list(lf)) list(lf) else lf)
		gl$logo.height <- lapply(gl$logo.height, function(lh) if (!is.list(lh)) list(lh) else lh)
		gl$logo.id <- glIDs
	}
	
	# get xlab and ylab element
	gxlabid <- which(names(x)=="tm_xlab")[1]
	if (length(gxlabid)>1) gxlabid <- gxlabid[length(gxlabid)]
	gylabid <- which(names(x)=="tm_ylab")[1]
	if (length(gylabid)>1) gylabid <- gylabid[length(gylabid)]
	glab <- c(if(is.na(gxlabid)) NULL else x[[gxlabid]],
			  if(is.na(gylabid)) NULL else x[[gylabid]])
			  
	# get minimap element
	gmmid <- which(names(x)=="tm_minimap")[1]
	gmm <- x[[gmmid]]
	
	# get mouse coordinates element
	gmmcid <- which(names(x)=="tm_mouse")[1]
	gmmc <- x[[gmmcid]]
	

	## get facets element
	shape.id.orig <- which(names(x)=="tm_shape")
	facet.id.orig <- which(names(x)=="tm_facets")
	nshps <- length(shape.id.orig)
	facet.shp.id <- vapply(facet.id.orig, function(i){tail(which(shape.id.orig<i), 1)}, integer(1))
	facet.ids <- rep(0, nshps)
	if (length(facet.shp.id)) facet.ids[facet.shp.id] <- facet.id.orig
	# create gf for each shape
	gfs <- lapply(1:nshps, function(i){
		gf <- if (facet.ids[i]==0) tm_facets()$tm_facets else x[[facet.ids[i]]]
		gf$shp_name <- x[[shape.id.orig[i]]]$shp_name
		gf$shp_nr <- ifelse(!is.null(gf$by) || !is.null(gf$along), i, 0)
		
		if (is.null(gf$free.scales)) {
			gf$treat_as_by <- !is.null(x[[shape.id.orig[i]]]$data) && attr(x[[shape.id.orig[i]]]$data, "treat_as_by")
			if (gf$treat_as_by) gf$call <- c(gf$call, "free.scales")
			gf$free.scales <- is.null(gf$along) && is.null(gf$by) && !gf$treat_as_by
		} 
		free_scales_id <- which(substr(names(gf), 1, 12) == "free.scales.")
		for (f in free_scales_id) if (is.null(gf[[f]])) gf[[f]] <- gf$free.scales
		
		gf$by <- if (is.null(gf$by)) "" else gf$by
		gf$along <- if (is.null(gf$along)) "" else gf$along
		gf
	})
	
	# get first specified gf
	startID <- which(vapply(gfs, FUN = function(x)!is.null(x$call), FUN.VALUE = 1)==1)[1]
	if (is.na(startID)) startID <- 1
	gf <- gfs[[startID]]
	if (startID>1) {
		gf$shp_name <- vapply(1:startID, FUN = function(x) gfs[[x]]$shp_name, FUN.VALUE = character(1))
		gf$shp_nr <- vapply(1:startID, FUN = function(x) gfs[[x]]$shp_nr, FUN.VALUE = numeric(1))
	}
	for (i in 1:startID) {
		x[[shape.id.orig[i]]]$by <- gfs[[i]]$by
		x[[shape.id.orig[i]]]$along <- gfs[[i]]$along
	}
	
	# update with remaining gf's
	if (nshps>startID) {
		for (i in (startID+1):nshps) {
			gf$shp_name <- c(gf$shp_name, gfs[[i]]$shp_name)
			gf$shp_nr <- c(gf$shp_nr, gfs[[i]]$shp_nr)
			gf_args <- setdiff(gfs[[i]]$call, c("by", "along"))
			gf[gf_args] <- gfs[[i]][gf_args]
			x[[shape.id.orig[i]]]$by <- gfs[[i]]$by
			x[[shape.id.orig[i]]]$along <- gfs[[i]]$along
		}
	}
	xnames <- names(x)

	## find tm_grid position
	if ("tm_grid" %in% xnames) {
		gridID <- which(xnames=="tm_grid")[1]
		shapeID <- which(xnames=="tm_shape")
		gridGrp <- tail(which(shapeID<gridID), 1)
		gridShpPos <- shapeID[gridGrp]
		
		gridPos <- if (gridID == gridShpPos+1) 1 else {
			belowGridLayers <- xnames[(gridShpPos+1):(gridID-1)]
			fillBorderID <- which(belowGridLayers %in% c("tm_fill", "tm_borders"))
			if (length(fillBorderID) >= 2) {
				belowGridLayers <- belowGridLayers[-fillBorderID[-1]]
			}
			sum(!(belowGridLayers %in% c("tm_layout", "tm_view", "tm_style", "tm_facets", "tm_credits", "tm_logo", "tm_compass", "tm_scale_bar", "tm_xlab", "tm_ylab"))) + 1
		}
	} else {
		gridGrp <- 0
	}
	
	
	## split x into gmeta and gbody
	x <- x[!(xnames %in% c("tm_layout", "tm_view", "tm_style", "tm_grid", "tm_facets", "tm_credits", "tm_logo", "tm_compass", "tm_scale_bar", "tm_xlab", "tm_ylab"))] #, "tm_add_legend"

	legids <- which(names(x)=="tm_add_legend") 
	if (length(legids)) {
		names(x)[legids] <- paste(names(x)[legids], 1L:length(legids), sep="_")
		
		for (legid in legids) {
			if (x[[legid]]$type == "symbol" && is.list(x[[legid]]$shape)) {
				just <- x[[legid]]$just
				
				x[[legid]]$shape <- submit_symbol_shapes(x[[legid]]$shape, interactive=interactive, just = c(.5, .5),
														 just.override = FALSE, grob.dim = c(width=48, height=48, render.width=256, render.height=256))
			}
		}
		
	}
	
	
	
	n <- length(x)
	
	## split x into clusters
	shape.id <- which(names(x)=="tm_shape")
	if (shape.id[1] != 1) stop("First element should be a tm_shape element.", call. = FALSE)
	y <- rep(0, n); y[shape.id] <- 1
	cluster.id <- cumsum(y)
	gs <- split(x, cluster.id)
	
	nlx <- vapply(gs, length, integer(1))
	if (any(nlx==1)) stop("Specify at least one layer after each tm_shape", call. = FALSE)
	
	## convert clusters to layers
	cnlx <- if (nshps==1) 0 else c(0, cumsum(nlx[1:(nshps-1)]-1))
	gp <- mapply(FUN=process_layers, gs, cnlx, MoreArgs = list(gt=gt, gf=gf, interactive=interactive), SIMPLIFY = FALSE)
	names(gp) <- paste0("tmLayer", 1:length(gp))
	
	gal <- do.call(c, lapply(gp, function(g) g$add_legends))
	gp <- lapply(gp, function(g) {
		g$add_legends <- NULL
		g
	})
	
	
	## add tm_grid to plot order
	if (gridGrp!=0) {
		plot.order <- gp[[gridGrp]]$plot.order
		if (gridPos==1) {
			plot.order <- c("tm_grid", plot.order)
		} else if (gridPos==length(plot.order)+1) {
			plot.order <- c(plot.order, "tm_grid")
		} else {
			plot.order <- c(plot.order[1:(gridPos-1)], "tm_grid", plot.order[gridPos:length(plot.order)])
		}
		gp[[gridGrp]]$plot.order <- plot.order
	}
	
	## get by vector
	data_by <- lapply(gp, function(i)i$data_by)

	## for raster: ignore drop.units
	is_raster <- lapply(gp, function(i)!is.null(i$raster))
	
	## for each 'grouped by' shape, where drop.units=TRUE, get order ids (used by split_tm) 
	order_by <- mapply(function(d, isr) {
		if (levels(d)[1]=="___NA___" || !gf$drop.units || isr) {
			NULL
		} else {
			lapply(1:nlevels(d), function(i)which(as.numeric(d)==i))
		}
	}, data_by, is_raster, SIMPLIFY=FALSE)
	
	
	
	## get treat_by_counts vector
	treat_by_counts <- vapply(gp, function(i)i$treat_by_count, integer(1))

	## check if by is consistent among groups
	by_counts <- vapply(data_by, nlevels, integer(1))
	if (any(treat_by_counts > 1)) by_counts[treat_by_counts>1] <- treat_by_counts[treat_by_counts>1]
	
	if (sum(by_counts>1)>1) {
		by_counts_pos <- by_counts[by_counts>1]
		if (any(by_counts_pos[-1]!=by_counts_pos[1])) stop("Number of facets defined by the 'by' argument of tm_facets are different for the groups.", call. = FALSE)
		
	}
	
	if (any(by_counts>1)) {
		# get panel names
		panel.names <- gp[[which(by_counts>1)[1]]]$panel.names
	} else {
		panel.names <- NA
	}
	

	## check number of levels for two variables and override gf
	ncols <- vapply(gp, function(i)i$ncol, integer(1))
	nrows <- vapply(gp, function(i)i$nrow, integer(1))
	if (any(!is.na(ncols))) {
		if (!all(na.omit(ncols)==na.omit(ncols)[[1]]) | !all(na.omit(nrows)==na.omit(nrows)[[1]])) stop("Inconsistent levels of the 'by' argument of tm_facets.", call.=FALSE)
		gf$ncol <- na.omit(ncols)[1]
		gf$nrow <- na.omit(nrows)[1]
	}
	
	
	## determine number of small multiples
	nxl <- unname(vapply(gp, function(x){
		max(vapply(x$varnames, length, integer(1)))
	}, integer(1)))
	
	nx <- max(nxl)
	#layer_vary <- unname(which(nxl==nx))
	layer_vary <- unname(which(nxl>1))
	
	
	servers <- unname(vapply(gp, function(x) {
		if ("raster.misc" %in% names(x)) {
			as.character(x$raster.misc$leaflet.server)
		} else as.character(NA)
	}, character(1)))
	if (any(!is.na(servers))) gt$basemaps <- servers[!is.na(servers)][1]
	

	any.legend <- any(vapply(gp, function(x)x$any.legend, logical(1))) || (length(legids))

	## get along names
	along_layer <- which(gf$shp_nr!=0)[1]
	if (is.na(along_layer)) along_layer <- 1
	along.names <- gp[[along_layer]]$along.names

	if (interactive && gf$as.layers) {
		if (!is.na(along.names) && gt$show.warnings) warning("along argument not used when as.layers = TRUE", call. = FALSE)
		along.names <- NA
		nxa <- nx
	} else {
		nxa <- nx / length(along.names)
		nxa <- process_limit_nx(nxa)
		nx <- nxa * length(along.names)
	}
	


	gmeta <- process_meta(gt, gf, gg, gc, gl, gsb, gcomp, glab, gmm, gmmc, nx, nxa, panel.names, along.names, layer_vary, gm, any.legend, interactive)
	panel.mode <- if (!gmeta$panel.show) {
		"none"
	} else if (is.list(panel.names)) {
		"both"
	} else "one"
	
	#if (panel.mode!="none") gmeta$title <- rep("", nx)
	
	## split into small multiples
	gps <- split_tm(gp, nx, order_by)
	scale <- gmeta$scale
	
	gal <- lapply(gal, function(x) {
		if (!is.null(x$size)) x$size <- x$size * scale
		if (!is.null(x$border.lwd)) x$border.lwd <- x$border.lwd * scale
		x
	})

	gps <- mapply(function(x, i){
		x <- lapply(x, function(xx) {
			within(xx, {
				lwd <- lwd * scale
				
				if (!is.null(fill)) {
					if (!is.na(xfill[1])) fill.legend.misc$lwd <- fill.legend.misc$lwd * scale
				}

				if (!is.null(symbol.size)) {
					
					symbol.size <- symbol.size * scale
					symbol.border.lwd <- symbol.border.lwd * scale
					#symbol.col.legend.misc$symbol.max.size <- symbol.col.legend.misc$symbol.max.size * scale
					#symbol.col.legend.misc$symbol.normal.size <- symbol.col.legend.misc$symbol.normal.size * scale
					symbol.col.legend.misc$symbol.border.lwd <- symbol.col.legend.misc$symbol.border.lwd * scale

					symbol.col.legend.sizes <- symbol.col.legend.sizes * scale
					
					#symbol.shape.legend.misc$symbol.max.size <- symbol.shape.legend.misc$symbol.max.size * scale
					symbol.shape.legend.misc$symbol.border.lwd <- symbol.shape.legend.misc$symbol.border.lwd * scale
					

					symbol.shape.legend.sizes <- symbol.shape.legend.sizes * scale
					
					symbol.size.legend.sizes <- symbol.size.legend.sizes * scale
					
					#symbol.size.legend.misc$legend.sizes <- symbol.size.legend.misc$legend.sizes * scale
					symbol.size.legend.misc$symbol.border.lwd <- symbol.size.legend.misc$symbol.border.lwd * scale
				}
				
				if (!is.null(line.lwd)) {
					
					line.lwd <- line.lwd * scale
					line.col.legend.misc$line.legend.lwd <- line.col.legend.misc$line.legend.lwd * scale
					line.lwd.legend.misc$legend.lwds <- line.lwd.legend.misc$legend.lwds * scale
				}
				
				if (!is.null(text)) {
					text.size <- text.size * scale
					text.size.legend.sizes <- text.size.legend.sizes * scale
					text.col.legend.misc$text.max.size <- text.col.legend.misc$text.max.size * scale
					#text.size.legend.misc$legend.sizes <- text.size.legend.misc$legend.sizes * scale
					
				}


				if (!is.null(raster)) {
					if (!is.na(xraster[1])) raster.legend.misc$lwd <- raster.legend.misc$lwd * scale
				}

			})
		})

		x
	}, gps, 1:nx, SIMPLIFY=FALSE)

	#gmeta$panel.names <- panel.names
	gmeta$panel.mode <- panel.mode

	list(gmeta=gmeta, gps=gps, gal=gal, nx=nx, nxl=nxl, data_by=data_by)
}
