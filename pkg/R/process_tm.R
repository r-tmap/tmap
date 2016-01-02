process_tm <- function(x, asp_ratio, shp_info) {
	fill <- NULL; xfill <- NULL; xraster <- NULL; text <- NULL
	## fill meta info
	
	style <- options("tmap.style")
	tln <- paste("tm_style", style,sep="_" )
	if (!exists(tln)) {
		warning("Style ", style, " unknown; ", tln, " does not exist. Please specify another style with the option \"tmap.stype\".", call. = FALSE)
		tln <- "tm_style_default"
	}
	gt <- do.call(tln, args = list())$tm_layout

	gts <- x[names(x)=="tm_layout"]
	if (length(gts)) {
		gtsn <- length(gts)
		extraCall <- character(0)
		for (i in 1:gtsn) {
			gt[gts[[i]]$call] <- gts[[i]][gts[[i]]$call]
			if ("attr.color" %in% gts[[i]]$call) gt[c("earth.boundary.color", "legend.text.color", "title.color")] <- gts[[i]]["attr.color"]
			extraCall <- c(extraCall, gts[[i]]$call)
		}
		gt$call <- c(gt$call, extraCall)
	}

	## preprocess gt
	gtnull <- names(which(sapply(gt, is.null)))
	gt <- within(gt, {
		pc <- list(sepia.intensity=sepia.intensity, saturation=saturation)
		sepia.intensity <- NULL
		saturation <- NULL
		
		if (!"scientific" %in% names(legend.format)) legend.format$scientific <- FALSE
		if (!"digits" %in% names(legend.format)) legend.format$digits <- NA
		if (!"text.separator" %in% names(legend.format)) legend.format$text.separator <- "to"
		if (!"text.less.than" %in% names(legend.format)) legend.format$text.less.than <- "Less than"
		if (!"text.or.more" %in% names(legend.format)) legend.format$text.or.more <- "or more"
		
		# put aes colors in right order and name them
		if (length(aes.color)==1 && is.null(names(aes.color))) names(aes.color) <- "base"
		
		if (!is.null(names(aes.color))) {
			aes.colors <- c(fill="grey85", borders="grey40", bubbles="blueviolet", dots="black", lines="red", text="black", na="grey60")
			aes.colors[names(aes.color)] <- aes.color
		} else {
			aes.colors <- rep(aes.color, length.out=7)
			names(aes.colors) <- c("fill", "borders", "bubbles", "dots", "lines", "text", "na")
		}
		if (is.na(aes.colors[1])) aes.colors[1] <- "black"

		aes.colors.light <- sapply(aes.colors, is_light)

	})
	gt[gtnull] <- list(NULL)
	
	## get grid element
	gridid <- which(names(x)=="tm_grid")[1]
	gg <- x[[gridid]]
	
	## get credits, scale_bar and compass element
	sc_compIDs <- match(c("tm_scale_bar", "tm_compass"), names(x))
	gsb <- x[[sc_compIDs[1]]]
	gcomp <- x[[sc_compIDs[2]]]
	if (!is.null(gsb)) gsb$scale.id <- sc_compIDs[1]
	if (!is.null(gcomp)) gcomp$compass.id <- sc_compIDs[2]
	gcIDs <- which(names(x)=="tm_credits")
	if (is.na(gcIDs[1])) {
		gc <- NULL
	} else {
		gc <- do.call("mapply", c(x[gcIDs], list(FUN=function(...)unname(c(...)), SIMPLIFY=FALSE)))  
		gc$credits.position <- unname(lapply(x[gcIDs], "[[", "credits.position"))
		gc$credits.id <- gcIDs
	}
	
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
	
	xnames <- names(x)

	## find tm_grid position
	if ("tm_grid" %in% xnames) {
		gridID <- which(xnames=="tm_grid")
		shapeID <- which(xnames=="tm_shape")
		gridGrp <- tail(which(shapeID<gridID), 1)
		gridShpPos <- shapeID[gridGrp]
		
		gridPos <- if (gridID == gridShpPos+1) 1 else {
			belowGridLayers <- xnames[(gridShpPos+1):(gridID-1)]
			fillBorderID <- which(belowGridLayers %in% c("tm_fill", "tm_borders"))
			if (length(fillBorderID) >= 2) {
				belowGridLayers <- belowGridLayers[-fillBorderID[-1]]
			}
			sum(!(belowGridLayers %in% c("tm_layout", "tm_style", "tm_facets", "tm_credits", "tm_compass", "tm_scale_bar"))) + 1
		}
	} else {
		gridGrp <- 0
	}
	

	## split x into gmeta and gbody
	x <- x[!(xnames %in% c("tm_layout", "tm_style", "tm_grid", "tm_facets", "tm_credits", "tm_compass", "tm_scale_bar"))]

	n <- length(x)
	
	## split x into clusters
	shape.id <- which(names(x)=="tm_shape")
	if (shape.id[1] != 1) stop("First element should be a tm_shape element.", call. = FALSE)
	y <- rep(0, n); y[shape.id] <- 1
	cluster.id <- cumsum(y)
	gs <- split(x, cluster.id)
	
	nlx <- sapply(gs, length)
	if (any(nlx==1)) warning("Specify at least one layer after each tm_shape", call. = FALSE)
	
	## convert clusters to layers
	cnlx <- if (nshps==1) 0 else c(0, cumsum(nlx[1:(nshps-1)]-1))
	gp <- mapply(FUN=process_layers, gs, cnlx, MoreArgs = list(gt=gt, gf=gf), SIMPLIFY = FALSE)
	names(gp) <- paste0("tmLayer", 1:length(gp))
	
	
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

	## for each 'grouped by' shape, where drop.shapes=TRUE, get order ids (used by split_tm) 
	order_by <- lapply(data_by, function(d) {
		if (levels(d)[1]=="_NA_" || !gf$drop.shapes) {
			NULL
		} else {
			lapply(1:nlevels(d), function(i)which(as.numeric(d)==i))
		}
	})
	
	## check if by is consistent among groups
	by_counts <- sapply(data_by, nlevels)
	if (sum(by_counts>1)>1) {
		by_counts_pos <- by_counts[by_counts>1]
		if (any(by_counts_pos[-1]!=by_counts_pos[1])) stop("Number of facets defined by the 'by' argument of tm_facets are different for the groups.", call. = FALSE)
	}

	## determine number of small multiples
	nx <- max(sapply(gp, function(x){
		max(sapply(x$varnames, length))
	}))

	## get by names
	by_names_list <- lapply(gp, function(gpl) gpl$varnames$by)
	by_names_specified <- !sapply(by_names_list, function(b) is.na(b[1]))
	by_names <- if (any(by_names_specified)) by_names_list[[which(by_names_specified)[1]]] else NA
	
	## process meta
	gmeta <- process_meta(gt, gf, gg, gc, gsb, gcomp, nx, by_names, asp_ratio, shp_info)
	
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
					text.col.legend.misc$text.max.size <- text.col.legend.misc$text.max.size * scale
					text.size.legend.misc$legend.sizes <- text.size.legend.misc$legend.sizes * scale
					
				}


				if (!is.null(raster)) {
					if (!is.na(xraster[1])) raster.legend.misc$lwd <- raster.legend.misc$lwd * scale
				}

			})
		})
		
		x$tm_layout <- gmeta
		x$tm_layout$title <- x$tm_layout$title[i]
		x
	}, gps, 1:nx, SIMPLIFY=FALSE)
	
	list(gmeta=gmeta, gps=gps, nx=nx, data_by=data_by)
}
