## general color aesthetic, color NA, alpha checks / defaults
check_g <- function(g, gt) {
	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (is.null(g$colorNULL)) g$colorNULL <- "#00000000"
	if (is.na(g$colorNULL)[1]) g$colorNULL <- gt$aes.colors["null"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE
	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_polygons/tm_fill is not a numeric", call. = FALSE)
	g
}

check_fill_specials <- function(x, g, gt, shpcols, data, nx) {
	if (attr(data, "kernel_density") && !("col" %in% g$call) && "level" %in% shpcols) {
		is.colors <- FALSE
		x <- "level"
	} else if (!all(x %in% shpcols)) {
		# check for direct color input
		is.colors <- all(valid_colors(x))
		if (is.colors) {
			x <- do.call("process_color", c(list(col=col2hex(x), alpha=g$alpha), gt$pc))
			for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
			x <- paste("COLOR", 1:nx, sep="_")
		} else if (x[1]=="MAP_COLORS") {
			palette <- if (is.null(g$palette)) {
				gt$aes.palette[["cat"]]
			} else if (g$palette[1] %in% c("seq", "div", "cat")) {
				gt$aes.palette[[g$palette[1]]] 
			} else g$palette
			mapcols <- do.call("map_coloring", args = c(list(x=attr(data, "NB"), palette=palette, contrast = g$contrast), g$map_coloring))
			mapcols <- do.call("process_color", c(list(col=mapcols, alpha=g$alpha), gt$pc))
			
			for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- mapcols
			x <- paste("COLOR", 1:nx, sep="_")
		} else {
			stop("Fill argument neither colors nor valid variable name(s)", call. = FALSE)
		}
	} else {
		is.colors <- FALSE
	}
	
	list(x = x,
		 data = data,
		 is.colors = is.colors)
}


check_poly_sizes <- function(g, data, nx, islist) {
	# process areas
	if (is.null(g$area)) {
		area_var <- "SHAPE_AREAS"
	} else {
		area_var <- g$area
	}
	
	areas <- data[[area_var]]
	
	if (any(is.na(areas)) || any(is.infinite(areas))) {
		if (g$convert2density) {
			warning("Some polygon areas cannot be determined. Therefore, convert2density is set to FALSE.", call. = FALSE)
		}
		areas_na_inf <- is.na(areas) | is.infinite(areas)
		areas[areas_na_inf] <- mean(areas[!areas_na_inf])
		
	}
	areas_prop <- as.numeric(areas/sum(areas, na.rm=TRUE))
	
	tiny <- areas_prop < g$thres.poly
	if (all(tiny)) warning("all relative area sizes are below thres.poly", call. = FALSE)
	
	sel <- if (islist) rep(list(!tiny), nx) else !tiny
	list(areas = areas, sel = sel)
}


process_fill <- function(data, g, gb, gt, gby, z, interactive) {
	## general variables
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-2)]

	
	type <- "fill"
	
	## aesthetics
	xs <- list(fill = g$col)

	xs <- mapply(function(x, nm) {
		if (length(x)==1 && is.na(x)[1]) gt$aes.colors[nm] else x
	}, xs, names(xs), SIMPLIFY = FALSE)
	
	g <- check_g(g, gt)
	
		
	xlen <- sapply(xs, length)
	
	## general 'by' check: if by => |aes| = 1, and determine nx
	if (nlevels(by)>1 && any(xlen > 1)) warning("When by is specified (tm_facets), only one value can be assigned to each aesthetic.", call. = FALSE)
	
	
	nx <- max(xlen)
	
	## check fill special inputs (kernel density and map colors)
	
	if ("fill" %in% names(xs)) {
		res <- check_fill_specials(xs[["fill"]], g, gt, shpcols, data, nx)
		xs[["fill"]] <- res$x
		data <- res$data
		is.colors <- res$is.colors
	} else {
		is.colors <- FALSE
	}
	

	fsnames <- paste0("free.scales.", names(xs))
	
	dts <- mapply(function(x, fsname) {
		process_data(data[, x, drop=FALSE], filter = data$tmapfilter, by=by, free.scales=gby[[fsname]], is.colors=is.colors)
	}, xs, fsnames, SIMPLIFY = FALSE)
	
	
	
	
	
	## impute showNA for first (i.e. color) aesthetic
	if (nlevels(by)>1) if (is.na(g$showNA) && !gby[[fsnames[[1]]]]) g$showNA <- any(attr(dts[[1]], "anyNA") & !(gby$drop.NA.facets & attr(dts[[1]], "allNA")))
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	
	## recalculate nx, now taking by into account
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	g$popup.format <- process_popup_format(g$popup.format, gt$legend.format, g$popup.vars)

	
		
	# # return if data is matrix of color values
	# if (is.matrix(dt)) {
	# 	sel <- attr(dt, "sel")
	# 	allNA <- attr(dt, "allNA")
	# 	fillna <- is.na(dt)
	# 	dt[fillna] <- g$colorNA
	# 	dt[!sel] <- g$colorNULL
	# 	col.nonemptyFacets <- !allNA
	# 	
	# 	return(list(fill=dt, 
	# 				fill.nonemptyFacets = col.nonemptyFacets,
	# 				xfill=rep(NA, nx), 
	# 				fill.lenged.title=rep(NA, nx),
	# 				fill.id=g$id,
	# 				fill.popup.vars=g$popup.vars,
	# 				fill.popup.format=g$popup.format,
	# 				fill.group = g$group))	
	# } 
	
	if ("fill" %in% names(xs)) {
		res <- check_poly_sizes(g, data, nx, islist = is.list(dts[[1]]))
		areas <- res$areas
		sel <- res$sel
	} else {
		areas <- NULL
		sel <- NA
	}
	
	# ####################
	# ## back to old
	x <- xs[["fill"]]
	dt <- dts[[1]]
	# 
	# ############
	
	res <- mapply(function(x, xname, dt, fsname) {
		if (xname == "fill") {
			dcr <- process_dtcol(dt, sel, g, gt, nx, npol, check_dens = TRUE, areas=as.numeric(areas), areas_unit=attr(areas, "unit"), reverse=g[[aname("legend.reverse", xname)]])
			
			
			if (dcr$is.constant) x <- rep(NA, nx)
			# col <- dcr$col
			# col.legend.labels <- dcr$legend.labels
			# col.legend.values <- dcr$legend.values
			# col.legend.palette <- dcr$legend.palette
			# col.nonemptyFacets <- dcr$nonemptyFacets
			# col.neutral <- dcr$col.neutral
			# breaks <- dcr$breaks
			# values <- dcr$values
			# title_append <- dcr$title_append
			
			legend.show <- if (dcr$is.constant) rep(FALSE, nx) else rep(g[[aname("legend.show", xname)]], length.out = nx)
			
			if (nx > 1 && gby[[fsname]]) {
				emptyLegend <- sapply(dcr$col.legend.labels, function(ssll) is.na(ssll[1]))
				legend.show[emptyLegend] <- FALSE
			}
			
			
			legend.title <- if (is.ena(g[[aname("title", xname)]])[1]) paste(x, title_append) else g[[aname("title", xname)]]
			legend.z <- if (is.na(g[[aname("legend.z", xname)]])) z else g[[aname("legend.z", xname)]]
			legend.hist.z <- if (is.na(g[[aname("legend.hist.z", xname)]])) z+.5 else g[[aname("legend.hist.z", xname)]]
			
			if (g[[aname("legend.hist", xname)]] && is.ena(g[[aname("legend.hist.title", xname)]]) && legend.z>legend.hist.z) {
				# histogram is drawn between title and legend enumeration
				legend.hist.title <- legend.title
				legend.title <- ""
			} else if (g[[aname("legend.hist", xname)]] && !is.na(g[[aname("legend.hist.title", xname)]])) {
				legend.hist.title <- g[[aname("legend.hist.title", xname)]]
			} else legend.hist.title <- ""
			
			#if (!g$legend.show) fill.legend.title <- NA
			
			if (any(!legend.show)) legend.title[!legend.show] <- NA
			
			
			
			c(dcr, list())
		} else {
			NULL
		}
		
		
		
		
		
		
	}, xs, names(xs), dts, fsnames, SIMPLIFY = FALSE)
	

	list(
		fill = list(col,
					x,
					legend.labels,
					legend.values,
					legend.palette,
					legend.misc,
					legend.hist.misc,
					legend.show,
					legend.title,
					legend.is.portrait,
					legend.reverse,
					legend.hist,
					legend.hist.title,
					legend.z,
					legend.hist.z),
		polygons = list(
			nonemptyFacets = col.nonemptyFacets,
			id = g$id,
			popup.vars = g$popup.vars,
			popup.format = g$popup.format,
			group = g$group)
	)
	
	# list(fill=col,
	# 	 fill.legend.labels=col.legend.labels,
	# 	 fill.legend.values=col.legend.values,
	# 	 fill.legend.palette=col.legend.palette,
	# 	 fill.legend.misc=list(lwd=gb$lwd, border.col=gb$col),
	# 	 fill.legend.hist.misc=list(values=values, breaks=breaks, densities=g$convert2density),
	# 	 xfill=x,
	# 	 fill.legend.show=fill.legend.show,
	# 	 fill.legend.title=fill.legend.title,
	# 	 fill.legend.is.portrait=g$legend.is.portrait,
	# 	 fill.legend.reverse=g$legend.reverse,
	# 	 fill.legend.hist=g$legend.hist,
	# 	 fill.legend.hist.title=fill.legend.hist.title,
	# 	 fill.legend.z=fill.legend.z,
	# 	 fill.legend.hist.z=fill.legend.hist.z,
	# 	 
	# 	 fill.nonemptyFacets = col.nonemptyFacets,
	# 	 fill.id=g$id,
	# 	 fill.popup.vars=g$popup.vars,
	# 	 fill.popup.format=g$popup.format,
	# 	 fill.group = g$group)
}

aname <- function(x, a) {
	if (a == "fill") {
		x
	} else {
		x
	}
}
