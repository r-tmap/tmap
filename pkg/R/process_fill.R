process_fill <- function(data, g, gb, gt, gby, z, allow.small.mult) {
	
	npol <- nrow(data)
	by <- data$GROUP_BY
	
	shpcols <- names(data)[1:(ncol(data)-2)]

	x <- g$col
	if (!allow.small.mult) x <- x[1]

	if (is.na(x)[1]) x <- gt$aes.colors["fill"]
	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE

	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_polygons/tm_fill is not a numeric", call. = FALSE)
		
	# if by is specified, use first value only
	if (nlevels(by)>1) x <- x[1]
	nx <- length(x)
	
	# check for direct color input
	is.colors <- all(valid_colors(x))
	if (attr(data, "dasymetric") && !("col" %in% g$call) && "level" %in% shpcols) {
		is.colors <- FALSE
		x <- "level"
	} else if (is.colors) {
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
		if (!all(x %in% shpcols)) stop("Fill argument neither colors nor valid variable name(s)", call. = FALSE)
	}
	dt <- process_data(data[, x, drop=FALSE], by=by, free.scales=gby$free.scales.fill, is.colors=is.colors)
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	if (length(g$legend.format)==nx && all(sapply(g$legend.format, is.list))) {
		g$legend.format <- lapply(g$legend.format, function(lf) {
			to_be_assigned <- setdiff(names(gt$legend.format), names(lf))
			lf[to_be_assigned] <- gt$legend.format[to_be_assigned]
			lf
		})
	} else {
		to_be_assigned <- setdiff(names(gt$legend.format), names(g$legend.format))
		g$legend.format[to_be_assigned] <- gt$legend.format[to_be_assigned]
	}

	
	# return if data is matrix of color values
	if (is.matrix(dt)) {
		if (!is.colors) {
			dt <- matrix(do.call("process_color", c(list(col=dt, alpha=g$alpha), gt$pc)),
						 ncol=ncol(dt))
		}
		return(list(fill=dt, xfill=rep(NA, nx), fill.lenged.title=rep(NA, nx)))	
	} 

	# process areas
	if (is.null(g$area)) {
		show_warning <- (!attr(data, "AREAS_is_projected"))
		area_var <- "SHAPE_AREAS"
	} else {
		show_warning <- FALSE
		area_var <- g$area
	}
	
	areas <- data[[area_var]]
	areas_prop <- areas/sum(areas, na.rm=TRUE)
	
	tiny <- areas_prop < g$thres.poly
	if (all(tiny)) warning("all relative area sizes are below thres.poly", call. = FALSE)
	
	
	sel <- if (is.list(dt)) rep(list(!tiny), nx) else !tiny
	
	dcr <- process_dtcol(dt, sel, g, gt, nx, npol, check_dens = TRUE, show_warning=show_warning, areas=areas)
	if (dcr$is.constant) xfill <- rep(NA, nx)
	col <- dcr$col
	col.legend.labels <- dcr$legend.labels
	col.legend.palette <- dcr$legend.palette
	col.neutral <- dcr$col.neutral
	breaks <- dcr$breaks
	values <- dcr$values
	
	fill.legend.title <- if (is.na(g$title)[1]) x else g$title
	fill.legend.z <- if (is.na(g$legend.z)) z else g$legend.z
	fill.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.5 else g$legend.hist.z
	
	if (g$legend.hist && is.na(g$legend.hist.title) && fill.legend.z>fill.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		fill.legend.hist.title <- fill.legend.title
		fill.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		fill.legend.hist.title <- g$legend.hist.title
	} else fill.legend.hist.title <- ""

	if (!g$legend.show) fill.legend.title <- NA
	
	list(fill=col,
		 fill.legend.labels=col.legend.labels,
		 fill.legend.palette=col.legend.palette,
		 fill.legend.misc=list(lwd=gb$lwd, border.col=gb$col),
		 fill.legend.hist.misc=list(values=values, breaks=breaks, densities=g$convert2density),
		 xfill=x,
		 fill.legend.show=g$legend.show,
		 fill.legend.title=fill.legend.title,
		 fill.legend.is.portrait=g$legend.is.portrait,
		 fill.legend.hist=g$legend.hist,
		 fill.legend.hist.title=fill.legend.hist.title,
		 fill.legend.z=fill.legend.z,
		 fill.legend.hist.z=fill.legend.hist.z,
		 fill.id=g$id)
}
