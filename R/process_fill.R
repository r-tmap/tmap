process_fill <- function(data, g, gb, gt, gby, z, interactive) {
	
	npol <- nrow(data)
	by <- data$GROUP_BY
	
	shpcols <- names(data)[1:(ncol(data)-2)]

	x <- g$col
	# if (interactive) {
	# 	if (length(x)>1) warning("Facets are not supported in view mode yet. Only polygon fill aesthetic value \"", x[1], "\" will be shown.", call.=FALSE)
	# 	x <- x[1]	
	# } 

	if (length(x)==1 && is.na(x)[1]) x <- gt$aes.colors["fill"]
	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE

	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_polygons/tm_fill is not a numeric", call. = FALSE)
		
	# if by is specified, use first value only
	if (nlevels(by)>1) x <- x[1]
	nx <- length(x)
	
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
		
	dt <- process_data(data[, x, drop=FALSE], by=by, free.scales=gby$free.scales.fill, is.colors=is.colors)
	if (nlevels(by)>1) if (is.na(g$showNA)) g$showNA <- attr(dt, "anyNA")
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	g$popup.format <- process_popup_format(g$popup.format, gt$legend.format, g$popup.vars)
	
	# return if data is matrix of color values
	if (is.matrix(dt)) {
		if (!is.colors) {
			dt <- matrix(do.call("process_color", c(list(col=dt, alpha=g$alpha), gt$pc)),
						 ncol=ncol(dt))
		}
		return(list(fill=dt, 
					xfill=rep(NA, nx), 
					fill.lenged.title=rep(NA, nx),
					fill.id=g$id,
					fill.popup.vars=g$popup.vars,
					fill.popup.format=g$popup.format))	
	} 

	# process areas
	if (is.null(g$area)) {
		area_var <- "SHAPE_AREAS"
	} else {
		area_var <- g$area
	}
	
	areas <- data[[area_var]]
	areas_prop <- areas/sum(areas, na.rm=TRUE)
	
	tiny <- areas_prop < g$thres.poly
	if (all(tiny)) warning("all relative area sizes are below thres.poly", call. = FALSE)
	
	
	sel <- if (is.list(dt)) rep(list(!tiny), nx) else !tiny
	
	dcr <- process_dtcol(dt, sel, g, gt, nx, npol, check_dens = TRUE, areas=areas, areas_unit=attr(areas, "unit"), reverse=g$legend.reverse)
	if (dcr$is.constant) xfill <- rep(NA, nx)
	col <- dcr$col
	col.legend.labels <- dcr$legend.labels
	col.legend.values <- dcr$legend.values
	col.legend.palette <- dcr$legend.palette
	col.neutral <- dcr$col.neutral
	breaks <- dcr$breaks
	values <- dcr$values
	title_append <- dcr$title_append
	
	fill.legend.title <- if (is.ena(g$title)[1]) paste(x, title_append) else g$title
	fill.legend.z <- if (is.na(g$legend.z)) z else g$legend.z
	fill.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.5 else g$legend.hist.z
	
	if (g$legend.hist && is.ena(g$legend.hist.title) && fill.legend.z>fill.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		fill.legend.hist.title <- fill.legend.title
		fill.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		fill.legend.hist.title <- g$legend.hist.title
	} else fill.legend.hist.title <- ""

	if (!g$legend.show) fill.legend.title <- NA
	list(fill=col,
		 fill.legend.labels=col.legend.labels,
		 fill.legend.values=col.legend.values,
		 fill.legend.palette=col.legend.palette,
		 fill.legend.misc=list(lwd=gb$lwd, border.col=gb$col),
		 fill.legend.hist.misc=list(values=values, breaks=breaks, densities=g$convert2density),
		 xfill=x,
		 fill.legend.show=g$legend.show,
		 fill.legend.title=fill.legend.title,
		 fill.legend.is.portrait=g$legend.is.portrait,
		 fill.legend.reverse=g$legend.reverse,
		 fill.legend.hist=g$legend.hist,
		 fill.legend.hist.title=fill.legend.hist.title,
		 fill.legend.z=fill.legend.z,
		 fill.legend.hist.z=fill.legend.hist.z,
		 fill.id=g$id,
		 fill.popup.vars=g$popup.vars,
		 fill.popup.format=g$popup.format)
}
