process_raster <- function(data, g, gt, gby, z, interactive) {
	## general variables
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]

	# update gt$pc's saturation
	gt$pc$saturation <- gt$pc$saturation * g$saturation
	
	## general (!color aesthetic), color NA, alpha checks / defaults
	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE
	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_raster is not a numeric", call. = FALSE)
	
	## check raster shortcuts
	if ("PIXEL__COLOR" %in% names(data)) {
		x <- "PIXEL__COLOR"
		data$PIXEL__COLOR <- do.call("process_color", c(list(col=data$PIXEL__COLOR, alpha=g$alpha), gt$pc))
		is.colors <- TRUE
		nx <- 1
	} else if ("FILE__VALUES" %in% names(data)) {
		x <- "FILE__VALUES"
		is.colors <- FALSE
		nx <- 1
	} else {
		x <- g$col

		# by default, use the first data variable
		if (is.na(x[1])) x <- names(data)[1]

		## general 'by' check: if by => |aes| = 1, and determine nx
		if (nlevels(by)>1 && length(x) > 1) {
			warning("When by is specified (tm_facets), only one value can be assigned to each aesthetic.", call. = FALSE)
			x <- x[1]
		}
		nx <- length(x)
		
		# check for direct color input
		if (all(x %in% shpcols)) {
			is.colors <- FALSE
		} else {
			# check for direct color input
			is.colors <- all(valid_colors(x))
			if (!is.colors) stop("Invalid line colors", call. = FALSE)
			
			x <- do.call("process_color", c(list(col=col2hex(x), alpha=g$alpha), gt$pc))
			for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
			x <- paste("COLOR", 1:nx, sep="_")
		}
	}
	
	## set interpolate: TRUE if is.colors (i.e. image)
	interpolate <- ifelse(is.na(g$interpolate), is.colors, g$interpolate)
	
	
	dt <- process_data(data[, x, drop=FALSE], by=by, free.scales=gby$free.scales.raster, is.colors=is.colors)
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	if (nlevels(by)>1) if (is.na(g$showNA)) g$showNA <- attr(dt, "anyNA")
	
	
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	
	
	# return if data is matrix of color values
	if (is.matrix(dt)) {
		if (!is.colors) {
			dt <- matrix(do.call("process_color", c(list(col=dt, alpha=g$alpha), gt$pc)), 
						 ncol=ncol(dt))
		}
		is.OSM <- attr(data, "is.OSM")
		leaflet.server <- attr(data, "leaflet.server")
		
		if (is.null(is.OSM)) is.OSM <- FALSE
		if (is.null(leaflet.server)) leaflet.server <- NA
		
		return(list(raster=dt, xraster=rep(NA, nx), raster.legend.title=rep(NA, nx), raster.misc=list(is.OSM=is.OSM, leaflet.server=leaflet.server, interpolate=interpolate), raster.group = g$group))
	}
	
	dcr <- process_dtcol(dt, sel=TRUE, g, gt, nx, npol, reverse=g$legend.reverse)
	if (dcr$is.constant) x <- rep(NA, nx)
	col <- dcr$col
	col.legend.labels <- dcr$legend.labels
	col.legend.values <- dcr$legend.values
	col.legend.palette <- dcr$legend.palette
	col.nonemptyFacets <- dcr$nonemptyFacets
	
	breaks <- dcr$breaks
	values <- dcr$values
	
	
	
	
	raster.legend.title <- if (is.ena(g$title)[1]) x else g$title
	raster.legend.z <- if (is.na(g$legend.z)) z else g$legend.z
	raster.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.5 else g$legend.hist.z
	
	if (g$legend.hist && is.ena(g$legend.hist.title) && raster.legend.z>raster.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		raster.legend.hist.title <- raster.legend.title
		raster.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		raster.legend.hist.title <- g$legend.hist.title
	} else raster.legend.hist.title <- ""
	
	if (!g$legend.show) raster.legend.title <- NA
	
	
	list(raster=col,
		 raster.nonemptyFacets = col.nonemptyFacets,
		 raster.legend.labels=col.legend.labels,
		 raster.legend.values=col.legend.values,
		 raster.legend.palette=col.legend.palette,
		 raster.legend.misc=list(),
		 raster.legend.hist.misc=list(values=values, breaks=breaks),
		 raster.misc=list(is.OSM=FALSE, leaflet.server=NA, interpolate=interpolate),
		 xraster=x,
		 raster.legend.show=g$legend.show,
		 raster.legend.title=raster.legend.title,
		 raster.legend.is.portrait=g$legend.is.portrait,
		 raster.legend.reverse=g$legend.reverse,
		 raster.legend.hist=g$legend.hist,
		 raster.legend.hist.title=raster.legend.hist.title,
		 raster.legend.z=raster.legend.z,
		 raster.legend.hist.z=raster.legend.hist.z,
		 raster.group = g$group)
}
