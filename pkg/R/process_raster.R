process_raster_vector <- function(x, g, gt) {
	textNA <- ifelse(any(is.na(x)) && !is.na(g$colorNA), g$textNA, NA)
	
	if (is.factor(x)) {
		palette <- if (is.null(g$palette)) ifelse(nlevels(x)>8, "Set3", "Dark2") else g$palette
		colsLeg <- cat2pal(x,
						   palette = palette,
						   colorNA = g$colorNA,
						   legend.NA.text = textNA,
						   max_levels=g$max.categories)
		raster.breaks <- NA
	} else {
		palette <- if (is.null(g$palette)) "RdYlGn" else g$palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.scientific=gt$legend.scientific,
						   legend.digits=gt$legend.digits,
						   legend.NA.text = textNA)
		raster.breaks <- colsLeg[[4]]
	}
	rast <- colsLeg[[1]]
	raster.legend.labels <- colsLeg[[2]]
	raster.legend.palette <- colsLeg[[3]]
	
	return(list(raster=rast, 
				raster.legend.labels=raster.legend.labels,
				raster.legend.palette=raster.legend.palette,
				raster.breaks=raster.breaks))
}


process_raster <- function(data, g, gt, gby) {
	#browser()
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]
	
	x <- g$col
	# if by is specified, use first value only
	if (nlevels(by)>1) x <- x[1]
	nx <- length(x)
	
	# check for direct color input
	if (all(valid_colors(x))) {
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
		x <- paste("COLOR", 1:nx, sep="_")
	} else {
		if (!all(x %in% shpcols)) stop("Raster argument neither colors nor valid variable names")
	}
	dt <- process_data(data[, x, drop=FALSE], by=by, free.scales=gby$free.scales.raster)
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	nx <- max(nx, nlevels(by))
		
	# return if data is matrix of color values
	if (is.matrix(dt)) return(list(raster=dt, raster.alpha=g$alpha, xraster=rep(NA, nx)))
	if (is.list(dt)) {
		isNum <- sapply(dt, is.numeric)
		res <- lapply(dt, process_raster_vector, g, gt)
		rast <- sapply(res, function(r)r$raster)
		raster.legend.labels <- lapply(res, function(r)r$raster.legend.labels)
		raster.legend.palette <- lapply(res, function(r)r$raster.legend.palette)
		raster.breaks <- lapply(res, function(r)r$raster.breaks)
		raster.values <- dt
	} else {
		res <- process_raster_vector(dt, g, gt)
		rast <- matrix(res$raster, nrow=npol)
		raster.legend.labels <- res$raster.legend.labels
		raster.legend.palette <- res$raster.legend.palette
		raster.breaks <- res$raster.breaks
		raster.values <- split(dt, rep(1:nx, each=npol))
	}
	list(raster=rast,
		 raster.alpha=g$alpha,
		 raster.legend.labels=raster.legend.labels,
		 raster.legend.palette=raster.legend.palette,
		 raster.legend.misc=list(values=raster.values, breaks=raster.breaks, alpha=g$alpha),
		 xraster=x)
}
