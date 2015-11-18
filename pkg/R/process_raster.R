process_raster_vector <- function(x, g, gt) {
	textNA <- ifelse(any(is.na(x)) && !is.na(g$colorNA), g$textNA, NA)
	
	if (is.factor(x)) {
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.ordered(x), "seq", "cat")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		colsLeg <- cat2pal(x,
						   palette = palette,
						   contrast = g$contrast,
						   colorNA = g$colorNA,
						   legend.labels=g$labels,
						   legend.NA.text = textNA,
						   max_levels=g$max.categories,
						   process.colors=c(list(alpha=g$alpha), gt$pc))
		raster.breaks <- NA
	} else {
		is.diverging <-  use_diverging_palette(x, g$breaks)
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.diverging, "div", "seq")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.NA.text = textNA,
						   process.colors=c(list(alpha=g$alpha), gt$pc),
						   legend.format=g$legend.format)
		raster.breaks <- colsLeg$breaks
	}
	rast <- colsLeg$cols
	raster.legend.labels <- colsLeg$legend.labels
	raster.legend.palette <- colsLeg$legend.palette
	
	return(list(raster=rast, 
				raster.legend.labels=raster.legend.labels,
				raster.legend.palette=raster.legend.palette,
				raster.breaks=raster.breaks))
}


process_raster <- function(data, g, gt, gby, z) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]

	# update legend format from tm_layout
	to_be_assigned <- setdiff(names(gt$legend.format), names(g$legend.format))
	g$legend.format[to_be_assigned] <- gt$legend.format[to_be_assigned]
	
	# update gt$pc's saturation
	gt$pc$saturation <- gt$pc$saturation * g$saturation
	
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
		if (is.na(x)) x <- names(data)[1]
		
		# if by is specified, use first value only
		if (nlevels(by)>1) x <- x[1]
		nx <- length(x)
		
		# check for direct color input
		is.colors <- all(valid_colors(x))
		if (is.colors) {
			x <- do.call("process_color", c(list(col=col2hex(x), alpha=g$alpha), gt$pc))
			for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
			x <- paste("COLOR", 1:nx, sep="_")
		} else {
			if (!all(x %in% shpcols)) stop("Raster argument neither colors nor valid variable names")
		}
	}
	
	dt <- process_data(data[, x, drop=FALSE], by=by, free.scales=gby$free.scales.raster, is.colors=is.colors)
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	
	nx <- max(nx, nlevels(by))
		
	# return if data is matrix of color values
	if (is.matrix(dt)) {
		if (!is.colors) {
			dt <- matrix(do.call("process_color", c(list(col=dt, alpha=g$alpha), gt$pc)), 
						 ncol=ncol(dt))
		}
		return(list(raster=dt, xraster=rep(NA, nx), raster.legend.title=rep(NA, nx)))
	}
	if (is.list(dt)) {
		# multiple variables are defined
		gs <- split_g(g, n=nx)
		isNum <- sapply(dt, is.numeric)
		res <- mapply(process_raster_vector, dt, gs, MoreArgs = list(gt), SIMPLIFY = FALSE)
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
	raster.legend.title <- if (is.na(g$title)[1]) x else g$title
	raster.legend.z <- if (is.na(g$legend.z)) z else g$legend.z
	raster.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.5 else g$legend.hist.z
	
	if (g$legend.hist && is.na(g$legend.hist.title) && raster.legend.z>raster.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		raster.legend.hist.title <- raster.legend.title
		raster.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		raster.legend.hist.title <- g$legend.hist.title
	} else raster.legend.hist.title <- ""
	
	
	list(raster=rast,
		 raster.legend.labels=raster.legend.labels,
		 raster.legend.palette=raster.legend.palette,
		 raster.legend.misc=list(),
		 raster.legend.hist.misc=list(values=raster.values, breaks=raster.breaks),
		 xraster=x,
		 raster.legend.show=g$legend.show,
		 raster.legend.title=raster.legend.title,
		 raster.legend.is.portrait=g$legend.is.portrait,
		 raster.legend.hist=g$legend.hist,
		 raster.legend.hist.title=raster.legend.hist.title,
		 raster.legend.z=raster.legend.z,
		 raster.legend.hist.z=raster.legend.hist.z)
}
