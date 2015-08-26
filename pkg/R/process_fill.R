process_fill_vector <- function(x, g, gt, gst, tiny) {
	fill.values <- x
	textNA <- ifelse(any(is.na(fill.values)), g$textNA, NA)
	
	x[tiny] <- NA
	
	
	if (is.factor(x)) {
		palette <- if (is.null(g$palette)) ifelse(nlevels(x)>8, "Set3", "Dark2") else g$palette
		colsLeg <- cat2pal(x,
						   palette = palette,
						   contrast = g$contrast,
						   colorNA = g$colorNA,
						   legend.labels=g$labels,
						   legend.NA.text = textNA,
						   max_levels=g$max.categories,
						   process.colors=c(list(alpha=g$alpha), gst))
		fill.breaks <- NA
	} else {
		palette <- if (is.null(g$palette)) "RdYlGn" else g$palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.NA.text = textNA,
						   process.colors=c(list(alpha=g$alpha), gst),
						   legend.format=g$legend.format)
		fill.breaks <- colsLeg[[4]]
	}
	fill <- colsLeg[[1]]
	fill.legend.labels <- colsLeg[[2]]
	fill.legend.palette <- colsLeg[[3]]
	
	## fill tiny
	if (!is.na(fill.breaks[1])) {
		tmp_breaks <- fill.breaks
		tmp_breaks[1] <- -Inf
		tmp_breaks[length(tmp_breaks)] <- Inf
		tmp_int <- findInterval(fill.values[tiny], tmp_breaks)
		tmp_int[is.na(tmp_int)] <- length(fill.legend.palette)
		fill[tiny] <- fill.legend.palette[tmp_int]
	}
	return(list(fill=fill, 
				fill.legend.labels=fill.legend.labels,
				fill.legend.palette=fill.legend.palette,
				fill.breaks=fill.breaks))
}


process_fill <- function(data, g, gb, gt, gst, gby, z) {
	
	npol <- nrow(data)
	by <- data$GROUP_BY
	
	shpcols <- names(data)[1:(ncol(data)-2)]
	
	
	
	x <- g$col
	# if by is specified, use first value only
	if (nlevels(by)>1) x <- x[1]
	nx <- length(x)
	
	# check for direct color input
	is.colors <- all(valid_colors(x))
	if (is.colors) {
		x <- do.call("process_color", c(list(col=col2hex(x), alpha=g$alpha), gst))
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
		x <- paste("COLOR", 1:nx, sep="_")
	} else if (x[1]=="MAP_COLORS") {
		palette <- if (is.null(g$palette)) "Set2" else g$palette
		mapcols <- do.call("map_coloring", args = c(list(x=attr(data, "NB"), palette=palette, contrast = g$contrast), g$map_coloring))
		mapcols <- do.call("process_color", c(list(col=mapcols, alpha=g$alpha), gst))
		
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- mapcols
		x <- paste("COLOR", 1:nx, sep="_")
	} else {
		if (!all(x %in% shpcols)) stop("Fill argument neither colors nor valid variable names")
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
			dt <- matrix(do.call("process_color", c(list(col=dt, alpha=g$alpha), gst)),
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
	if (all(tiny)) warning("all relative area sizes are below thres.poly")
	if (is.list(dt)) {
		# multiple variables are defined
		gs <- split_g(g, n=nx)
		isNum <- sapply(dt, is.numeric)
		isDens <- sapply(gs, "[[", "convert2density")
		
		if (any(isNum & isDens) && show_warning) warning("Density values are not correct, because the shape coordinates are not projected.")
		dt[isNum & isDens] <- lapply(dt[isNum & isDens], function(d) {
			d / areas
		})
		res <- mapply(process_fill_vector, dt, gs, MoreArgs = list(gt, gst, tiny), SIMPLIFY = FALSE)
		fill <- sapply(res, function(r)r$fill)
		fill.legend.labels <- lapply(res, function(r)r$fill.legend.labels)
		fill.legend.palette <- lapply(res, function(r)r$fill.legend.palette)
		fill.breaks <- lapply(res, function(r)r$fill.breaks)
		fill.values <- lapply(dt, function(d)d[!tiny])
	} else {
		if (is.numeric(dt) && g$convert2density) {
			if (show_warning) warning("Density values are not correct, because the shape coordinates are not projected.")
			dt <- dt / areas
		}
		res <- process_fill_vector(dt, g, gt, gst, tiny)
		fill <- matrix(res$fill, nrow=npol)
		fill.legend.labels <- res$fill.legend.labels
		fill.legend.palette <- res$fill.legend.palette
		fill.breaks <- res$fill.breaks
		fill.values <- lapply(split(dt, rep(1:nx, each=npol)), function(d)d[!tiny])
	}
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
	
	list(fill=fill,
		 fill.legend.labels=fill.legend.labels,
		 fill.legend.palette=fill.legend.palette,
		 fill.legend.misc=list(lwd=gb$lwd, border.col=gb$col),
		 fill.legend.hist.misc=list(values=fill.values, breaks=fill.breaks),
		 xfill=x,
		 fill.legend.title=fill.legend.title,
		 fill.legend.is.portrait=g$legend.is.portrait,
		 fill.legend.hist=g$legend.hist,
		 fill.legend.hist.title=fill.legend.hist.title,
		 fill.legend.z=fill.legend.z,
		 fill.legend.hist.z=fill.legend.hist.z,
		 fill.id=g$id)
}
