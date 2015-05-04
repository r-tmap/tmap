process_fill_vector <- function(x, g, gt, tiny) {
	fill.values <- x
	textNA <- ifelse(any(is.na(fill.values)), g$textNA, NA)
	
	x[tiny] <- NA
	
	
	if (is.factor(x)) {
		palette <- if (is.null(g$palette)) ifelse(nlevels(x)>8, "Set3", "Dark2") else g$palette
		colsLeg <- cat2pal(x,
						   palette = palette,
						   colorNA = g$colorNA,
						   legend.NA.text = textNA,
						   max_levels=g$max.categories,
						   alpha=g$alpha)
		fill.breaks <- NA
	} else {
		palette <- if (is.null(g$palette)) "RdYlGn" else g$palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.scientific=gt$legend.scientific,
						   legend.digits=gt$legend.digits,
						   legend.NA.text = textNA,
						   alpha=g$alpha, 
						   text_separator = g$text_separator,
						   text_less_than = g$text_less_than,
						   text_or_more = g$text_or_more)
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


process_fill <- function(data, g, gb, gt, gby) {
	
	npol <- nrow(data)
	by <- data$GROUP_BY
	areas <- data$SHAPE_AREAS
	areas_prop <- areas/sum(areas, na.rm=TRUE)
	shpcols <- names(data)[1:(ncol(data)-2)]
	
	x <- g$col
	# if by is specified, use first value only
	if (nlevels(by)>1) x <- x[1]
	nx <- length(x)
	
	# check for direct color input
	is.colors <- all(valid_colors(x))
	if (is.colors) {
		x <- get_alpha_col(col2hex(x), g$alpha)
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
		x <- paste("COLOR", 1:nx, sep="_")
	} else {
		if (!all(x %in% shpcols)) stop("Fill argument neither colors nor valid variable names")
	}
	dt <- process_data(data[, x, drop=FALSE], by=by, free.scales=gby$free.scales.fill, is.colors=is.colors)
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	nx <- max(nx, nlevels(by))
		
	# return if data is matrix of color values
	if (is.matrix(dt)) {
		if (!is.colors) {
			dt <- matrix(get_alpha_col(dt, g$alpha), ncol=ncol(dt))
		}
		return(list(fill=dt, xfill=rep(NA, nx)))	
	} 
	tiny <- areas_prop < g$thres.poly
	if (all(tiny)) warning("all relative area sizes are below thres.poly")
	if (is.list(dt)) {
		isNum <- sapply(dt, is.numeric)
		if (any(isNum) && g$convert2density) {
			dt[isNum] <- lapply(dt[isNum], function(d) {
				d / areas
			})
		}
		res <- lapply(dt, process_fill_vector, g, gt, tiny)
		fill <- sapply(res, function(r)r$fill)
		fill.legend.labels <- lapply(res, function(r)r$fill.legend.labels)
		fill.legend.palette <- lapply(res, function(r)r$fill.legend.palette)
		fill.breaks <- lapply(res, function(r)r$fill.breaks)
		fill.values <- lapply(dt, function(d)d[!tiny])
	} else {
		if (is.numeric(dt) && g$convert2density) {
			dt <- dt / areas
		}
		res <- process_fill_vector(dt, g, gt, tiny)
		fill <- matrix(res$fill, nrow=npol)
		fill.legend.labels <- res$fill.legend.labels
		fill.legend.palette <- res$fill.legend.palette
		fill.breaks <- res$fill.breaks
		fill.values <- lapply(split(dt, rep(1:nx, each=npol)), function(d)d[!tiny])
	}
	list(fill=fill,
		 fill.legend.labels=fill.legend.labels,
		 fill.legend.palette=fill.legend.palette,
		 fill.legend.misc=list(values=fill.values, breaks=fill.breaks, lwd=gb$lwd, border.col=gb$col, border.alpha=gb$alpha),
		 xfill=x)
}
