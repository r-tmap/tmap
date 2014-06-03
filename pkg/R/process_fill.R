process_fill_vector <- function(x, g, gt, tiny) {
	choro.values <- x
	
	x[tiny] <- NA
	
	if (is.factor(x)) {
		palette <- if (is.null(g$palette)) ifelse(nlevels(x)>8, "Set3", "Dark2") else g$palette
		colsLeg <- cat2pal(x,
						   palette = palette,
						   colorNA = g$colorNA,
						   legend.NA.text = gt$legend.NA.text,
						   max_levels=g$max.categories)
		fill.breaks <- NA
	} else {
		palette <- if (is.null(palette)) "RdYlGn" else palette
		colsLeg <- num2pal(x, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.digits=gt$legend.digits,
						   legend.NA.text = gt$legend.NA.text)
		fill.breaks <- colsLeg[[4]]
	}
	fill <- colsLeg[[1]]
	fill.legend.labels <- colsLeg[[2]]
	fill.legend.palette <- colsLeg[[3]]
	
	## adjust hisogram
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


process_fill <- function(data, g, gt, gby) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	areas <- data$SHAPE_AREAS
	
	x <- g$col
	# if by is specified, use first value only
	if (nlevels(by)>1) x <- x[1]
	nx <- length(x)
	
	# check for direct color input
	if (all(valid_colors(x))) {
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
		x <- paste("COLOR", 1:nx, sep="_")
	}
	
	dt <- process_data(data[, x, drop=FALSE], by=by, free.scales=gby$free.scales.fill)
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	# return if data is matrix of color values
	if (is.matrix(dt)) return(list(fill=dt, xfill=rep(NA, nx)))
	
	tiny <- areas < g$thres.poly

	fill.values <- dt
	if (is.list(dt)) {
		isNum <- sapply(dt, is.numeric)
		
		if (any(isNum) && convert2density) {
			dt[isNum] <- lapply(data[isNum], function(d) {
				d / (areas * g$total.area.km2)
			})
		}
		res <- lapply(dt, process_fill_vector, g, gt, tiny)
		fill <- sapply(res, function(r)r$fill)
		fill.legend.labels <- lapply(res, function(r)r$fill.legend.labels)
		fill.legend.palette <- lapply(res, function(r)r$fill.legend.palette)
		fill.legend.breaks <- lapply(res, function(r)r$fill.legend.breaks)
	} else {
		if (is.numeric(dt) && convert2density) {
			dt <- dt / (areas * g$total.area.km2)
		}
		res <- process_fill_vector(dt, g, gt, tiny)
		fill <- matrix(res$fill, nrow=npol)
		fill.legend.labels <- res$fill.legend.breaks
		fill.legend.palette <- res$fill.legend.palette
		fill.legend.breaks <- res$fill.legend.breaks
		fill.values <- split(fill.values, rep(1:nx, each=npol))
	}
	
	list(fill=fill,
		 fill.legend.labels=fill.legend.labels,
		 fill.legend.palette=fill.legend.palette,
		 fill.legend.misc=list(values=fill.values, breaks=fill.legend.breaks),
		 xfill=x)
}
