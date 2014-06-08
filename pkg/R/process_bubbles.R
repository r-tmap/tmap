process_bubbles_size_vector <- function(x, g, rescale) {
	x_legend <- pretty(x, 7)
	x_legend <- x_legend[x_legend!=0]
	nxl <- length(x_legend)
	if (nxl>4) x_legend <- x_legend[-c(nxl-3, nxl-1)]
	maxX <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	bubble.size <- g$bubble.scale*sqrt(x/maxX)
	bubble.max.size <- max(bubble.size, na.rm=TRUE)
	bubble.size.legend.labels <- format(x_legend, trim=TRUE)
	bubble.legend.sizes <- g$bubble.scale*sqrt(x_legend/maxX)
	list(bubble.size=bubble.size,
		 bubble.size.legend.labels=bubble.size.legend.labels,
		 bubble.legend.sizes=bubble.legend.sizes,
		 bubble.max.size=bubble.max.size)
}

process_bubbles_col_vector <- function(xc, xs, g, gt) {
	bubble.col.is.numeric <- is.numeric(xc)
	if (bubble.col.is.numeric) {
		palette <- if (is.null(g$palette))  "Blues" else g$palette
		colsLeg <- num2pal(xc, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   legend.digits=gt$legend.digits,
						   legend.NA.text=gt$legend.NA.text)
		bubble.col <- colsLeg[[1]]
	} else {
		palette <- if (is.null(g$palette))  "Dark2" else g$palette
		#remove unused levels in legend
		sel <- !is.na(xs)
		colsLeg <- cat2pal(xc[sel],
						   palette = palette,
						   colorNA = g$colorNA,
						   legend.NA.text=gt$legend.NA.text,
						   max_levels=g$max.categories)
		
		bubble.col <- rep(NA, length(sel))
		bubble.col[sel] <- colsLeg[[1]]
		
	}
	bubble.col.legend.labels <- colsLeg[[2]]
	bubble.col.legend.palette <- colsLeg[[3]]
	
	list(bubble.col=bubble.col,
		 bubble.col.legend.labels=bubble.col.legend.labels,
		 bubble.col.legend.palette=bubble.col.legend.palette,
		 bubble.col.is.numeric=bubble.col.is.numeric)
	
}

process_bubbles <- function(data, g, gt, gby) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]
	
	xsize <- g$bubble.size
	xcol <- g$bubble.col
	
	
	if (is.null(xsize)) {
		return(list(bubble.size=NULL,
					xsize=NA,
					xcol=NA))
	}
	
	# if by is specified, use first value only
	if (nlevels(by)>1) {
		xsize <- xsize[1]
		xcol <- xcol[1]
	}
	nxsize <- length(xsize)
	nxcol <- length(xcol)
	
	varysize <- all(xsize %in% shpcols) && !is.null(xsize)
	varycol <- all(xcol %in% shpcols) && !is.null(xcol)
	
	nx <- max(nxcol, nxsize)
	if (nxcol<nx) xcol <- rep(xcol, length.out=nx)
	if (nxsize<nx) xsize <- rep(xsize, length.out=nx)
	
	if (!varysize) {
		if (!all(is.numeric(xsize))) stop("Bubble sizes are neither numeric nor valid variable names")
		for (i in 1:nx) data[[paste("SIZE", i, sep="_")]] <- xsize[i]
		xsize <- paste("SIZE", 1:nx, sep="_")
		gby$free.scales.bubble.size <- FALSE
	}
	
	if (!varycol) {
		if (!all(valid_colors(xcol))) stop("Invalid bubble colors")
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.bubble.col)
	dtsize <- process_data(data[, xsize, drop=FALSE], by=by, free.scales=gby$free.scales.bubble.size)
	
	if (is.list(dtsize)) {
		res <- lapply(dtsize, process_bubbles_size_vector, g, rescale=varysize)
		bubble.size <- sapply(res, function(r)r$bubble.size)
		bubble.size.legend.labels <- lapply(res, function(r)r$bubble.size.legend.labels)
		bubble.legend.sizes <- lapply(res, function(r)r$bubble.legend.sizes)
		bubble.max.size <- sapply(res, function(r)r$bubble.max.size)
	} else {
		res <- process_bubbles_size_vector(dtsize, g, rescale=varysize)
		bubble.size <- matrix(res$bubble.size, nrow=npol)
		if (varysize) {
			bubble.size.legend.labels <- res$bubble.size.legend.labels
			bubble.legend.sizes <- res$bubble.legend.sizes
			bubble.max.size <- res$bubble.max.size
		} else {
			bubble.size.legend.labels <- NA
			bubble.legend.sizes <- NA
			bubble.max.size <- 1
			xsize <- rep(NA, nx)
		}
	}
	
	
	if (is.matrix(dtcol)) {
		bubble.col <- dtcol
		xcol <- rep(NA, nx)
		bubble.col.legend.labels <- NA
		bubble.col.legend.palette <- NA
		bubble.col.is.numeric <- NA
	} else if (is.list(dtcol)) {
		bubble.size_list <- as.list(as.data.frame(bubble.size))
		res <- mapply(process_bubbles_col_vector, dtcol, bubble.size_list, MoreArgs=list(g, gt), SIMPLIFY=FALSE)
		bubble.col <- sapply(res, function(r)r$bubble.col)
		bubble.col.legend.labels <- lapply(res, function(r)r$bubble.col.legend.labels)
		bubble.col.legend.palette <- lapply(res, function(r)r$bubble.col.legend.palette)
		bubble.col.is.numeric <- sapply(res, function(r)r$bubble.col.is.numeric)
	} else {
		bubble.size_vector <- unlist(bubble.size)
		res <- process_bubbles_col_vector(dtcol, bubble.size_vector, g, gt)
		bubble.col <- matrix(res$bubble.col, nrow=npol)
		bubble.col.legend.labels <- res$bubble.col.legend.labels
		bubble.col.legend.palette <- res$bubble.col.legend.palette
		bubble.col.is.numeric <- res$bubble.col.is.numeric
	}
		
		
	xmod <- g$bubble.xmod
	ymod <- g$bubble.ymod
	xmod <- if (is.character(xmod)) data[[xmod]] else rep(xmod, length.out=npol)
	ymod <-  if (is.character(ymod)) data[[ymod]] else rep(ymod, length.out=npol)

	bubble.size.legend.palette <- if (is.list(bubble.col.legend.palette)) {
		mapply(function(pal, isnum) {
			if (isnum) pal[length(pal)] else pal[1]
		}, bubble.col.legend.palette, bubble.col.is.numeric)
	} else if (is.na(bubble.col.legend.palette[1])) {
		apply(bubble.col, 2, function(bc) na.omit(bc)[1])
	} else {
		rep(bubble.col.legend.palette[1], nx)
	}
	
	list(bubble.size=bubble.size,
		 bubble.col=bubble.col,
		 bubble.border.lwd=g$bubble.border.lwd,
		 bubble.border.col=g$bubble.border.col,
		 bubble.scale=g$bubble.scale,
		 bubble.col.legend.labels=bubble.col.legend.labels,
		 bubble.col.legend.palette=bubble.col.legend.palette,
		 bubble.col.legend.misc=list(bubble.border.lwd=g$bubble.border.lwd, bubble.border.col=g$bubble.border.col, bubble.max.size=bubble.max.size),
		 bubble.size.legend.labels=bubble.size.legend.labels,
		 bubble.size.legend.palette= bubble.size.legend.palette,
		 bubble.size.legend.misc=list(bubble.border.lwd=g$bubble.border.lwd, bubble.border.col=g$bubble.border.col, legend.sizes=bubble.legend.sizes),
		 xsize=xsize,
		 xcol=xcol,
		 bubble.xmod=xmod,
		 bubble.ymod=ymod)
}
