process_bubbles_size_vector <- function(x, g, rescale, gt) {
	if (!is.na(g$size.lim[1])) {
		x[x<g$size.lim[1]] <- NA
		x[x>g$size.lim[2]] <- g$size.lim[2]
	}
	
	if (is.null(g$sizes.legend)) {
		x_legend <- pretty(x, 5)
		x_legend <- x_legend[x_legend!=0]
		nxl <- length(x_legend)
		if (nxl>5) x_legend <- x_legend[-c(nxl-3, nxl-1)]
	} else {
		x_legend <- g$sizes.legend
	}

	if (is.null(g$sizes.legend.labels)) {
		bubble.size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of bubbles in the legend", call. = FALSE)
		bubble.size.legend.labels <- g$sizes.legend.labels
	}
	
	maxX <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	scaling <- ifelse(g$perceptual, 0.5716, 0.5)
	bubble.size <- g$scale*(x/maxX)^scaling
	bubble.max.size <- max(bubble.size, na.rm=TRUE)
	bubble.legend.sizes <- g$scale*(x_legend/maxX)^scaling
	list(bubble.size=bubble.size,
		 bubble.size.legend.labels=bubble.size.legend.labels,
		 bubble.legend.sizes=bubble.legend.sizes,
		 bubble.max.size=bubble.max.size)
}


process_bubbles <- function(data, g, gt, gby, z, allow.small.mult) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]
	
	# update legend format from tm_layout
	to_be_assigned <- setdiff(names(gt$legend.format), names(g$legend.format))
	g$legend.format[to_be_assigned] <- gt$legend.format[to_be_assigned]
	
	xsize <- g$size
	xcol <- g$col
	
	if (!allow.small.mult) xsize <- xsize[1]
	if (!allow.small.mult) xcol <- xcol[1]
	
	
	if (is.na(xcol)[1]) xcol <- if (g$are.dots) gt$aes.colors["dots"] else gt$aes.colors["bubbles"]
	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE
	
	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_bubbles/tm_dots is not a numeric", call. = FALSE)
	
	if (is.null(xsize)) {
		return(list(bubble.size=NULL,
					xsize=NA,
					xcol=NA,
					bubble.size.legend.title=NA,
					bubble.col.legend.title=NA))
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
		if (!all(is.numeric(xsize))) stop("Bubble sizes are neither numeric nor valid variable name(s)", call. = FALSE)
		for (i in 1:nx) data[[paste("SIZE", i, sep="_")]] <- xsize[i]
		xsize <- paste("SIZE", 1:nx, sep="_")
		gby$free.scales.bubble.size <- FALSE
	}
	
	# check for direct color input
	is.colors <- all(valid_colors(xcol))
	if (!varycol) {
		if (!is.colors) stop("Invalid bubble colors", call. = FALSE)
		xcol <- do.call("process_color", c(list(col=col2hex(xcol), alpha=g$alpha), gt$pc))
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.bubble.col, is.colors=is.colors)
	dtsize <- process_data(data[, xsize, drop=FALSE], by=by, free.scales=gby$free.scales.bubble.size, is.colors=FALSE)
	
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		if (!all(sapply(dtsize, is.numeric))) stop("size argument of tm_bubbles/tm_dots contains a non-numeric variable", call. = FALSE)
		res <- mapply(process_bubbles_size_vector, dtsize, gss, MoreArgs = list(rescale=varysize, gt), SIMPLIFY = FALSE)
		bubble.size <- sapply(res, function(r)r$bubble.size)
		bubble.size.legend.labels <- lapply(res, function(r)r$bubble.size.legend.labels)
		bubble.legend.sizes <- lapply(res, function(r)r$bubble.legend.sizes)
		bubble.max.size <- sapply(res, function(r)r$bubble.max.size)
	} else {
		if (!is.numeric(dtsize)) stop("size argument of tm_bubbles/tm_dots is not a numeric variable", call. = FALSE)
		res <- process_bubbles_size_vector(dtsize, g, rescale=varysize, gt)
		bubble.size <- matrix(res$bubble.size, nrow=npol)
		if (varysize) {
			bubble.size.legend.labels <- res$bubble.size.legend.labels
			bubble.legend.sizes <- res$bubble.legend.sizes
			bubble.max.size <- res$bubble.max.size
		} else {
			bubble.size.legend.labels <- NA
			bubble.legend.sizes <- NA
			bubble.max.size <- res$bubble.max.size
			xsize <- rep(NA, nx)
			bubble.size.legend.title <- rep(NA, nx)
		}
	}
	
	sel <- if (is.list(dtsize)) {
		lapply(dtsize, function(i)!is.na(i))
	} else !is.na(dtsize)
	
	dcr <- process_dtcol(dtcol, sel, g, gt, nx, npol)
	if (dcr$is.constant) xcol <- rep(NA, nx)
	col <- dcr$col
	col.legend.labels <- dcr$legend.labels
	col.legend.palette <- dcr$legend.palette
	col.neutral <- dcr$col.neutral
	breaks <- dcr$breaks
	values <- dcr$values
	
	xmod <- g$xmod
	ymod <- g$ymod
	xmod <- if (is.character(xmod)) data[[xmod]] else rep(xmod, length.out=npol)
	ymod <-  if (is.character(ymod)) data[[ymod]] else rep(ymod, length.out=npol)

	if (g$jitter>0) {
		xmod <- xmod + rnorm(n=npol, sd=g$jitter)
		ymod <- ymod + rnorm(n=npol, sd=g$jitter)
	}
	
	
	bubble.size.legend.title <- if (is.na(g$title.size)[1]) xsize else g$title.size
	bubble.col.legend.title <- if (is.na(g$title.col)[1]) xcol else g$title.col
	bubble.size.legend.z <- if (is.na(g$legend.size.z)) z else g$legend.size.z
	bubble.col.legend.z <- if (is.na(g$legend.col.z)) z+.33 else g$legend.col.z
	bubble.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.66 else g$legend.hist.z
	
	if (g$legend.hist && is.na(g$legend.hist.title) && bubble.col.legend.z>bubble.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		bubble.col.legend.hist.title <- bubble.col.legend.title
		bubble.col.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		bubble.col.legend.hist.title <- g$legend.hist.title
	} else bubble.col.legend.hist.title <- ""
	
	bubble.border.col <- do.call("process_color", c(list(col=g$border.col, alpha=g$border.alpha), gt$pc))
	
	if (!g$legend.size.show) bubble.size.legend.title <- NA
	if (!g$legend.col.show) bubble.col.legend.title <- NA

	list(bubble.size=bubble.size,
		 bubble.col=col,
		 bubble.border.lwd=g$border.lwd,
		 bubble.border.col=bubble.border.col,
		 bubble.scale=g$scale,
		 bubble.col.legend.labels=col.legend.labels,
		 bubble.col.legend.palette=col.legend.palette,
		 bubble.col.legend.misc=list(bubble.border.lwd=g$border.lwd, bubble.border.col=bubble.border.col, bubble.max.size=bubble.max.size),
		 bubble.size.legend.labels=bubble.size.legend.labels,
		 bubble.size.legend.palette= col.neutral,
		 bubble.size.legend.misc=list(bubble.border.lwd=g$border.lwd, bubble.border.col=bubble.border.col, legend.sizes=bubble.legend.sizes),
		 bubble.col.legend.hist.misc=list(values=values, breaks=breaks),
		 bubble.misc = list(bubble.are.dots=g$are.dots),
		 xsize=xsize,
		 xcol=xcol,
		 bubble.xmod=xmod,
		 bubble.ymod=ymod,
		 bubble.size.legend.show=g$legend.size.show,
		 bubble.col.legend.show=g$legend.col.show,
		 bubble.size.legend.title=bubble.size.legend.title,
		 bubble.col.legend.title=bubble.col.legend.title,
		 bubble.size.legend.is.portrait=g$legend.size.is.portrait,
		 bubble.col.legend.is.portrait=g$legend.col.is.portrait,
		 bubble.col.legend.hist=g$legend.hist,
		 bubble.col.legend.hist.title=bubble.col.legend.hist.title,
		 bubble.size.legend.z=bubble.size.legend.z,
		 bubble.col.legend.z=bubble.col.legend.z,
		 bubble.col.legend.hist.z=bubble.legend.hist.z,
		 bubble.id=g$id)
}
