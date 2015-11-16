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
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of bubbles in the legend")
		bubble.size.legend.labels <- g$sizes.legend.labels
	}
	
	maxX <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	scaling <- ifelse(g$perceptual, 0.5716, 0.5)
	bubble.size <- g$bubble.scale*(x/maxX)^scaling
	bubble.max.size <- max(bubble.size, na.rm=TRUE)
	bubble.legend.sizes <- g$bubble.scale*(x_legend/maxX)^scaling
	list(bubble.size=bubble.size,
		 bubble.size.legend.labels=bubble.size.legend.labels,
		 bubble.legend.sizes=bubble.legend.sizes,
		 bubble.max.size=bubble.max.size)
}

process_bubbles_col_vector <- function(xc, xs, g, gt) {
	bubble.col.is.numeric <- is.numeric(xc)
	if (bubble.col.is.numeric) {
		
		is.diverging <-  use_diverging_palette(xc, g$breaks)
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.diverging, "div", "seq")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		colsLeg <- num2pal(xc, g$n, style=g$style, breaks=g$breaks, 
						   palette = palette,
						   auto.palette.mapping = g$auto.palette.mapping,
						   contrast = g$contrast, legend.labels=g$labels,
						   colorNA=g$colorNA, 
						   legend.NA.text=g$textNA,
						   process.colors=c(list(alpha=g$bubble.alpha), gt$pc),
						   legend.format=g$legend.format)
		bubble.col <- colsLeg[[1]]
		bubble.col.neutral <- colsLeg$legend.neutral.col
		bubble.breaks <- colsLeg[[4]]
	} else {
		palette <- if (is.null(g$palette)) {
			gt$aes.palette[[ifelse(is.ordered(xc), "seq", "cat")]] 
		} else if (g$palette[1] %in% c("seq", "div", "cat")) {
			gt$aes.palette[[g$palette[1]]]
		} else g$palette
		#remove unused levels in legend
		sel <- !is.na(xs)
		colsLeg <- cat2pal(xc[sel],
						   palette = palette,
						   contrast = g$contrast,
						   colorNA = g$colorNA,
						   legend.labels=g$labels,
						   legend.NA.text=g$textNA,
						   max_levels=g$max.categories,
						   process.colors=c(list(alpha=g$bubble.alpha), gt$pc))
		
		bubble.col <- rep(NA, length(sel))
		bubble.col[sel] <- colsLeg[[1]]
		bubble.col.neutral <- bubble.col[sel[1]]
		bubble.breaks <- NA
	}
	bubble.col.legend.labels <- colsLeg[[2]]
	bubble.col.legend.palette <- colsLeg[[3]]
	
	list(bubble.col=bubble.col,
		 bubble.col.legend.labels=bubble.col.legend.labels,
		 bubble.col.legend.palette=bubble.col.legend.palette,
		 bubble.col.is.numeric=bubble.col.is.numeric,
		 bubble.col.neutral=bubble.col.neutral,
		 bubble.breaks=bubble.breaks)
}

process_bubbles <- function(data, g, gt, gby, z) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]
	
	# update legend format from tm_layout
	to_be_assigned <- setdiff(names(gt$legend.format), names(g$legend.format))
	g$legend.format[to_be_assigned] <- gt$legend.format[to_be_assigned]
	
	xsize <- g$bubble.size
	xcol <- g$bubble.col
	
	if (is.na(xcol)[1]) xcol <- if (g$are.dots) gt$aes.colors["dots"] else gt$aes.colors["bubbles"]
	
	
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
		if (!all(is.numeric(xsize))) stop("Bubble sizes are neither numeric nor valid variable names")
		for (i in 1:nx) data[[paste("SIZE", i, sep="_")]] <- xsize[i]
		xsize <- paste("SIZE", 1:nx, sep="_")
		gby$free.scales.bubble.size <- FALSE
	}
	
	# check for direct color input
	is.colors <- all(valid_colors(xcol))
	if (!varycol) {
		if (!is.colors) stop("Invalid bubble colors")
		xcol <- do.call("process_color", c(list(col=col2hex(xcol), alpha=g$bubble.alpha), gt$pc))
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.bubble.col, is.colors=is.colors)
	dtsize <- process_data(data[, xsize, drop=FALSE], by=by, free.scales=gby$free.scales.bubble.size, is.colors=FALSE)
	
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		res <- mapply(process_bubbles_size_vector, dtsize, gss, MoreArgs = list(rescale=varysize, gt), SIMPLIFY = FALSE)
		bubble.size <- sapply(res, function(r)r$bubble.size)
		bubble.size.legend.labels <- lapply(res, function(r)r$bubble.size.legend.labels)
		bubble.legend.sizes <- lapply(res, function(r)r$bubble.legend.sizes)
		bubble.max.size <- sapply(res, function(r)r$bubble.max.size)
	} else {
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
	
	
	if (is.matrix(dtcol)) {
		bubble.col <- if (!is.colors) {
			matrix(do.call("process_color", c(list(col=dtcol, alpha=g$bubble.alpha), gt$pc)),
				   ncol=ncol(dtcol))
		} else dtcol
		xcol <- rep(NA, nx)
		bubble.col.legend.title <- rep(NA, nx)
		bubble.col.legend.labels <- NA
		bubble.col.legend.palette <- NA
		bubble.col.is.numeric <- NA
		bubble.col.neutral <- apply(bubble.col, 2, function(bc) na.omit(bc)[1])
		bubble.breaks <- NA
		bubble.values <- NA
	} else if (is.list(dtcol)) {
		# multiple variables for col are defined
		gsc <- split_g(g, n=nx)
		bubble.size_list <- as.list(as.data.frame(bubble.size))
		res <- mapply(process_bubbles_col_vector, dtcol, bubble.size_list, gsc, MoreArgs=list(gt), SIMPLIFY=FALSE)
		bubble.col <- sapply(res, function(r)r$bubble.col)
		bubble.col.legend.labels <- lapply(res, function(r)r$bubble.col.legend.labels)
		bubble.col.legend.palette <- lapply(res, function(r)r$bubble.col.legend.palette)
		bubble.col.is.numeric <- sapply(res, function(r)r$bubble.col.is.numeric)
		bubble.col.neutral <- sapply(res, function(r)r$bubble.col.neutral)
		bubble.breaks <- lapply(res, function(r)r$bubble.breaks)
		bubble.values <- dtcol
	} else {
		bubble.size_vector <- unlist(bubble.size)
		res <- process_bubbles_col_vector(dtcol, bubble.size_vector, g, gt)
		bubble.col <- matrix(res$bubble.col, nrow=npol)
		bubble.col.legend.labels <- res$bubble.col.legend.labels
		bubble.col.legend.palette <- res$bubble.col.legend.palette
		bubble.col.is.numeric <- res$bubble.col.is.numeric
		bubble.col.neutral <- res$bubble.col.neutral
		bubble.breaks <- res$bubble.breaks
		bubble.values <- split(dtcol, rep(1:nx, each=npol))
	}
		
		
	xmod <- g$bubble.xmod
	ymod <- g$bubble.ymod
	xmod <- if (is.character(xmod)) data[[xmod]] else rep(xmod, length.out=npol)
	ymod <-  if (is.character(ymod)) data[[ymod]] else rep(ymod, length.out=npol)

	if (g$bubble.jitter>0) {
		xmod <- xmod + rnorm(n=npol, sd=g$bubble.jitter)
		ymod <- ymod + rnorm(n=npol, sd=g$bubble.jitter)
	}
	
	
	bubble.size.legend.palette <- bubble.col.neutral

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
	
	bubble.border.col <- do.call("process_color", c(list(col=g$bubble.border.col, alpha=g$bubble.border.alpha)))
	
	list(bubble.size=bubble.size,
		 bubble.col=bubble.col,
		 bubble.border.lwd=g$bubble.border.lwd,
		 bubble.border.col=bubble.border.col,
		 bubble.scale=g$bubble.scale,
		 bubble.col.legend.labels=bubble.col.legend.labels,
		 bubble.col.legend.palette=bubble.col.legend.palette,
		 bubble.col.legend.misc=list(bubble.border.lwd=g$bubble.border.lwd, bubble.border.col=bubble.border.col, bubble.max.size=bubble.max.size),
		 bubble.size.legend.labels=bubble.size.legend.labels,
		 bubble.size.legend.palette= bubble.size.legend.palette,
		 bubble.size.legend.misc=list(bubble.border.lwd=g$bubble.border.lwd, bubble.border.col=bubble.border.col, legend.sizes=bubble.legend.sizes),
		 bubble.col.legend.hist.misc=list(values=bubble.values, breaks=bubble.breaks),
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
		 bubble.id=g$bubble.id)
}
