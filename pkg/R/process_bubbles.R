process_symbols_shape_vector <- function(x, sel, g, map_shapes, gt) {
	if (map_shapes) {
		x2 <- as.factor(x)
		
		shape.legend.labels <- levels(x2)
		shape.legend.shapes <- rep(g$shapes, length.out=nlevels(x2))
		
		symbol.shape <- shape.legend.shapes[as.integer(x2)]
		shape.neutral <- shape.legend.shapes[1]
	} else {
		symbol.shape <- x
		shape.legend.labels <- NA
		shape.legend.shapes <- NA
		shape.neutral <- x[1]
	}
	
	list(symbol.shape=symbol.shape,
		 shape.legend.labels=shape.legend.labels,
		 shape.legend.shapes=shape.legend.shapes,
		 shape.neutral=shape.neutral)

}

process_symbols_size_vector <- function(x, g, rescale, gt) {
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
		symbol.size.legend.labels <- do.call("fancy_breaks", c(list(vec=x_legend, intervals=FALSE), g$legend.format))
	} else {
		if (length(g$sizes.legend.labels) != length(x_legend)) stop("length of sizes.legend.labels is not equal to the number of symbols in the legend", call. = FALSE)
		symbol.size.legend.labels <- g$sizes.legend.labels
	}
	
	maxX <- ifelse(rescale, max(x, na.rm=TRUE), 1)
	scaling <- ifelse(g$perceptual, 0.5716, 0.5)
	symbol.size <- g$scale*(x/maxX)^scaling
	symbol.max.size <- max(symbol.size, na.rm=TRUE)
	symbol.legend.sizes <- g$scale*(x_legend/maxX)^scaling
	list(symbol.size=symbol.size,
		 symbol.size.legend.labels=symbol.size.legend.labels,
		 symbol.legend.sizes=symbol.legend.sizes,
		 symbol.max.size=symbol.max.size)
}


process_symbols <- function(data, g, gt, gby, z, allow.small.mult) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]
	
	
	xsize <- g$size
	xcol <- g$col
	xshape <- g$shape
	
	if (!allow.small.mult) xsize <- xsize[1]
	if (!allow.small.mult) xcol <- xcol[1]
	if (!allow.small.mult) xshape <- xshape[1]
	
	
	if (is.na(xcol)[1]) xcol <- if (g$are.dots) gt$aes.colors["dots"] else gt$aes.colors["symbols"]
	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE
	
	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_symbols/tm_dots is not a numeric", call. = FALSE)
	
	if (is.null(xsize)) {
		return(list(symbol.size=NULL,
					xsize=NA,
					xcol=NA,
					xshape=NA,
					symbol.size.legend.title=NA,
					symbol.col.legend.title=NA,
					symbol.shape.legend.title))
	}
	
	# if by is specified, use first value only
	if (nlevels(by)>1) {
		xsize <- xsize[1]
		xcol <- xcol[1]
		xshape <- xshape[1]
	}
	nxsize <- length(xsize)
	nxcol <- length(xcol)
	nxshape <- length(xshape)
	
	varysize <- all(xsize %in% shpcols) && !is.null(xsize)
	varycol <- all(xcol %in% shpcols) && !is.null(xcol)
	varyshape <- all(xshape %in% shpcols) && !is.null(xshape)
	
	nx <- max(nxcol, nxsize, nxshape)
	if (nxcol<nx) xcol <- rep(xcol, length.out=nx)
	if (nxsize<nx) xsize <- rep(xsize, length.out=nx)
	if (nxshape<nx) xshape <- rep(xshape, length.out=nx)
	
	if (!varysize) {
		if (!all(is.numeric(xsize))) stop("symbol sizes are neither numeric nor valid variable name(s)", call. = FALSE)
		for (i in 1:nx) data[[paste("SIZE", i, sep="_")]] <- xsize[i]
		xsize <- paste("SIZE", 1:nx, sep="_")
		gby$free.scales.symbol.size <- FALSE
	}
	
	# check for direct color input
	is.colors <- all(valid_colors(xcol))
	if (!varycol) {
		if (!is.colors) stop("Invalid symbol colors", call. = FALSE)
		xcol <- do.call("process_color", c(list(col=col2hex(xcol), alpha=g$alpha), gt$pc))
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}
	
	if (!varyshape) {
		if (!all(is.numeric(xshape))) stop("symbol shapes are neither numeric nor valid variable name(s)", call. = FALSE)
		for (i in 1:nx) data[[paste("SHAPE", i, sep="_")]] <- xshape[i]
		xshape <- paste("SHAPE", 1:nx, sep="_")
	}
	
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.symbol.col, is.colors=is.colors)
	dtsize <- process_data(data[, xsize, drop=FALSE], by=by, free.scales=gby$free.scales.symbol.size, is.colors=FALSE)
	dtshape <- process_data(data[, xshape, drop=FALSE], by=by, free.scales=gby$free.scales.symbol.shape, is.colors=FALSE)
	
	if (nlevels(by)>1) if (is.na(g$showNA)) g$showNA <- attr(dtcol, "anyNA")
	
	
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		if (!all(sapply(dtsize, is.numeric))) stop("size argument of tm_symbols/tm_dots contains a non-numeric variable", call. = FALSE)
		res <- mapply(process_symbols_size_vector, dtsize, gss, MoreArgs = list(rescale=varysize, gt), SIMPLIFY = FALSE)
		symbol.size <- sapply(res, function(r)r$symbol.size)
		symbol.size.legend.labels <- lapply(res, function(r)r$symbol.size.legend.labels)
		symbol.legend.sizes <- lapply(res, function(r)r$symbol.legend.sizes)
		symbol.max.size <- sapply(res, function(r)r$symbol.max.size)
	} else {
		if (!is.numeric(dtsize)) stop("size argument of tm_symbols/tm_dots is not a numeric variable", call. = FALSE)
		res <- process_symbols_size_vector(dtsize, g, rescale=varysize, gt)
		symbol.size <- matrix(res$symbol.size, nrow=npol)
		if (varysize) {
			symbol.size.legend.labels <- res$symbol.size.legend.labels
			symbol.legend.sizes <- res$symbol.legend.sizes
			symbol.max.size <- res$symbol.max.size
		} else {
			symbol.size.legend.labels <- NA
			symbol.legend.sizes <- NA
			symbol.max.size <- res$symbol.max.size
			xsize <- rep(NA, nx)
			symbol.size.legend.title <- rep(NA, nx)
		}
	}
	
	
	
	
	# selection: which symbol sizes are NA?
	sel <- if (is.list(dtsize)) {
		lapply(dtsize, function(i)!is.na(i))
	} else {
		if (is.list(dtcol)) {
			cnts <- vapply(dtcol, length, integer(1))
			cnts2 <- 1:length(dtcol)
			f <- factor(unlist(mapply(rep, cnts2, cnts, SIMPLIFY = FALSE)))
			split(dtsize, f = f)
		} else if (is.list(dtshape)) {
			cnts <- vapply(dtshape, length, integer(1))
			cnts2 <- 1:length(dtshape)
			f <- factor(unlist(mapply(rep, cnts2, cnts, SIMPLIFY = FALSE)))
			split(dtsize, f = f)
		} else !is.na(dtsize)
	} 
	
	
	
	
	
	if (is.list(dtshape)) {
		sel2 <- if (is.na(sel[1])) rep(TRUE, nx) else sel
		
		# multiple variables for size are defined
		gsp <- split_g(g, n=nx)
		#if (!all(sapply(dtshape, is.numeric))) stop("size argument of tm_symbols/tm_dots contains a non-numeric variable", call. = FALSE)
		res <- mapply(process_symbols_shape_vector, dtshape, sel2, gsp, MoreArgs = list(map_shapes=varyshape, gt), SIMPLIFY = FALSE)
		symbol.shape <- sapply(res, function(r)r$symbol.shape)
		shape.legend.labels <- lapply(res, function(r)r$shape.legend.labels)
		shape.legend.shapes <- lapply(res, function(r)r$shape.legend.shapes)
		shape.neutral <- sapply(res, function(r)r$shape.neutral)
	} else {
		#if (!is.numeric(dtsize)) stop("size argument of tm_symbols/tm_dots is not a numeric variable", call. = FALSE)
		sel2 <- if (is.na(sel[1])) TRUE else sel
		res <- process_symbols_shape_vector(dtshape, sel2, g, map_shapes=varyshape, gt)
		symbol.shape <- matrix(res$symbol.shape, nrow=npol)
		if (varyshape) {
			shape.legend.labels <- res$shape.legend.labels
			shape.legend.shapes <- res$shape.legend.shapes
			shape.neutral <- res$shape.neutral
		} else {
			shape.legend.labels <- NA
			shape.legend.shapes <- NA
			xshape <- rep(NA, nx)
			symbol.shape.legend.title <- rep(NA, nx)
			shape.neutral <- g$shapes[1]
		}
	}
	
	
	
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
	
	
	symbol.size.legend.title <- if (is.ena(g$title.size)[1]) xsize else g$title.size
	symbol.col.legend.title <- if (is.ena(g$title.col)[1]) xcol else g$title.col
	symbol.shape.legend.title <- if (is.ena(g$title.shape)[1]) xcol else g$title.shape
	symbol.size.legend.z <- if (is.na(g$legend.size.z)) z else g$legend.size.z
	symbol.col.legend.z <- if (is.na(g$legend.col.z)) z+.33 else g$legend.col.z
	symbol.shape.legend.z <- if (is.na(g$legend.shape.z)) z+.80 else g$legend.shape.z
	symbol.legend.hist.z <- if (is.na(g$legend.hist.z)) z+.66 else g$legend.hist.z
	
	if (g$legend.hist && is.ena(g$legend.hist.title) && symbol.col.legend.z>symbol.legend.hist.z) {
		# histogram is drawn between title and legend enumeration
		symbol.col.legend.hist.title <- symbol.col.legend.title
		symbol.col.legend.title <- ""
	} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
		symbol.col.legend.hist.title <- g$legend.hist.title
	} else symbol.col.legend.hist.title <- ""
	
	symbol.border.col <- do.call("process_color", c(list(col=g$border.col, alpha=g$border.alpha), gt$pc))
	
	if (!g$legend.size.show) symbol.size.legend.title <- NA
	if (!g$legend.col.show) symbol.col.legend.title <- NA
	if (!g$legend.shape.show) symbol.shape.legend.title <- NA
	
	list(symbol.size=symbol.size,
		 symbol.col=col,
		 symbol.shape=symbol.shape,
		 symbol.border.lwd=g$border.lwd,
		 symbol.border.col=symbol.border.col,
		 symbol.scale=g$scale,
		 symbol.col.legend.labels=col.legend.labels,
		 symbol.col.legend.palette=col.legend.palette,
		 symbol.col.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, symbol.max.size=symbol.max.size, symbol.shapes=shape.neutral),
		 symbol.size.legend.labels=symbol.size.legend.labels,
		 symbol.size.legend.palette= col.neutral,
		 symbol.size.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, legend.sizes=symbol.legend.sizes, symbol.shapes=shape.neutral),
		 symbol.shape.legend.labels=shape.legend.labels,
		 symbol.shape.legend.palette=col.neutral,
		 symbol.shape.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, symbol.max.size=symbol.max.size, symbol.shapes=shape.legend.shapes), 
		 symbol.col.legend.hist.misc=list(values=values, breaks=breaks),
		 symbol.misc = list(symbol.are.dots=g$are.dots),
		 xsize=xsize,
		 xcol=xcol,
		 xshape=xshape,
		 symbol.xmod=xmod,
		 symbol.ymod=ymod,
		 symbol.size.legend.show=g$legend.size.show,
		 symbol.size.legend.title=symbol.size.legend.title,
		 symbol.size.legend.is.portrait=g$legend.size.is.portrait,
		 symbol.size.legend.z=symbol.size.legend.z,
		 symbol.shape.legend.show=g$legend.shape.show,
		 symbol.shape.legend.title=symbol.shape.legend.title,
		 symbol.shape.legend.is.portrait=g$legend.shape.is.portrait,
		 #symbol.shape.legend.hist=g$legend.hist,
		 #symbol.shape.legend.hist.title=symbol.shape.legend.hist.title,
		 symbol.shape.legend.z=symbol.shape.legend.z,
		 #symbol.shape.legend.hist.z=symbol.legend.hist.z,
		 symbol.col.legend.show=g$legend.col.show,
		 symbol.col.legend.title=symbol.col.legend.title,
		 symbol.col.legend.is.portrait=g$legend.col.is.portrait,
		 symbol.col.legend.hist=g$legend.hist,
		 symbol.col.legend.hist.title=symbol.col.legend.hist.title,
		 symbol.col.legend.z=symbol.col.legend.z,
		 symbol.col.legend.hist.z=symbol.legend.hist.z,
		 symbol.id=g$id)
}
