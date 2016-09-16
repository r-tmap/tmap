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
	nxshape <- if (inherits(xshape, c("grob", "ggplot"))) 1 else length(xshape)
	
	varysize <- all(xsize %in% shpcols) && !is.null(xsize)
	varycol <- all(xcol %in% shpcols) && !is.null(xcol)
	varyshape <- is.vector(xshape) && all(xshape %in% shpcols) && !is.null(xshape)
	
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

	# symbol shapes: create a library with all the custom symbols (grobs), represented by symbol numbers 1000+
	shapeLib <- get(".shapeLib", envir = .TMAP_CACHE)
	if (!varyshape) {
		if (is.vector(xshape)) {
			if (!all(is.numeric(xshape))) stop("symbol shape(s) ('shape' argument) is/are neither numeric nor valid variable name(s)", call. = FALSE)
		} else if (is.grob(xshape)) {
			shapeLib <- c(shapeLib, list(xshape))
			xshape <- 999 + length(shapeLib)
		} else if (is.list(xshape)) {
			libs_ids <- 999 + length(shapeLib) + (1:length(xshape))
			shapeLib <- c(shapeLib, xshape)
			xshape <- libs_ids
		} else {
			stop("symbol shape(s) ('shape' argument) is/are neither symbol numers, nor grobs, nor valid variable name(s)", call. = FALSE)
		}
		for (i in 1:nx) data[[paste("SHAPE", i, sep="_")]] <- xshape[i]
		xshape <- paste("SHAPE", 1:nx, sep="_")
	}
	if (is.list(g$shapes)) {
		if (is.grob(g$shapes)) g$shapes <- list(g$shapes)
		
		shape_is_correct <- sapply(g$shapes, function(gshp) inherits(gshp, c("grob", "numeric", "integer")))
		if (!all(shape_is_correct)) stop("symbol shapes (shapes argument)", !which(shape_is_correct), "not correct" , call. = FALSE)

		shape_is_grob <- sapply(g$shapes, function(gshp) inherits(gshp, "grob"))
		
		lib_ids <- (999 + length(shapeLib)) + 1:sum(shape_is_grob)
		shapeLib <- c(shapeLib, g$shapes[shape_is_grob])
		g$shapes[shape_is_grob] <- lib_ids
		if (is.list(g$shapes)) g$shapes <- unlist(g$shapes)
		
	}
	assign(".shapeLib", shapeLib, envir = .TMAP_CACHE)

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
		symbol.max.size <- lapply(res, function(r)r$symbol.max.size)
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
	selCol <- if (is.list(dtsize)) {
		if (is.list(dtcol)) {
			lapply(dtsize, function(i)!is.na(i))	
		} else {
			!is.na(unlist(dtsize))
		}
	} else {
		if (is.list(dtcol)) {
			cnts <- vapply(dtcol, length, integer(1))
			cnts2 <- 1:length(dtcol)
			f <- factor(unlist(mapply(rep, cnts2, cnts, SIMPLIFY = FALSE)))
			split(!is.na(dtsize), f = f)
		} else {
			!is.na(dtsize)
		}
	}
	
	
	selShape <- if (is.list(dtsize)) {
		if (is.list(dtshape)) {
			lapply(dtsize, function(i)!is.na(i))	
		} else {
			!is.na(unlist(dtsize))
		}
	} else {
		if (is.list(dtshape)) {
			cnts <- vapply(dtshape, length, integer(1))
			cnts2 <- 1:length(dtshape)
			f <- factor(unlist(mapply(rep, cnts2, cnts, SIMPLIFY = FALSE)))
			split(!is.na(dtsize), f = f)
		} else {
			!is.na(dtsize)
		}
	}
	

	
	if (is.list(dtshape)) {
		sel2 <- if (is.na(selShape[1])) rep(TRUE, nx) else selShape
		
		# multiple variables for size are defined
		gsp <- split_g(g, n=nx)
		#if (!all(sapply(dtshape, is.numeric))) stop("size argument of tm_symbols/tm_dots contains a non-numeric variable", call. = FALSE)
		res <- mapply(process_symbols_shape_vector, dtshape, sel2, gsp, MoreArgs = list(map_shapes=varyshape, gt), SIMPLIFY = FALSE)
		symbol.shape <- sapply(res, function(r)r$symbol.shape)
		shape.legend.labels <- lapply(res, function(r)r$shape.legend.labels)
		shape.legend.shapes <- lapply(res, function(r)r$shape.legend.shapes)
		shape.neutral <- lapply(res, function(r)r$shape.neutral)
		if (!varyshape) xshape <- rep(NA, nx)
	} else {
		#if (!is.numeric(dtsize)) stop("size argument of tm_symbols/tm_dots is not a numeric variable", call. = FALSE)
		sel2 <- if (is.na(selShape[1])) TRUE else selShape
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
			shape.neutral <- symbol.shape[1]
		}
	}
	
	
	
	dcr <- process_dtcol(dtcol, selCol, g, gt, nx, npol)
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
	symbol.shape.legend.title <- if (is.ena(g$title.shape)[1]) xshape else g$title.shape
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
	
	if (!is.null(g$shapes.legend)) {
		shape.neutral <- g$shapes.legend
		col.neutral <- if (is.na(g$shapes.legend.fill)[1]) gt$aes.colors["symbols"] else  g$shapes.legend.fill
	}
	
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
		 symbol.col.legend.sizes=symbol.max.size,
		 symbol.col.legend.shapes=shape.neutral,
		 symbol.col.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, symbol.normal.size=g$legend.max.symbol.size),
		 symbol.size.legend.labels=symbol.size.legend.labels,
		 symbol.size.legend.palette= col.neutral,
		 symbol.size.legend.sizes=symbol.legend.sizes,
		 symbol.size.legend.shapes=shape.neutral,
		 symbol.size.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, symbol.normal.size=g$legend.max.symbol.size),
		 symbol.shape.legend.labels=shape.legend.labels,
		 symbol.shape.legend.palette=col.neutral,
		 symbol.shape.legend.sizes=symbol.max.size,
		 symbol.shape.legend.shapes=shape.legend.shapes,
		 symbol.shape.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, symbol.normal.size=g$legend.max.symbol.size), 
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
