process_symbols <- function(data, g, gt, gby, z, interactive) {
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-1)]
	
	
	xsize <- g$size
	xcol <- g$col
	xshape <- g$shape
	
	if (is.list(xshape) && "iconUrl" %in% names(xshape)) xshape <- split_icon(xshape)
	if (is.grob(xshape)) xshape <- list(xshape)
	
	
	# if (interactive) {
	# 	if (length(xsize)>1) warning("Facets are not supported in view mode yet. Only symbol size aesthetic value \"", xsize[1], "\" will be shown.", call.=FALSE)
	# 	if (length(xcol)>1) warning("Facets are not supported in view mode yet. Only symbol color aesthetic value \"", xcol[1], "\" will be shown.", call.=FALSE)
	# 	xsize <- xsize[1]
	# 	xcol <- xcol[1]
	# 	xshape <- xshape[1]
	# }

	
	if (length(xcol)==1 && is.na(xcol)[1]) xcol <- if (g$are.dots) gt$aes.colors["dots"] else gt$aes.colors["symbols"]
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
					symbol.shape.legend.title=NA,
					symbol.id=g$id,
					symbol.popup.vars=g$popup.vars,
					symbol.popup.format=g$popup.format))
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
	
	if (varycol) {
		is.colors <- FALSE
	} else {
		# check for direct color input
		is.colors <- all(valid_colors(xcol))
		if (!is.colors) stop("Invalid symbol colors", call. = FALSE)
		xcol <- do.call("process_color", c(list(col=col2hex(xcol), alpha=g$alpha), gt$pc))
		for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- xcol[i]
		xcol <- paste("COLOR", 1:nx, sep="_")
	}

	# symbol shapes: create a library with all the custom symbols (grobs) or icons, represented by symbol numbers 1000+
	just <- g$just
	
	if (any(is.na(just))) {
		just <- c(.5, .5)
		just.override <- FALSE
	} else {
		just <- c(ifelse(is_num_string(just[1]), as.numeric(just[1]), ifelse(just[1]=="left", 1, ifelse(just[1]=="right", 0, .5))),
				  ifelse(is_num_string(just[2]), as.numeric(just[2]), ifelse(just[2]=="bottom", 1, ifelse(just[2]=="top", 0, .5))))
		just.override <- TRUE
	}
	
	
	
	#justx <- size.npc.w * ( - .5)
	#justy <- size.npc.h * ( - .5)
	
	if (!varyshape) {
		if (!is.list(xshape)) {
			if (!all(is.numeric(xshape))) stop("symbol shape(s) ('shape' argument) is/are neither numeric nor valid variable name(s)", call. = FALSE)
		} else if (is.list(xshape)) {
			xshape <- submit_symbol_shapes(xshape, interactive=interactive, just=just, just.override=just.override, grob.dim=g$grob.dim)
		} else {
			stop("symbol shape(s) ('shape' argument) is/are neither symbol numers, nor grobs, nor valid variable name(s)", call. = FALSE)
		}
		for (i in 1:nx) data[[paste("SHAPE", i, sep="_")]] <- xshape[i]
		xshape <- paste("SHAPE", 1:nx, sep="_")
	}
	if (is.list(g$shapes)) {
		if (inherits(g$shapes, "grob") || inherits(g$shapes[[1]], "grob") || (("iconUrl" %in% names(g$shapes)))) {
			# one grob, list of grobs or icon(s)			
			if ("iconUrl" %in% names(g$shapes)) g$shapes <- split_icon(g$shapes)
			g$shapes <- submit_symbol_shapes(g$shapes, interactive=interactive, just=just, just.override=just.override, grob.dim=g$grob.dim)			} else {
			# list of list of grobs or icons
			g$shapes <- lapply(g$shapes, function(gshape) {
				if ("iconUrl" %in% names(gshape)) gshape <- split_icon(gshape)
				submit_symbol_shapes(gshape, interactive=interactive, just=just, just.override=just.override, grob.dim=g$grob.dim)	
			})
		}
	} 
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	g$popup.format <- process_popup_format(g$popup.format, gt$legend.format, g$popup.vars)
	
	dtcol <- process_data(data[, xcol, drop=FALSE], by=by, free.scales=gby$free.scales.symbol.col, is.colors=is.colors)
	dtsize <- process_data(data[, xsize, drop=FALSE], by=by, free.scales=gby$free.scales.symbol.size, is.colors=FALSE)
	dtshape <- process_data(data[, xshape, drop=FALSE], by=by, free.scales=gby$free.scales.symbol.shape, is.colors=FALSE)
	
	if (nlevels(by)>1) if (is.na(g$showNA)) g$showNA <- attr(dtcol, "anyNA")
	
	
	if (is.list(dtsize)) {
		# multiple variables for size are defined
		gss <- split_g(g, n=nx)
		if (!all(sapply(dtsize, is.numeric))) stop("size argument of tm_symbols/tm_dots contains a non-numeric variable", call. = FALSE)
		res <- mapply(process_symbols_size_vector, dtsize, gss, MoreArgs = list(rescale=varysize, gt=gt, reverse=g$legend.size.reverse), SIMPLIFY = FALSE)
		symbol.size <- sapply(res, function(r)r$symbol.size)
		symbol.size.legend.labels <- lapply(res, function(r)r$symbol.size.legend.labels)
		symbol.size.legend.values <- lapply(res, function(r)r$symbol.size.legend.values)
		symbol.legend.sizes <- lapply(res, function(r)r$symbol.legend.sizes)
		symbol.max.size <- lapply(res, function(r)r$symbol.max.size)
	} else {
		if (!is.numeric(dtsize)) stop("size argument of tm_symbols/tm_dots is not a numeric variable", call. = FALSE)
		res <- process_symbols_size_vector(dtsize, g, rescale=varysize, gt=gt, reverse=g$legend.size.reverse)
		symbol.size <- matrix(res$symbol.size, nrow=npol)
		if (varysize) {
			symbol.size.legend.labels <- res$symbol.size.legend.labels
			symbol.size.legend.values <- res$symbol.size.legend.values
			symbol.legend.sizes <- res$symbol.legend.sizes
			symbol.max.size <- res$symbol.max.size
		} else {
			symbol.size.legend.labels <- NA
			symbol.size.legend.values <- NA
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
		res <- mapply(process_symbols_shape_vector, dtshape, sel2, gsp, MoreArgs = list(map_shapes=varyshape, gt=gt, reverse=g$legend.shape.reverse), SIMPLIFY = FALSE)
		symbol.shape <- sapply(res, function(r)r$symbol.shape)
		shape.legend.labels <- lapply(res, function(r)r$shape.legend.labels)
		shape.legend.values <- lapply(res, function(r)r$shape.legend.values)
		shape.legend.shapes <- lapply(res, function(r)r$shape.legend.shapes)
		shape.neutral <- lapply(res, function(r)r$shape.neutral)
		if (!varyshape) xshape <- rep(NA, nx)
	} else {
		#if (!is.numeric(dtsize)) stop("size argument of tm_symbols/tm_dots is not a numeric variable", call. = FALSE)
		sel2 <- if (is.na(selShape[1])) TRUE else selShape
		res <- process_symbols_shape_vector(dtshape, sel2, g, map_shapes=varyshape, gt=gt, reverse=g$legend.shape.reverse)
		symbol.shape <- matrix(res$symbol.shape, nrow=npol)
		if (varyshape) {
			shape.legend.labels <- res$shape.legend.labels
			shape.legend.values <- res$shape.legend.values
			shape.legend.shapes <- res$shape.legend.shapes
			shape.neutral <- res$shape.neutral
		} else {
			shape.legend.labels <- NA
			shape.legend.values <- NA
			shape.legend.shapes <- NA
			xshape <- rep(NA, nx)
			symbol.shape.legend.title <- rep(NA, nx)
			shape.neutral <- symbol.shape[which(!is.na(symbol.shape))[1]]
		}
	}
	
	
	dcr <- process_dtcol(dtcol, selCol, g, gt, nx, npol, reverse=g$legend.col.reverse)
	if (dcr$is.constant) xcol <- rep(NA, nx)
	col <- dcr$col
	col.legend.labels <- dcr$legend.labels
	col.legend.values <- dcr$legend.values
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
	
	xmod <- matrix(xmod, nrow=npol, ncol=nx)
	ymod <- matrix(ymod, nrow=npol, ncol=nx)
	
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
	
	if (is.null(g$border.col)) {
		symbol.border.col <- NA
		g$border.lwd <- NA
	} else {
		symbol.border.col <- g$border.col
		if (is.na(symbol.border.col)) {
			symbol.border.col <- gt$aes.colors["borders"]
		}
		symbol.border.col <- do.call("process_color", c(list(col=symbol.border.col, alpha=g$border.alpha), gt$pc))
	}
	
	
	if (!is.null(g$shapes.legend)) {
		shape.neutral <- g$shapes.legend
		col.neutral <- if (is.na(g$shapes.legend.fill)[1]) gt$aes.colors["symbols"] else  g$shapes.legend.fill
	}
	
	if (!g$legend.size.show) symbol.size.legend.title <- NA
	if (!g$legend.col.show) symbol.col.legend.title <- NA
	if (!g$legend.shape.show) symbol.shape.legend.title <- NA
	
	are.icons <- any(!is.na(symbol.shape) & symbol.shape>999)
	
	if (are.icons && !interactive) {
		scale <- g$scale * g$icon.scale
		symbol.size <- symbol.size * g$icon.scale
		symbol.legend.sizes <- symbol.legend.sizes * g$icon.scale
		g$legend.max.symbol.size <- g$legend.max.symbol.size * g$icon.scale
		
	} else scale <- g$scale

	clustering <- g$clustering
	if (identical(clustering, FALSE)) {
		clustering <- NULL
	} else if (identical(clustering, TRUE)) {
		clustering <- leaflet::markerClusterOptions()	
	}
	
	
	list(symbol.size=symbol.size,
		 symbol.col=col,
		 symbol.shape=symbol.shape,
		 symbol.border.lwd=g$border.lwd,
		 symbol.border.col=symbol.border.col,
		 #symbol.scale=scale, # not needed anymore?
		 symbol.col.legend.labels=col.legend.labels,
		 symbol.col.legend.values=col.legend.values,
		 symbol.col.legend.palette=col.legend.palette,
		 symbol.col.legend.sizes=symbol.max.size,
		 symbol.col.legend.shapes=shape.neutral,
		 symbol.col.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, symbol.normal.size=g$legend.max.symbol.size),
		 symbol.size.legend.labels=symbol.size.legend.labels,
		 symbol.size.legend.values=symbol.size.legend.values,
		 symbol.size.legend.palette= col.neutral,
		 symbol.size.legend.sizes=symbol.legend.sizes,
		 symbol.size.legend.shapes=shape.neutral,
		 symbol.size.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, symbol.normal.size=g$legend.max.symbol.size),
		 symbol.shape.legend.labels=shape.legend.labels,
		 symbol.shape.legend.values=shape.legend.values,
		 symbol.shape.legend.palette=col.neutral,
		 symbol.shape.legend.sizes=symbol.max.size,
		 symbol.shape.legend.shapes=shape.legend.shapes,
		 symbol.shape.legend.misc=list(symbol.border.lwd=g$border.lwd, symbol.border.col=symbol.border.col, symbol.normal.size=g$legend.max.symbol.size), 
		 symbol.col.legend.hist.misc=list(values=values, breaks=breaks),
		 symbol.misc = list(symbol.are.dots=g$are.dots, symbol.are.markers=g$are.markers, symbol.are.icons=are.icons, just=just, clustering=clustering),
		 xsize=xsize,
		 xcol=xcol,
		 xshape=xshape,
		 symbol.xmod=xmod,
		 symbol.ymod=ymod,
		 symbol.size.legend.show=g$legend.size.show,
		 symbol.size.legend.title=symbol.size.legend.title,
		 symbol.size.legend.is.portrait=g$legend.size.is.portrait,
		 symbol.size.legend.reverse=g$legend.size.reverse,
		 symbol.size.legend.z=symbol.size.legend.z,
		 symbol.shape.legend.show=g$legend.shape.show,
		 symbol.shape.legend.title=symbol.shape.legend.title,
		 symbol.shape.legend.is.portrait=g$legend.shape.is.portrait,
		 symbol.shape.legend.reverse=g$legend.shape.reverse,
		 #symbol.shape.legend.hist=g$legend.hist,
		 #symbol.shape.legend.hist.title=symbol.shape.legend.hist.title,
		 symbol.shape.legend.z=symbol.shape.legend.z,
		 #symbol.shape.legend.hist.z=symbol.legend.hist.z,
		 symbol.col.legend.show=g$legend.col.show,
		 symbol.col.legend.title=symbol.col.legend.title,
		 symbol.col.legend.is.portrait=g$legend.col.is.portrait,
		 symbol.col.legend.reverse=g$legend.col.reverse,
		 symbol.col.legend.hist=g$legend.hist,
		 symbol.col.legend.hist.title=symbol.col.legend.hist.title,
		 symbol.col.legend.z=symbol.col.legend.z,
		 symbol.col.legend.hist.z=symbol.legend.hist.z,
		 symbol.id=g$id,
		 symbol.popup.vars=g$popup.vars,
		 symbol.popup.format=g$popup.format)
}

submit_symbol_shapes <- function(x, interactive, just, just.override, grob.dim) {
	shapeLib <- get(".shapeLib", envir = .TMAP_CACHE)
	justLib <- get(".justLib", envir = .TMAP_CACHE)
	n <- length(x)
	id <- 999 + length(shapeLib)
	if (interactive) {
		items <- lapply(x, function(xs) {
			ic <- if ("iconUrl" %in% names(xs)) {
				split_icon(xs)[[1]]
			} else if (is.grob(xs)) {
				grob2icon(xs, grob.dim, just)
			} else NA
			
			# add anchor based on just specs
			if (all(c("iconWidth", "iconHeight") %in% names(ic)) && 
				((!any(c("iconAnchorX", "iconAnchorY") %in% names(ic))) || just.override)) {
				ic$iconAnchorX <- ic$iconWidth * (1-just[1])
				ic$iconAnchorY <- ic$iconHeight * just[2]
			}
			ic
		})
		just_items <- as.list(rep(NA, n))
	} else {
		just_items <- lapply(x, function(xs) {
			if (just.override) {
				just
			} else if ("iconUrl" %in% names(xs)) {
				if (all(c("iconWidth", "iconHeight", "iconAnchorX", "iconAnchorY") %in% names(xs))) {
					c(1-(xs$iconAnchorX / xs$iconWidth), xs$iconAnchorY / xs$iconHeight)
				} else NA
			} else NA
		})
		
		items <- lapply(x, function(xs) {
			if ("iconUrl" %in% names(xs)) {
				grb <- icon2grob(xs)
				# take first one
				if (is.grob(grb)) grb else grb[[1]]
			} else if (is.grob(xs)) {
				xs
			} else NA
		})	
	}
	
	numbers <- is.na(items)

	if (all(numbers)) return(unlist(x))
	
	new_id <- id + 1:sum(!numbers)
	
	x2 <- integer(n)
	x2[numbers] <- unlist(x[numbers])
	x2[!numbers] <- new_id
	
	shapeLib <- c(shapeLib, items[!numbers])
	justLib <- c(justLib, just_items[!numbers])
	assign(".shapeLib", shapeLib, envir = .TMAP_CACHE)
	assign(".justLib", justLib, envir = .TMAP_CACHE)
	names(x2) <- names(x)
	x2
}
