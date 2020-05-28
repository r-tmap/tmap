check_symbol_specials <- function(xcol, xsize, xshape, g, gt, gby, xvary, data, nx, interactive) {

	if (!xvary[["symbol.size"]]) {
		if (!all(is.numeric(xsize))) stop("symbol sizes are neither numeric nor valid variable name(s)", call. = FALSE)
		for (i in 1:nx) data[[paste("SIZE", i, sep="_")]] <- xsize[i]
		xsize <- paste("SIZE", 1:nx, sep="_")
		gby$free.scales.symbol.size <- FALSE
	}
	
	if (xvary[["symbol.col"]]) {
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
	
	if (!xvary[["symbol.shape"]]) {
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
		if (inherits(g$shapes, "grob") || any(vapply(g$shapes, inherits, FUN.VALUE = logical(1), "grob")) || (("iconUrl" %in% names(g$shapes)))) {
			# one grob, list of grobs or icon(s)			
			if ("iconUrl" %in% names(g$shapes)) g$shapes <- split_icon(g$shapes)
			g$shapes <- submit_symbol_shapes(g$shapes, interactive=interactive, just=just, just.override=just.override, grob.dim=g$grob.dim)			
		} else {
			# list of list of grobs or icons
			g$shapes <- lapply(g$shapes, function(gshape) {
				if ("iconUrl" %in% names(gshape)) gshape <- split_icon(gshape)
				submit_symbol_shapes(gshape, interactive=interactive, just=just, just.override=just.override, grob.dim=g$grob.dim)	
			})
		}
	} 
	
	
	
	
	list(xcol = xcol, xsize = xsize, xshape = xshape, g=g, gby = gby, data = data, is.colors = is.colors, just = just)		
}

postprocess_symbols <- function(res, g, gt, data, npol, nx, just, interactive) {
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
	#	col.neutral <- if (is.na(g$shapes.legend.fill)[1]) gt$aes.colors["symbols"] else  g$shapes.legend.fill
	} else {
		shape.neutral <- NULL
	#	col.neutral <- NA
	}

	col.neutral <- if (is.na(g$shapes.legend.fill)[1]) NA else  g$shapes.legend.fill
	
		
	# if (!g$legend.size.show) symbol.size.legend.title <- NA
	# if (!g$legend.col.show) symbol.col.legend.title <- NA
	# if (!g$legend.shape.show) symbol.shape.legend.title <- NA
	
	are.icons <- any(!is.na(res$symbol.shape) & res$symbol.shape>999)
	
	if (are.icons && !interactive) {
		scale <- g$scale * g$icon.scale
		res$symbol.size <- res$symbol.size * g$icon.scale
		res$symbol.size.legend.sizes <- res$symbol.size.legend.sizes * g$icon.scale
		g$legend.max.symbol.size <- g$legend.max.symbol.size * g$icon.scale
		
	} else scale <- g$scale
	
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
	
	
	clustering <- g$clustering
	if (identical(clustering, FALSE)) {
		clustering <- NULL
	} else if (identical(clustering, TRUE)) {
		clustering <- leaflet::markerClusterOptions()	
	}
	
	# list(
	# 	 shape.neutral = shape.neutral,
	# 	 col.neutral = col.neutral,
	# 	 are.icons = are.icons,
	# 	 scale = scale)
	
	
	res$symbol.col.legend.misc$symbol.border.col <- symbol.border.col
	res$symbol.size.legend.misc$symbol.border.col <- symbol.border.col
	res$symbol.shape.legend.misc$symbol.border.col <- symbol.border.col
	
	
	
	res$symbol.col.legend.sizes <- res$symbol.size.legend.misc$symbol.max.size
	res$symbol.col.legend.shapes <- if (is.null(shape.neutral)) res$symbol.shape.legend.misc$shape.neutral else shape.neutral
	
	res$symbol.size.legend.shapes <- if (is.null(shape.neutral)) res$symbol.shape.legend.misc$shape.neutral else shape.neutral
	if (!is.na(col.neutral)) res$symbol.size.legend.palette <- col.neutral

	res$symbol.shape.legend.sizes <- res$symbol.size.legend.misc$symbol.max.size
	if (!is.na(col.neutral)) res$symbol.shape.legend.palette <- col.neutral

	
	
	
	res$symbol.misc <- list(symbol.are.dots=g$are.dots, symbol.are.markers=g$are.markers, symbol.are.icons=are.icons, just=just, clustering=clustering)
	
	res$symbol.xmod <- xmod
	res$symbol.ymod <- ymod
	
	res$symbol.border.lwd <- g$border.lwd
	res$symbol.border.col <- symbol.border.col
	
	res
	
}



process_symbols <- function(data, g, gt, gby, z, interactive) {
	
	
	# aesthetics
	xs <- list(symbol.col = g$col, symbol.size = g$size, symbol.shape = g$shape)
	process_aes(type = "symbol", xs, c("xcol", "xsize", "xshape"), ifelse(g$are.dots, "dots", "symbols"), data, g, gt, gby, z, interactive)
}


submit_symbol_shapes <- function(x, interactive, just, just.override, grob.dim) {
	if (any(vapply(x, is.null, logical(1)))) stop("one of more shapes are NULL")
	shapeLib <- get("shapeLib", envir = .TMAP_CACHE)
	justLib <- get("justLib", envir = .TMAP_CACHE)
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
	
	if (all(numbers)) return(unlist(x, use.names = FALSE))
	
	new_id <- id + 1:sum(!numbers)
	
	x2 <- integer(n)
	x2[numbers] <- unlist(x[numbers], use.names = FALSE)
	x2[!numbers] <- new_id
	
	shapeLib <- c(shapeLib, items[!numbers])
	justLib <- c(justLib, just_items[!numbers])
	assign("shapeLib", shapeLib, envir = .TMAP_CACHE)
	assign("justLib", justLib, envir = .TMAP_CACHE)
	names(x2) <- names(x)
	x2
}

