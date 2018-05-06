aname <- function(x, a) {
	a <- gsub(".*\\.", "", a)
	
	if (a %in% c("fill", "raster")) {
		x
	} else {
		y <- gsub(".*\\.", "\\1", x)
		if (y == "title") {
			paste("title", a, sep = ".")
		} else {
			paste("legend", a, y, sep = ".")
		}
		
	}
}

## general color aesthetic, color NA, alpha checks / defaults
check_g <- function(g, gt) {
	if (is.null(g$colorNA)) g$colorNA <- "#00000000"
	if (is.na(g$colorNA)[1]) g$colorNA <- gt$aes.colors["na"]
	if (is.null(g$colorNULL)) g$colorNULL <- "#00000000"
	if (is.na(g$colorNULL)[1]) g$colorNULL <- gt$aes.colors["null"]
	if (g$colorNA=="#00000000") g$showNA <- FALSE
	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_polygons/tm_fill is not a numeric", call. = FALSE)
	g
}

process_aes <- function(type, xs, data, g, gt, gby, z, interactive) {
	
	
	## general variables
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-2)]
	
	
	
	xs <- mapply(function(x, nm) {
		if (length(x)==1 && is.na(x)[1]) gt$aes.colors[nm] else x
	}, xs, names(xs), SIMPLIFY = FALSE)
	
	g <- check_g(g, gt)
	
	
	# find length, and readjust it to 1 if by is specified
	xlen <- sapply(xs, length)
	if (nlevels(by)>1 && any(xlen > 1)) {
		warning("When by is specified (tm_facets), only one value can be assigned to each aesthetic.", call. = FALSE)
		xs <- lapply(xs, "[[", 1)
	}
	xlen <- sapply(xs, length)
	
	nx <- max(xlen)
	
	## make aesthetics same length and check whether they specified with variable names (e.g. vary...)
	xs <- lapply(xs, function(x) {
		if (length(x) < nx) rep(x, length.out=nx) else x
	})
	
	xvary <- lapply(xs, function(x) {
		all(x %in% shpcols) && !is.null(x)
	})
	

	## check fill special inputs (kernel density and map colors)
	
	if (type == "fill") {
		res <- check_fill_specials(xs[["fill"]], g, gt, shpcols, data, nx)
		xs[["fill"]] <- res$x
		data <- res$data
		is.colors <- res$is.colors
	} else {
		is.colors <- FALSE
	}
	
	
	fsnames <- paste0("free.scales.", names(xs))
	
	dts <- mapply(function(x, fsname) {
		process_data(data[, x, drop=FALSE], filter = data$tmapfilter, by=by, free.scales=gby[[fsname]], is.colors=is.colors)
	}, xs, fsnames, SIMPLIFY = FALSE)
	
	

	## impute showNA for first (i.e. color) aesthetic
	if (nlevels(by)>1) if (is.na(g$showNA) && !gby[[fsnames[[1]]]]) g$showNA <- any(attr(dts[[1]], "anyNA") & !(gby$drop.NA.facets & attr(dts[[1]], "allNA")))
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	
	## recalculate nx, now taking by into account
	nx <- max(nx, nlevels(by))
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	g$popup.format <- process_popup_format(g$popup.format, gt$legend.format, g$popup.vars)
	
	
	
	# # return if data is matrix of color values
	# if (is.matrix(dt)) {
	# 	sel <- attr(dt, "sel")
	# 	allNA <- attr(dt, "allNA")
	# 	fillna <- is.na(dt)
	# 	dt[fillna] <- g$colorNA
	# 	dt[!sel] <- g$colorNULL
	# 	col.nonemptyFacets <- !allNA
	# 	
	# 	return(list(fill=dt, 
	# 				fill.nonemptyFacets = col.nonemptyFacets,
	# 				xfill=rep(NA, nx), 
	# 				fill.lenged.title=rep(NA, nx),
	# 				fill.id=g$id,
	# 				fill.popup.vars=g$popup.vars,
	# 				fill.popup.format=g$popup.format,
	# 				fill.group = g$group))	
	# } 
	
	if (type == "fill") {
		res <- check_poly_sizes(g, data, nx, islist = is.list(dts[[1]]))
		areas <- res$areas
		sel <- rep(res$sel, nx)
	} else {
		areas <- NULL
		
		if (type == "line") {
			sel <- !is.na(unname(unlist(dts[["line.lwd"]])))
		} else if (type == "symbol") {
			sel <- !is.na(unname(unlist(dts[["symbol.size"]])))
		} else {
			sel <- NA	
		}
	}
	
	# ####################
	# ## back to old
	# x <- xs[["fill"]]
	# dt <- dts[[1]]
	# 
	# ############
	

	res <- mapply(function(x, xname, dt, fsname) {
		
		if (xname %in% c("fill", "line.col", "symbol.col", "raster", "text.col")) {
			dcr <- process_dtcol(xname, dt, sel, g, gt, nx, npol, check_dens = (xname == "fill"), areas=as.numeric(areas), areas_unit=attr(areas, "unit"))
		} else if (xname == "line.lwd") {
			dcr <- process_dtlwd(dt, g, gt, nx, npol, xvary$line.lwd)
		} else if (xname == "symbol.size") {
		} else if (xname == "text.size") {
		} else if (xname == "symbol.shape") {
		} else {
			stop("xname unknown")
		}

		if (dcr$is.constant) x <- rep(NA, nx)
		legend.show <- if (dcr$is.constant) rep(FALSE, nx) else rep(g[[aname("legend.show", xname)]], length.out = nx)
		legend.title <- if (is.ena(g[[aname("title", xname)]])[1]) paste(x, dcr$title_append) else g[[aname("title", xname)]]
		legend.z <- if (is.na(g[[aname("legend.z", xname)]])) z else g[[aname("legend.z", xname)]]

		if (nx > 1 && gby[[fsname]]) {
			emptyLegend <- sapply(dcr$legend.labels, function(ssll) is.na(ssll[1]))
			legend.show[emptyLegend] <- FALSE
		}
		if (any(!legend.show)) legend.title[!legend.show] <- NA

		
		## histogram
		if (xname %in% c("fill", "line.col", "symbol.col", "raster", "text.col")) {
			legend.hist.z <- if (is.na(g$legend.hist.z)) z+.5 else g$legend.hist.z
			
			if (g$legend.hist && is.ena(g$legend.hist.title) && legend.z>legend.hist.z) {
				# histogram is drawn between title and legend enumeration
				legend.hist.title <- legend.title
				legend.title <- ""
			} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
				legend.hist.title <- g$legend.hist.title
			} else legend.hist.title <- ""
			hlist <- list(legend.hist=g$legend.hist, legend.hist.title = legend.hist.title, legend.hist.z = legend.hist.z)
		} else {
			hlist <- list()
		}		
				
		dcr[c("is.constant", "title_append", "col.neutral")] <- NULL

		c(dcr, list(x=x, legend.show = legend.show, legend.title = legend.title, legend.is.portrait = g$legend.is.portrait, legend.reverse = g$legend.reverse, legend.z = legend.z), hlist)

	}, xs, names(xs), dts, fsnames, SIMPLIFY = FALSE)
	names(res) <- names(xs)
	
	nonemptyFacets <- unname(apply(as.matrix(sapply(res, function(r) {
		r$nonemptyFacets	
	})), MARGIN = 1, all))
	
	layerInfo <- list(nonemptyFacets = nonemptyFacets,
						   id = g$id,
						   popup.vars = g$popup.vars,
						   popup.format = g$popup.format,
						   group = g$group)
	
	c(res, list(layerInfo = layerInfo))
}