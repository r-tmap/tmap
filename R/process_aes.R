process_aes <- function(type, xs, data, g, gt, gby, z, interactive) {
	
	
	## general variables
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- names(data)[1:(ncol(data)-2)]
	
	
	
	xs <- mapply(function(x, nm) {
		if (length(x)==1 && is.na(x)[1]) gt$aes.colors[nm] else x
	}, xs, names(xs), SIMPLIFY = FALSE)
	
	g <- check_g(g, gt)
	
	
	xlen <- sapply(xs, length)
	
	## general 'by' check: if by => |aes| = 1, and determine nx
	if (nlevels(by)>1 && any(xlen > 1)) warning("When by is specified (tm_facets), only one value can be assigned to each aesthetic.", call. = FALSE)
	
	
	nx <- max(xlen)
	
	## check fill special inputs (kernel density and map colors)
	
	if ("fill" %in% names(xs)) {
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
	
	if ("fill" %in% names(xs)) {
		res <- check_poly_sizes(g, data, nx, islist = is.list(dts[[1]]))
		areas <- res$areas
		sel <- res$sel
	} else {
		areas <- NULL
		sel <- NA
	}
	
	# ####################
	# ## back to old
	x <- xs[["fill"]]
	dt <- dts[[1]]
	# 
	# ############
	
	res <- mapply(function(x, xname, dt, fsname) {
		if (xname == "fill") {
			dcr <- process_dtcol(dt, sel, g, gt, nx, npol, check_dens = TRUE, areas=as.numeric(areas), areas_unit=attr(areas, "unit"), reverse=g[[aname("legend.reverse", xname)]])
			
			
			if (dcr$is.constant) x <- rep(NA, nx)
			# col <- dcr$col
			# col.legend.labels <- dcr$legend.labels
			# col.legend.values <- dcr$legend.values
			# col.legend.palette <- dcr$legend.palette
			# col.nonemptyFacets <- dcr$nonemptyFacets
			# col.neutral <- dcr$col.neutral
			# breaks <- dcr$breaks
			# values <- dcr$values
			# title_append <- dcr$title_append
			
			legend.show <- if (dcr$is.constant) rep(FALSE, nx) else rep(g[[aname("legend.show", xname)]], length.out = nx)
			
			if (nx > 1 && gby[[fsname]]) {
				emptyLegend <- sapply(dcr$legend.labels, function(ssll) is.na(ssll[1]))
				legend.show[emptyLegend] <- FALSE
			}
			
			
			legend.title <- if (is.ena(g[[aname("title", xname)]])[1]) paste(x, dcr$title_append) else g[[aname("title", xname)]]
			legend.z <- if (is.na(g[[aname("legend.z", xname)]])) z else g[[aname("legend.z", xname)]]
			legend.hist.z <- if (is.na(g[[aname("legend.hist.z", xname)]])) z+.5 else g[[aname("legend.hist.z", xname)]]
			
			if (g[[aname("legend.hist", xname)]] && is.ena(g[[aname("legend.hist.title", xname)]]) && legend.z>legend.hist.z) {
				# histogram is drawn between title and legend enumeration
				legend.hist.title <- legend.title
				legend.title <- ""
			} else if (g[[aname("legend.hist", xname)]] && !is.na(g[[aname("legend.hist.title", xname)]])) {
				legend.hist.title <- g[[aname("legend.hist.title", xname)]]
			} else legend.hist.title <- ""
			
			#if (!g$legend.show) fill.legend.title <- NA
			
			if (any(!legend.show)) legend.title[!legend.show] <- NA
			
			dcr[c("is.constant", "title_append")] <- NULL
			
			dcr$legend.misc <- list(lwd=g$gborders$lwd, border.col=g$gborders$col)
			
			c(dcr, list(x=x, legend.show = legend.show, legend.title = legend.title, legend.z = legend.z, legend.hist.z = legend.hist.z))
		} else {
			NULL
		}
		
	}, xs, names(xs), dts, fsnames, SIMPLIFY = FALSE)
	names(res) <- names(xs)
	
	nonemptyFacets <- apply(as.matrix(sapply(res, function(r) {
		r$nonemptyFacets	
	})), MARGIN = 1, all)
	
	layerInfo <- list(nonemptyFacets = nonemptyFacets,
						   id = g$id,
						   popup.vars = g$popup.vars,
						   popup.format = g$popup.format,
						   group = g$group)
	
	c(res, list(layerInfo = layerInfo))
}