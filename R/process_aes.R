aname <- function(x, a) {
	a <- gsub(".*\\.", "", a)
	
	if (a %in% c("fill", "raster")) {
		x
	} else {
		y <- gsub("^[a-z]*\\.", "\\1", x)
		if (y == "title") {
			paste("title", a, sep = ".")
		} else {
			paste("legend", a, y, sep = ".") # e.g. legend.is.portrait => legend.fill.is.portrait
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
	if (!is.na(g$alpha) && !is.numeric(g$alpha)) stop("alpha argument in tm_XXX is not a numeric", call. = FALSE)
	g
}


## checks if columns have to be converted to density and determines title_append (when converted 2 density or when units column is used)
check_num_col <- function(col, g, areas = NULL, areas_unit = NULL) {
	if (is.numeric(col) && !is.null(g$convert2density) && g$convert2density) {
		col <- col / areas
		title_append <- paste(" per", areas_unit)
	} else if (!is.null(attr(col, "units"))) {
		title_append <- paste0(" per ", attr(col, "units"))
	} else title_append <- ""
	list(col = col, title_append = title_append)
}


process_aes <- function(type, xs, xlabels, colname, data, g, gt, gby, z, interactive, fill = NA) {

	## general variables
	npol <- nrow(data)
	by <- data$GROUP_BY
	shpcols <- setdiff(names(data), c("tmapfilter", "GROUP_BY", "ALONG"))
	
	treat_as_by = attr(data, "treat_as_by")
	
	xs <- mapply(function(x, nm) {
		if (inherits(x, c("sf", "stars", "raster"))) {
			stop("Spatial objects should be specified with tm_shape.", call. = FALSE)
		}
		
		#if (length(x)==1 && is.na(x)[1] && !any(type == c("raster", "text")) && !treat_as_by) gt$aes.colors[nm] else x
		if (treat_as_by) {
			if (!is.na(x[1])) {
				if (gt$show.warnings) {
					if (type == "raster") {
						warning("col specification in tm_raster is ignored, since stars object contains a 3rd dimension, where its values are used to create facets", call. = FALSE)		
					} else {
						warning("col specification in tm_fill/tm_polygons is ignored, since stars object contains another dimension, where its values are used to create facets", call. = FALSE)
					}
				}
			} 
			attr(data, "shpnames")
		} else if (length(x)==1 && is.na(x[1]) && !any(type == c("raster", "text"))) {
			gt$aes.colors[nm]
		} else {
			x
		}
	}, xs, colname, SIMPLIFY = FALSE)
	
	## put symbol shapes in list
	if (type == "symbol") {
		if (is.list(xs[["symbol.shape"]]) && "iconUrl" %in% names(xs[["symbol.shape"]])) xs[["symbol.shape"]] <- split_icon(xs[["symbol.shape"]])
		if (is.grob(xs[["symbol.shape"]])) xs[["symbol.shape"]] <- list(xs[["symbol.shape"]])
	} 
	
	g <- check_g(g, gt)
	
	
	# find length, and readjust it to 1 if by is specified
	xlen <- vapply(xs, length, integer(1))
	if (nlevels(by)>1 && any(xlen > 1)) {
		if (gt$show.warnings) warning("When by is specified (tm_facets), only one value can be assigned to each aesthetic.", call. = FALSE)
		xs <- lapply(xs, "[[", 1)
	}
	
	xlen <- vapply(xs, length, integer(1))
	
	nx_fill <- if (is.na(fill[1])) 1 else if (is.matrix(fill)) ncol(fill) else 1 # backgrond for tm_text
	
	nx <- max(xlen, nx_fill)
	
	## make aesthetics same length and check whether they specified with variable names (e.g. vary...)
	xs <- lapply(xs, function(x) {
		if (length(x) < nx) rep(x, length.out=nx) else x
	})
	
	xvary <- vapply(xs, function(x) {
		all(x %in% shpcols) && !is.null(x)
	}, logical(1))

	if (type == "text") {
		if ((xvary[["text.size"]] || identical(xs[["text.size"]], "AREA")) && interactive && !gt$text.size.variable) {
			if (gt$show.messages) message("Text size will be constant in view mode. Set tm_view(text.size.variable = TRUE) to enable variable text sizes.")
			xvary[["text.size"]] <- FALSE
			xlen["text.size"] <- 1
			xs[["text.size"]] <- 1
			
			# recalculate nx and xs
			nx <- max(xlen, nx_fill)
			xs <- lapply(xs, function(x) {
				if (length(x) < nx) rep(x, length.out=nx) else x
			})
		}	
	}
	
		

	## check special inputs
	
	if (type == "fill") {
		res <- check_fill_specials(xs[["fill"]], g, gt, shpcols, data, nx)
		xs[["fill"]] <- res$x
		nx <- res$nx
		
		data <- res$data
		is.colors <- res$is.colors
		split.by <- TRUE
	} else if (type == "line") {
		res <- check_line_specials(xs[["line.col"]], xs[["line.lwd"]], g, gt, gby, xvary, data, nx)

		xs[["line.col"]] <- res$xcol
		xs[["line.lwd"]] <- res$xlwd
		gby <- res$gby
		data <- res$data
		is.colors <- c(res$is.colors, FALSE)
		split.by <- c(TRUE, res$split.by)
	} else if (type == "symbol") {
		res <- check_symbol_specials(xs[["symbol.col"]], xs[["symbol.size"]], xs[["symbol.shape"]], g, gt, gby, xvary, data, nx, interactive)
		
		xs[["symbol.col"]] <- res$xcol
		xs[["symbol.size"]] <- res$xsize
		xs[["symbol.shape"]] <- res$xshape
		g <- res$g
		gby <- res$gby
		data <- res$data
		is.colors <- c(res$is.colors, FALSE, FALSE)
		just <- res$just
		split.by <- rep(TRUE, 3)
	} else if (type == "raster") {
		res <- check_raster_specials(xs[["raster"]], g, gt, shpcols, data, nx)
		g <- res$g
		xs[["raster"]] <- res$x
		data <- res$data
		is.colors <- res$is.colors
		xvary[["raster"]] <- !is.colors # xvary will chang when tm_raster() is called
		nx <- res$nx
		misc <- res$misc
		
		split.by <- TRUE
	} else if (type == "text") {
		res <- check_text_specials(fill, xs[["text.col"]], xs[["text.size"]], g, gt, gby, xvary, data, shpcols, nx, npol, interactive)

		xs[["text.col"]] <- res$xtcol
		xs[["text.size"]] <- res$xtsize
		g <- res$g
		gby <- res$gby
		data <- res$data
		is.colors <- c(FALSE, res$is.colors)

		fill <- res$fill
		collight <- res$collight
		coldark <- res$coldark
		
		xtext <- res$xtext
		
		split.by <- rep(TRUE, 2)
	}
	
	fsnames <- paste0("free.scales.", names(xs))
	
	dts <- mapply(function(x, fsname, isc, sby, xv) {
		process_data(data[, x, drop=FALSE], filter = data$tmapfilter, by=by, free.scales=gby[[fsname]], is.colors=isc, split.by = sby, vary = xv)
	}, xs, fsnames, is.colors, split.by, xvary, SIMPLIFY = FALSE)
	
	## impute showNA for first (i.e. color) aesthetic
	if (nlevels(by)>1) if (is.na(g$showNA) && !gby[[fsnames[[1]]]]) g$showNA <- any(attr(dts[[1]], "anyNA") & !(gby$drop.NA.facets & attr(dts[[1]], "allNA")))
	
	if (type == "symbol") {
		if (nlevels(by)>1) if (is.na(g$shape.showNA) && !gby[[fsnames[[3]]]]) g$shape.showNA <- any(attr(dts[[3]], "anyNA") & !(gby$drop.NA.facets & attr(dts[[3]], "allNA")))
	}
	
	# if (type == "raster") {
	# 	attr(dts[[1]], "raster.projected") <- attr(data, "raster.projected")
	# }
	
	
	## output: matrix=colors, list=free.scales, vector=!freescales
	
	
	## recalculate nx, now taking by into account
	nx <- max(nx, nlevels(by))
	
	## as.layers tip
	if (nx > 1 && ("raster" %in% names(xvary)) && xvary[["raster"]] && !gby$as.layers && gt$show.messages && interactive) {
		message("Tip: rasters can be shown as layers instead of facets by setting tm_facets(as.layers = TRUE).")
	}
	
	# update legend format from tm_layout
	g$legend.format <- process_legend_format(g$legend.format, gt$legend.format, nx)
	g$popup.format <- process_popup_format(g$popup.format, gt$legend.format, g$popup.vars, show.warnings = gt$show.warnings)

	if (type == "fill") {
		res <- check_poly_sizes(g, data, nx, islist = is.list(dts[[1]]), show.warnings = gt$show.warnings)
		areas <- res$areas
		sel <- rep(res$sel, nx)
	} else {
		areas <- NULL
		
		if (type == "line") {
			sel <- !is.na(unname(unlist(dts[["line.lwd"]], use.names = FALSE)))
		} else if (type == "symbol") {
			sel <- !is.na(unname(unlist(dts[["symbol.size"]], use.names = FALSE)))
		} else {
			sel <- NA	
		}
	}
	
	if (type == "text") {
		text <- if (nx > 1) matrix(unlist(lapply(data[, xtext], as.character), use.names = FALSE), nrow=npol, ncol=nx) else as.character(data[[xtext]])
		if (!is.na(g$case)) text <- if(g$case=="upper") toupper(text) else tolower(text)
	} else {
		text <- NULL
	}
	
	

	res <- mapply(function(x, xname, dt, fsname) {
		if (xname %in% c("fill", "line.col", "symbol.col", "raster", "text.col")) {
			if (xname == "text.col") {
				text_sel <- get("text_sel", envir = .TMAP_CACHE)
				sel <- as.vector(text_sel)
			} else {
				text_sel <- NULL
			}
			
			dcr <- process_dtcol(xname, dt, sel, g, gt, nx, npol, areas=as.numeric(areas), areas_unit=attr(areas, "unit"), text = text, text_sel = text_sel)
			#if (xname == "fill") assign("dcr_fill", dcr$col, pos = 1) # needed for tm_text
			assign("col.neutral", dcr$col.neutral, envir = .TMAP_CACHE)
		} else if (xname == "line.lwd") {
			dcr <- process_dtlwd(dt, g, gt, nx, npol, xvary["line.lwd"], get("col.neutral", envir = .TMAP_CACHE))
		} else if (xname == "symbol.size") {
			dcr <- process_dtsize(dt, g, gt, nx, npol, xvary["symbol.size"], get("col.neutral", envir = .TMAP_CACHE))
		} else if (xname == "symbol.shape") {
			dcr <- process_dtshape(dt, g, gt, sel, nx, npol, xvary["symbol.shape"], get("col.neutral", envir = .TMAP_CACHE))
		} else if (xname == "text.size") {
			dcr <- process_dttsize(dt, text, g, gt, nx, npol, xvary["text.size"]) 
			assign("text_sel", dcr$text_sel, .TMAP_CACHE)
		} else {
			stop("xname unknown")
		}

		if (dcr$is.constant) x <- rep(NA, nx)
		legend.show <- if (dcr$is.constant) rep(FALSE, nx) else rep(g[[aname("legend.show", xname)]], length.out = nx)
		
		if (xname %in% c("fill", "line.col", "symbol.col", "raster", "text.col")) {
			if (is.list(dcr$legend.palette)) {
				legend.empty <- vapply(dcr$legend.palette, FUN = function(x) length(x) == 0L, logical(1))
			} else {
				legend.empty <- length(dcr$legend.palette) == 0L
			}
			legend.show <- legend.show & !legend.empty
		}

		
		# overrule legend.show in case the legend is empty (all NA, colorNA = "#00000000)
		
		
		if (is.list(dcr$legend.labels)) {
			emptySizeLegend <- vapply(dcr$legend.labels, function(ssll) is.na(ssll[1]), logical(1))
			legend.show[emptySizeLegend] <- FALSE
		}
		
	
		
		if (xname == "text.col") {
			dcr$legend.text <- dcr$legend.misc$legend.text
		}
		
		# else if (xname == "text.size") {
		# 	dcr$legend.text <- dcr$legend.misc$legend.text
		# }
		
		
		legend.title <- rep(
			if (!is.ena(g[[aname("title", xname)]])[1]) {
				g[[aname("title", xname)]]
			} else if (attr(data, "treat_as_by")) {
				attr(data, "by_var")
			} else if (is.ena(g[[aname("title", xname)]])[1]) {
				paste0(x, dcr$title_append)
			}, 
			length.out = nx)
		legend.z <- if (is.na(g[[aname("legend.z", xname)]])) z else g[[aname("legend.z", xname)]]

		# if (nx > 1 && gby[[fsname]]) {
		# 	emptyLegend <- sapply(dcr$legend.labels, function(ssll) is.na(ssll[1]))
		# 	legend.show[emptyLegend] <- FALSE
		# }
		
		
		# see also process_layers L189
		#if (any(!legend.show)) legend.title[!legend.show] <- NA 	#disabled when fixing #290

		
		## histogram
		
		
		if (xname %in% c("fill", "line.col", "symbol.col", "raster", "text.col")) {
			if (any(g$style %in% c("order", "cont")) && g$legend.hist) {
				warning("Histogram not supported for styles \"cont\" or \"order\"", call. = FALSE)
				g$legend.hist = FALSE
				hlist <- list(legend.hist = FALSE)
			} else {
				legend.hist.z <- if (is.na(g$legend.hist.z)) z+.5 else g$legend.hist.z
				
				if (g$legend.hist && is.ena(g$legend.hist.title) && legend.z>legend.hist.z) {
					# histogram is drawn between title and legend enumeration
					legend.hist.title <- legend.title
					legend.title <- ""
				} else if (g$legend.hist && !is.na(g$legend.hist.title)) {
					legend.hist.title <- g$legend.hist.title
				} else legend.hist.title <- ""
				hlist <- list(legend.hist=g$legend.hist, legend.hist.title = legend.hist.title, legend.hist.z = legend.hist.z)
			}
		} else {
			hlist <- list()
		}		
				
		dcr[c("is.constant", "title_append", "col.neutral")] <- NULL

		c(dcr, list(x=x, legend.show = legend.show, legend.title = legend.title, legend.is.portrait = g[[aname("legend.is.portrait", xname)]], legend.reverse = g[[aname("legend.reverse", xname)]], legend.z = legend.z), hlist)

	}, xs, names(xs), dts, fsnames, SIMPLIFY = FALSE)
	names(res) <- names(xs)
	
	
	
	nFs <- lapply(res, function(r) r$nonemptyFacets)
	nFsNULL <- vapply(nFs, is.null, logical(1))
	
	if (all(nFsNULL)) {
		nonemptyFacets <- NULL
	} else {
		mat <- do.call(cbind, nFs[!nFsNULL])
		nonemptyFacets <- unname(apply(mat, MARGIN = 1, all))	
	}
	
	#id <- if (is.null(g$id) || is.na(g$id)) names(data)[1] else g$id
	id <- if (is.null(g$id)) NA else if (is.na(g$id)) names(data)[1] else g$id
	
	
	layerInfo <- list(nonemptyFacets = nonemptyFacets,
					  id = id,popup.vars = g$popup.vars,
					  interactive = g$interactive,
					  popup.format = g$popup.format,
					  group = g$group,
					  zindex = g$zindex)
	
	
	
	
	res <- mapply(function(rs, xn, xl) {
		names(rs) <- paste(xn, names(rs), sep = ".")
		names(rs)[1] <- xn
		names(rs)[names(rs)==paste0(xn, ".x")] <- xl
		rs
	}, res, names(xs), xlabels, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	res <- do.call(c, res)
	
	if (type == "line") {
		
		if (!is.na(g$lwd.legend.col)[1]) res$line.lwd.legend.palette <- g$lwd.legend.col
		res$line.col.legend.misc$line.legend.lwd <- assign_legend_line_widths(res$line.lwd.legend.misc$legend.lwds, res$line.lwd, nx)
		res$line.lty <- g$lty
		res$line.alpha <- g$alpha
	} else if (type == "symbol") {
		res <- postprocess_symbols(res, g, gt, data, npol, nx, just, interactive)
	} else if (type == "raster") {
		res$raster.misc <- misc
	} else if (type == "text") {
		res <- postprocess_text(res, g, gt, data, npol, nx, just, interactive, text, collight, coldark, xtext)
	}
	
	
	names(layerInfo) <- paste(type, names(layerInfo), sep = ".")

	c(res, layerInfo)
}