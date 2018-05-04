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

check_fill_specials <- function(x, g, gt, shpcols, data, nx) {
	if (attr(data, "kernel_density") && !("col" %in% g$call) && "level" %in% shpcols) {
		is.colors <- FALSE
		x <- "level"
	} else if (!all(x %in% shpcols)) {
		# check for direct color input
		is.colors <- all(valid_colors(x))
		if (is.colors) {
			x <- do.call("process_color", c(list(col=col2hex(x), alpha=g$alpha), gt$pc))
			for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- x[i]
			x <- paste("COLOR", 1:nx, sep="_")
		} else if (x[1]=="MAP_COLORS") {
			palette <- if (is.null(g$palette)) {
				gt$aes.palette[["cat"]]
			} else if (g$palette[1] %in% c("seq", "div", "cat")) {
				gt$aes.palette[[g$palette[1]]] 
			} else g$palette
			mapcols <- do.call("map_coloring", args = c(list(x=attr(data, "NB"), palette=palette, contrast = g$contrast), g$map_coloring))
			mapcols <- do.call("process_color", c(list(col=mapcols, alpha=g$alpha), gt$pc))
			
			for (i in 1:nx) data[[paste("COLOR", i, sep="_")]] <- mapcols
			x <- paste("COLOR", 1:nx, sep="_")
		} else {
			stop("Fill argument neither colors nor valid variable name(s)", call. = FALSE)
		}
	} else {
		is.colors <- FALSE
	}
	
	list(x = x,
		 data = data,
		 is.colors = is.colors)
}


check_poly_sizes <- function(g, data, nx, islist) {
	# process areas
	if (is.null(g$area)) {
		area_var <- "SHAPE_AREAS"
	} else {
		area_var <- g$area
	}
	
	areas <- data[[area_var]]
	
	if (any(is.na(areas)) || any(is.infinite(areas))) {
		if (g$convert2density) {
			warning("Some polygon areas cannot be determined. Therefore, convert2density is set to FALSE.", call. = FALSE)
		}
		areas_na_inf <- is.na(areas) | is.infinite(areas)
		areas[areas_na_inf] <- mean(areas[!areas_na_inf])
		
	}
	areas_prop <- as.numeric(areas/sum(areas, na.rm=TRUE))
	
	tiny <- areas_prop < g$thres.poly
	if (all(tiny)) warning("all relative area sizes are below thres.poly", call. = FALSE)
	
	sel <- if (islist) rep(list(!tiny), nx) else !tiny
	list(areas = areas, sel = sel)
}


process_fill <- function(data, g, gt, gby, z, interactive) {
	
	
	type <- "Lfill"
	
	## aesthetics
	xs <- list(fill = g$col)
	
	res <- process_aes(type, xs, data, g, gt, gby, z, interactive)

	fill <- res$fill
	names(fill) <- paste0("fill.", names(fill))
	names(fill)[1] <- "fill"
	names(fill)[names(fill=="fill.x")] <- "xfill"

	layerInfo <- res$layerInfo
		
	names(layerInfo) <- paste0("fill.", names(layerInfo))
	
	c(fill, layerInfo)
	
	# list(fill=col,
	# 	 fill.legend.labels=col.legend.labels,
	# 	 fill.legend.values=col.legend.values,
	# 	 fill.legend.palette=col.legend.palette,
	# 	 fill.legend.misc=list(lwd=gb$lwd, border.col=gb$col),
	# 	 fill.legend.hist.misc=list(values=values, breaks=breaks, densities=g$convert2density),
	# 	 xfill=x,
	# 	 fill.legend.show=fill.legend.show,
	# 	 fill.legend.title=fill.legend.title,
	# 	 fill.legend.is.portrait=g$legend.is.portrait,
	# 	 fill.legend.reverse=g$legend.reverse,
	# 	 fill.legend.hist=g$legend.hist,
	# 	 fill.legend.hist.title=fill.legend.hist.title,
	# 	 fill.legend.z=fill.legend.z,
	# 	 fill.legend.hist.z=fill.legend.hist.z,
	# 	 
	# 	 fill.nonemptyFacets = col.nonemptyFacets,
	# 	 fill.id=g$id,
	# 	 fill.popup.vars=g$popup.vars,
	# 	 fill.popup.format=g$popup.format,
	# 	 fill.group = g$group)
}

aname <- function(x, a) {
	if (a == "fill") {
		x
	} else {
		x
	}
}
