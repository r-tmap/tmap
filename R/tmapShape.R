tmapReproject = function(...) {
	UseMethod("tmapReproject")
}

#' @method tmapReproject stars
#' @export
tmapReproject.stars = function(shp, tmapID, bbox = NULL, ..., crs) {
	shp[[1]] = tmapID
	
	shp2 = transwarp(shp, crs, raster.warp = TRUE)
	tmapID2 = shp2[[1]]
	shp2[[1]] = NA
	
	if (!is.null(bbox)) bbox = reproject_bbox(bbox, crs)
	
	shapeTM(shp2, tmapID2, bbox, ...)
}


#' @method tmapReproject dimensions
#' @export
tmapReproject.dimensions = function(shp, tmapID, bbox = NULL, ..., crs) {
	s = structure(list(tmapID = matrix(tmapID, nrow = nrow(shp))), dimensions = shp, class = "stars")
	
	if (is.na(sf::st_crs(shp))) {
		shp2 = s
	} else {
		shp2 = transwarp(s, crs, raster.warp = TRUE)
	}
	
	tmapID2 = shp2[[1]]
	
	d2 = stars::st_dimensions(shp2)
	if (!is.null(bbox)) bbox = reproject_bbox(bbox, crs)
	
	shapeTM(d2, tmapID2, bbox, ...)
}


#' @method tmapReproject sfc
#' @export
tmapReproject.sfc = function(shp, tmapID, bbox = NULL, ..., crs) {
	if (is.na(sf::st_crs(shp))) {
		shp2 = shp
		#sf::st_crs(shp2) = crs
		#warning("Setting missing CRS to ", as.character(crs))
	} else {
		shp2 = sf::st_transform(shp, crs)
	}
	if (!is.null(bbox)) bbox = reproject_bbox(bbox, crs)
	shapeTM(shp2, tmapID, bbox, ...)
}


reproject_bbox = function(bbox, crs) {
	s = tmaptools::bb_poly(bbox) #st_as_sfc does not add intermediate points
	s2 = sf::st_transform(s, crs)
	sf::st_bbox(s2)
}




#' Internal method that processed shape objects
#' 
#' Internal method that processed shape objects
#' 
#' @param shp shp
#' @param is.main is.main
#' @param crs crs
#' @param bbox bbox
#' @param unit unit
#' @param filter filter
#' @param shp_name shp_name
#' @param smeta smeta
#' @param o o
#' @param tmf tmf
#' @export
#' @import data.table
#' @keywords internal
tmapShape = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	UseMethod("tmapShape")
}



tmapShape.Raster = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	tmapShape.SpatRaster(terra::rast(shp), is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf)
}


#' @method tmapShape SpatRaster
#' @export
tmapShape.SpatRaster = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	#tmapShape.stars(stars::st_as_stars(shp), is.main, crs, bbox, unit, filter, shp_name)
	rlang::check_installed("terra")
	
	#if (!inherits(bbox, "bbox")) bbox = sf::st_bbox(bbox)
	
	
	shp = downsample_SpatRaster(shp, max.raster = o$raster.max.cells / (o$fn[1] * o$fn[2]))
	
	
	ctabs = terra::coltab(shp)
	cats = terra::cats(shp)
	
	
	dt = data.table::setDT(terra::as.data.frame(shp, na.rm=FALSE))
	dt[, tmapID__:=1L:nrow(dt)]
	#dt = data.table::melt(dt, id.vars = "tmapID__", variable.name = "layer", value.name = "value")
	
	xy_dim = dim(shp)[1:2]
	b = st_bbox(shp)
	
	
	crs = st_crs(shp)
	
	
	shp = structure(list(x = structure(list(from = 1, to = xy_dim[2], offset = b[1], delta = (b[3] - b[1]) / xy_dim[2], refsys = crs, point = FALSE, values = NULL), class = "dimension"),
			 y = structure(list(from = 1, to = xy_dim[1], offset = b[4], delta = (b[2] - b[4]) / xy_dim[1], refsys = crs, point = FALSE, values = NULL), class = "dimension")), class = "dimensions")
	attr(shp, "raster") = structure(list(affine = c(0, 0), dimensions = c("x", "y"), curvilinear = FALSE), class = "stars_raster")
			 
	
	#m = matrix(NA, nrow = xy_dim[2], ncol = xy_dim[1])
	
	#shp = stars::st_as_stars(list(values = m), dimensions = dimsxy)
	shpclass = "stars"
	
	#if (is.null(bbox)) bbox = st_bbox(shp)
	
	
	dtcols = setdiff(names(dt), "tmapID__")
	
	names(ctabs) = dtcols
	names(cats) = dtcols
	
	make_by_vars(dt, tmf, smeta)
	
	if (is.null(filter)) filter = rep(TRUE, nrow(dt))
	dt[, ':='(sel__ = filter)] # tmapID__ = 1L:nrow(dt), 
	
	shpTM = shapeTM(shp = shp, tmapID = 1L:(nrow(shp) * ncol(shp)), bbox = bbox)
	
	for (nm in dtcols) {
		if (!is.null(ctabs[[nm]])) {
			ct = ctabs[[nm]]
			lt = cats[[nm]]
			if (is.factor(dt[[nm]])) {
				#levels(dt[[nm]])
				
				ids = match(lt$value[match(levels(dt[[nm]]), lt$levels)], ct$value)
				cti = ct[ids,]
				
				cls = rgb(cti$red, cti$green, cti$blue, cti$alpha, maxColorValue = 255)
				
				levels(dt[[nm]]) = paste(levels(dt[[nm]]), cls, sep = "=<>=")
			} else if ("values" %in% names(ct)) {
				cls = rgb(ct$red, ct$green, ct$blue, ct$alpha, maxColorValue = 255)
				dt[[nm]] = factor(dt[[nm]], levels = ct$values, labels = paste(ct$values, cls, sep = "=<>="))
			}
				
		}
	}
	
	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name, smeta = smeta), class = "tmapShape")
	
}



#' @method tmapShape SpatRaster
#' @export
tmapShape.SpatVector = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	tmapShape.sf(sf::st_as_sf(shp), is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf)
}





#' Internal tmap function to create by variables (used for faceting)
#' 
#' Internal tmap function to create by variables (used for faceting)
#'
#' @param dt data.table
#' @param tmf tmf object
#' @param smeta smeta object
#' @export
#' @keywords internal
make_by_vars = function(dt, tmf, smeta) {
	by123 = paste0("by", 1L:3L) 
	by123__ = paste0("by", 1L:3L, "__")
	with(tmf, {
		if (length(b)) {
			for (w in b) {
				byvar = by123[w]
				byname = by123__[w]
				var = tmf[[byvar]]
				
				if (var %in% smeta$vars) {
					levs = smeta$vars_levs[[var]]
					if (attr(levs, "showNA")) levs[length(levs)] = NA
					dt[, (byname):= match(get(get(..byvar)), levs)]
				} else if (tmf[[byvar]] %in% smeta$dims) {
					dt[, (byname):= match(get(get(..byvar)), smeta$dims_vals[[var]])]
				}
			}
		}
	})
	invisible(NULL)
}



#' @method tmapShape stars
#' @export
tmapShape.stars = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	dev = getOption("tmap.devel.mode")
	
	if (dev) cat("-- stars object:", shp_name, "--\n")
		
	
	if (!has_raster(shp)) {
		dimnms = dimnames(shp)
		
		dimvals = lapply(1:length(dimnms), function(i) stars::st_get_dimension_values(shp, i))
		dimsfc = vapply(dimvals, inherits, what = "sfc", FUN.VALUE = logical(1))
		
		if (!any(dimsfc)) {
			stop("stars object ", shp_name, " is a stars object without raster and doens't have a geometry dimension")
		} else {
			dimid = which(dimsfc)
			geoms = dimvals[[dimid]]
			dimnms_new = dimnms
			dimnms_new[dimid] = "tmapID__"
			dimcols = dimnms_new[-dimid] # columns names, used for default facetting
			dimvls = lapply(dimcols, function(d) stars::st_get_dimension_values(shp, d))		
			shpnames = names(shp)
			shp = stars::st_set_dimensions(shp, dimnms[dimid], values = 1L:length(geoms))
			shp = stars::st_set_dimensions(shp, names = dimnms_new)
		}
		
		dt = as.data.table(shp)

		if (!is.null(crs) && sf::st_crs(geoms) != crs) {
			shp = sf::st_transform(shp, crs)
		} else {
			shp = geoms
		}
		shpTM = shapeTM(shp = shp, tmapID = 1L:length(shp), bbox = bbox)
		
		
		shpclass = "sfc"
	} else { 
		shp = downsample_stars(shp, max.raster = o$raster.max.cells / (o$fn[1] * o$fn[2]))
		if (!is.null(crs) && sf::st_crs(shp) != crs) {
			shp = transwarp(shp, crs, raster.warp = TRUE)
		}
		
		dims = stars::st_dimensions(shp)
		rst = attr(dims, "raster")
		
		dim_xy = get_xy_dim(shp)
		dimsxy = dims[names(dim_xy)]
		
		if (rst$curvilinear) {
			shp3 = shp
			attr(shp3, "dimensions")[[rst$dimensions[1]]]$values = 1L:nrow(shp)
			attr(shp3, "dimensions")[[rst$dimensions[2]]]$values = 1L:ncol(shp)
			attr(attr(shp3, "dimensions"), "raster")$curvilinear = FALSE
		} else {
			shp2 = stars::st_set_dimensions(shp, rst$dimensions[1], values = 1L:nrow(shp))
			shp3 = stars::st_set_dimensions(shp2, rst$dimensions[2], values = 1L:ncol(shp))
		}
		
		
		dt = as.data.table(shp3, center = FALSE)
		# Circumvent bug in tests on Windows
		tryCatch(
			names(dt) <- c(names(dim_xy), names(shp3)), # prevent "1" -> "X1" for split_stars_dim
			error = function(e) warning("could not rename the data.table")
		)

		
		setnames(dt, names(dim_xy)[1], "X__")
		setnames(dt, names(dim_xy)[2], "Y__")
		
		dt[, tmapID__ := as.integer((Y__-1) * nrow(shp) + X__)]
		dt[, X__:= NULL]
		dt[, Y__:= NULL]
		
		data.table::setorder(dt, cols = "tmapID__")

		shp = dimsxy
		shpclass = "stars"
		
	
		shpTM = shapeTM(shp = shp, tmapID = 1L:(nrow(shp) * ncol(shp)), bbox = bbox)
		
	}

	make_by_vars(dt, tmf, smeta)
	
	#if (is.null(bbox)) bbox = sf::st_bbox(shp)
	
	dtcols = setdiff(names(dt), "tmapID__")
	
	filter = if (is.null(filter)) {
		rep(TRUE, nrow(dt))
	} else filter[dt$tmapID__]
	dt[, ':='(sel__ = filter)] # tmapID__ = 1L:nrow(dt), 
	
	for (nm in dtcols) {
		if (is.factor(dt[[nm]])) {
			lvls = levels(dt[[nm]])
			cols = attr(dt[[nm]], "colors")
			ids = seq_along(lvls)[lvls!="NA"]
			if (!is.null(cols)) {
				#if (any(lvls=="NA")) {
					dt[[nm]] = factor(as.integer(dt[[nm]]), levels = ids, labels = paste(lvls[ids], cols[ids], sep = "=<>="))
				#}
				#dt[[nm]] = factor()
			}
		}
	}
	
	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name, smeta = smeta), class = "tmapShape")
}


#' @method tmapShape sf
#' @export
tmapShape.sf = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	if (!is.null(crs) && sf::st_crs(shp) != crs) {
		shp = sf::st_transform(shp, crs = crs)
	}
	
	sfc = sf::st_geometry(shp)
	dt = as.data.table(sf::st_drop_geometry(shp))
	
	dtcols = copy(names(dt))
	
	#if (is.null(bbox)) bbox = sf::st_bbox(sfc)
	
	if (is.null(filter)) filter = rep(TRUE, nrow(dt))
	dt[, ':='(tmapID__ = 1L:nrow(dt), sel__ = filter)]

	make_by_vars(dt, tmf, smeta)
	
	shpTM = shapeTM(shp = sfc, tmapID = 1L:(length(sfc)), bbox = bbox)

	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = "sfc", bbox = bbox, unit = unit, shp_name = shp_name, smeta = smeta), class = "tmapShape")
}
