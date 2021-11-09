tmapReproject = function(...) {
	UseMethod("tmapReproject")
}

#' @method tmapReproject stars
#' @export
tmapReproject.stars = function(shp, tmapID, crs) {
	
	shp[[1]] = tmapID
	
	shp2 = transwarp(shp, crs, raster.warp = TRUE)
	tmapID2 = shp2[[1]]
	shp2[[1]] = NA
	shapeTM(shp2, tmapID2)
}

#' @method tmapReproject sfc
#' @export
tmapReproject.sfc = function(shp, tmapID, crs) {
	shp2 = sf::st_transform(shp, crs)
	shapeTM(shp2, tmapID)
}




#' @import data.table
tmapShape = function(...) {
	UseMethod("tmapShape")
}


tmapGetShapeMeta = function(...) {
	UseMethod("tmapGetShapeMeta")
}


tmapShape.Raster = function(shp, is.main, crs, bbox, unit, filter, shp_name, o) {
	tmapShape.SpatRaster(terra::rast(shp), is.main, crs, bbox, unit, filter, shp_name)
}


#' @method tmapShape SpatRaster
#' @export
tmapShape.SpatRaster = function(shp, is.main, crs, bbox, unit, filter, shp_name, o) {
	#tmapShape.stars(stars::st_as_stars(shp), is.main, crs, bbox, unit, filter, shp_name)
	if (!requireNamespace("terra")) stop("terra package needed", call. = FALSE)
	
	
	
	shp = downsample_SpatRaster(shp, max.raster = o$max.raster)
	
	dt = data.table::setDT(terra::as.data.frame(shp, na.rm=FALSE))
	dt[, tmapID__:=1L:nrow(dt)]
	#dt = data.table::melt(dt, id.vars = "tmapID__", variable.name = "layer", value.name = "value")
	
	xy_dim = dim(shp)[1:2]
	b = terra::bbox(shp)
	
	crs = st_crs(shp)
	
	
	shp = structure(list(x = structure(list(from = 1, to = xy_dim[2], offset = b[1,1], delta = (b[1,2] - b[1,1]) / xy_dim[2], refsys = crs, point = FALSE, values = NULL), class = "dimension"),
			 y = structure(list(from = 1, to = xy_dim[1], offset = b[2,2], delta = (b[2,1] - b[2,2]) / xy_dim[1], refsys = crs, point = FALSE, values = NULL), class = "dimension")), class = "dimensions")
	attr(shp, "raster") = structure(list(affine = c(0, 0), dimensions = c("x", "y"), curvilinear = FALSE), class = "stars_raster")
			 
	
	#m = matrix(NA, nrow = xy_dim[2], ncol = xy_dim[1])
	
	#shp = stars::st_as_stars(list(values = m), dimensions = dimsxy)
	shpclass = "stars"
	
	bbox = st_bbox(shp)
	
	dtcols = setdiff(names(dt), "tmapID__")
	
	
	if (is.null(filter)) filter = rep(TRUE, nrow(dt))
	dt[, ':='(sel__ = filter)] # tmapID__ = 1L:nrow(dt), 
	
	shpTM = list(shp = shp, tmapID = 1L:(nrow(shp) * ncol(shp)))
	
	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name), class = "tmapShape")
	
}



#' @method tmapShape SpatRaster
#' @export
tmapShape.SpatVector = function(shp, is.main, crs, bbox, unit, filter, shp_name, o) {
	tmapShape.sf(sf::st_as_sf(shp), is.main, crs, bbox, unit, filter, shp_name)
	
}


get_fact_levels_na = function(x) {
	if (is.factor(x)) {
		levs = levels(x)
		if (!any(is.na(levs))) {
			if (any(is.na(x))) {
				levs = c(levs, NA)
			}
		}
	} else {
		levs = NULL
	}
	levs
}


#' @method tmapGetShapeMeta stars
#' @export
tmapGetShapeMeta.stars = function(shp) {
	d = stars::st_dimensions(shp)

	if (!has_raster(shp)) {
		d_non_xy = local({
			dimvals = lapply(seq_along(d), function(i) stars::st_get_dimension_values(shp, i))
			dimsfc = vapply(dimvals, inherits, what = "sfc", FUN.VALUE = logical(1))	
			d[!dimsfc]
		})
	} else {
		d_non_xy = local({
			dxy = attr(d, "raster")$dimensions	
			d[setdiff(names(d), dxy)]
		})
	}
	
	dims = names(d_non_xy)
	dims_vals = lapply(dims, function(d) stars::st_get_dimension_values(shp, d))		
	names(dims_vals) = dims
	
	vars = names(shp)
	vars_levs = lapply(seq_along(vars), function(i) {
		get_fact_levels_na(shp[[i]])
	})
	names(vars_levs) = vars
	
	list(vars = vars,
		 vars_levs = vars_levs,
		 dims = dims, 
		 dims_vals = dims_vals)
}

#' @method tmapGetShapeMeta sf
#' @export
tmapGetShapeMeta.sf = function(shp) {
	vars = setdiff(names(shp), attr(shp, "sf_column"))
	vars_levs = lapply(vars, function(v) {get_fact_levels_na(shp[[v]])})
	dims = character(0)
	dims_vals = list()
	
	list(vars = vars,
		 vars_levs = vars_levs,
		 dims = dims, 
		 dims_vals = dims_vals)
}





#' @method tmapShape stars
#' @export
tmapShape.stars = function(shp, is.main, crs, bbox, unit, filter, shp_name, o) {
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
			shpnames = names(shp)
			shp = stars::st_set_dimensions(shp, dimnms[dimid], values = 1L:length(geoms))
			shp = stars::st_set_dimensions(shp, names = dimnms_new)
		}
		
		dt = as.data.table(shp)
		attrcols = setdiff(names(dt), c("tmapID__", dimcols))
		
		if (!is.null(crs) && sf::st_crs(geoms) != crs) {
			shp = sf::st_transform(shp, crs)
		} else {
			shp = geoms
		}
		shpclass = "sfc"
	} else {
		shp = downsample_stars(shp, max.raster = o$max.raster)
		if (!is.null(crs) && sf::st_crs(shp) != crs) {
			shp = transwarp(shp, crs, raster.warp = TRUE)
		}
		
		dims = stars::st_dimensions(shp)
		rst = attr(dims, "raster")
		
		dim_xy = get_xy_dim(shp)
		dimsxy = dims[names(dim_xy)]
		
		if (rst$curvilinear) {
			shp2 = stars::st_set_dimensions(shp, rst$dimensions[1], values = 1L:nrow(shp))
			shp3 = stars::st_set_dimensions(shp2, rst$dimensions[2], values = 1L:ncol(shp))
			attr(attr(shp3, "dimensions"), "raster")$curvilinear = FALSE
		} else {
			shp2 = stars::st_set_dimensions(shp, rst$dimensions[1], values = {if (dimsxy[[1]]$delta > 0)  1L:nrow(shp) else nrow(shp):1L})
			shp3 = stars::st_set_dimensions(shp2, rst$dimensions[2], values = {if (dimsxy[[2]]$delta < 0)  1L:ncol(shp) else ncol(shp):1L})
		}
		

		#shp2 = stars::st_set_dimensions(shp, rst$dimensions[1], values = 1L:nrow(shp))
		#shp3 = stars::st_set_dimensions(shp2, rst$dimensions[2], values = 1L:ncol(shp))
		
		dimcols = setdiff(names(dim(shp3)), names(dim_xy))
		dimvls = lapply(dimcols, function(d) stars::st_get_dimension_values(shp3, d))		
		
		
		dt = as.data.table(shp3, center = FALSE)

		setnames(dt, names(dim_xy)[1], "X__")
		setnames(dt, names(dim_xy)[2], "Y__")
		
		dt[, tmapID__ := as.integer((Y__-1) * nrow(shp) + X__)]
		dt[, X__:= NULL]
		dt[, Y__:= NULL]
		
		attrcols = setdiff(names(dt), c("tmapID__", dimcols))
		attrvls = lapply(attrcols, function(a) {if (is.factor(dt[[a]])) levels(dt[[a]]) else NA})
		
		data.table::setorder(dt, cols = "tmapID__")
		#m = matrix(NA, nrow = nrow(shp), ncol = ncol(shp))
		
		#shp = stars::st_as_stars(list(values = m), dimensions = dimsxy)
		
		shp = dimsxy
		shpclass = "stars"
	}
	
	if (dev) {
		cat("dimensions:", dimcols, "\n")
		if (length(dimcols)) {
			mapply(function(dc, dv) cat("values dim,", dc, ":", dv, "\n"), dimcols, dimvls, SIMPLIFY = FALSE)
		}
		cat("attributes:", attrcols, "\n")
		if (length(attrcols)) {
			lapply(attrvls, function(av) cat("attribute values:", av, "\n"))
		}
	}
	

	bbox = sf::st_bbox(shp)
	
	dtcols = setdiff(names(dt), "tmapID__")
	
	filter = if (is.null(filter)) {
		rep(TRUE, nrow(dt))
	} else filter[dt$tmapID__]
	dt[, ':='(sel__ = filter)] # tmapID__ = 1L:nrow(dt), 
	
	shpTM = list(shp = shp, tmapID = 1L:(nrow(shp) * ncol(shp)))
	
	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, dimcols = dimcols, dimvls = dimvls, attrcols = attrcols, attrvls = attrvls, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name), class = "tmapShape")
}


#' @method tmapShape sf
#' @export
tmapShape.sf = function(shp, is.main, crs, bbox, unit, filter, shp_name, o) {
	if (!is.null(crs) && sf::st_crs(shp) != crs) {
		shp = sf::st_transform(shp, crs = crs)
	}
	
	sfc = sf::st_geometry(shp)
	dt = as.data.table(sf::st_drop_geometry(shp))
	
	dtcols = copy(names(dt))
	
	bbox = sf::st_bbox(sfc)
	
	if (is.null(filter)) filter = rep(TRUE, nrow(dt))
	dt[, ':='(tmapID__ = 1L:nrow(dt), sel__ = filter)]

	shpTM = list(shp = sfc, tmapID = 1L:(length(sfc)))

	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = "sfc", bbox = bbox, unit = unit, shp_name = shp_name), class = "tmapShape")
}
