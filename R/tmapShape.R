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


#' @method tmapShape stars
#' @export
tmapShape.stars = function(shp, is.main, crs, bbox, unit, filter, shp_name, o) {
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
		
		shp2 = stars::st_set_dimensions(shp, rst$dimensions[1], values = {if (dimsxy[[1]]$delta > 0)  1L:nrow(shp) else nrow(shp):1L})
		shp3 = stars::st_set_dimensions(shp2, rst$dimensions[2], values = {if (dimsxy[[2]]$delta < 0)  1L:ncol(shp) else ncol(shp):1L})

		#shp2 = stars::st_set_dimensions(shp, rst$dimensions[1], values = 1L:nrow(shp))
		#shp3 = stars::st_set_dimensions(shp2, rst$dimensions[2], values = 1L:ncol(shp))
		
				
		dt = as.data.table(shp3, center = FALSE)
		
		setnames(dt, names(dim_xy)[1], "X__")
		setnames(dt, names(dim_xy)[2], "Y__")
		
		dt[, tmapID__ := as.integer((Y__-1) * nrow(shp) + X__)]
		dt[, X__:= NULL]
		dt[, Y__:= NULL]
		
		data.table::setorder(dt, cols = "tmapID__")
		#m = matrix(NA, nrow = nrow(shp), ncol = ncol(shp))
		
		#shp = stars::st_as_stars(list(values = m), dimensions = dimsxy)
		
		shp = dimsxy
		shpclass = "stars"
	}
	
	bbox = sf::st_bbox(shp)
	
	dtcols = setdiff(names(dt), "tmapID__")
	
	filter = if (is.null(filter)) {
		rep(TRUE, nrow(dt))
	} else filter[dt$tmapID__]
	dt[, ':='(sel__ = filter)] # tmapID__ = 1L:nrow(dt), 
	
	shpTM = list(shp = shp, tmapID = 1L:(nrow(shp) * ncol(shp)))
	
	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name), class = "tmapShape")
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


get_asp_ratio = function (x)  {
	bbx = sf::st_bbox(x)
	crs = sf::st_crs(x)
	
	ll = sf::st_is_longlat(crs)
	
	xlim = bbx[c(1, 3)]
	ylim = bbx[c(2, 4)]
	asp = if (diff(xlim) == 0 || diff(ylim) == 0) {
		1
	}
	else unname((diff(xlim)/diff(ylim)) * ifelse(ll,cos((mean(ylim) * pi)/180), 1))
	asp
}
