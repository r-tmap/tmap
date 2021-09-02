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



#' @method tmapShape SpatRaster
#' @export
tmapShape.SpatRaster = function(shp, is.main, crs, bbox, unit, shp_name) {
	tmapShape.stars(stars::st_as_stars(shp), is.main, crs, bbox, unit, shp_name)
}

#' @method tmapShape SpatRaster
#' @export
tmapShape.SpatVector = function(shp, is.main, crs, bbox, unit, shp_name) {
	tmapShape.sf(sf::st_as_sf(shp), is.main, crs, bbox, unit, shp_name)
}


#' @method tmapShape stars
#' @export
tmapShape.stars = function(shp, is.main, crs, bbox, unit, shp_name) {
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
		shp = downsample_stars(shp, max.raster = tmap_options()[["max.raster"]])
		if (!is.null(crs) && sf::st_crs(shp) != crs) {
			shp = transwarp(shp, crs, raster.warp = TRUE)
		}
		
		dims = stars::st_dimensions(shp)
		rst = attr(dims, "raster")
		
		dim_xy = get_xy_dim(shp)
		dimsxy = dims[names(dim_xy)]
		
		shp2 = stars::st_set_dimensions(shp, rst$dimensions[1], values = 1L:nrow(shp))
		shp3 = stars::st_set_dimensions(shp2, rst$dimensions[2], values = 1L:ncol(shp))
		
		dt = as.data.table(shp3, center = FALSE)
		
		setnames(dt, names(dim_xy)[1], "X__")
		setnames(dt, names(dim_xy)[2], "Y__")
		
		dt[, tmapID__ := as.integer((Y__-1) * nrow(shp) + X__)]
		dt[, X__:= NULL]
		dt[, Y__:= NULL]
		
		m = matrix(NA, nrow = nrow(shp), ncol = ncol(shp))
		
		shp = stars::st_as_stars(list(values = m), dimensions = dimsxy)
		shpclass = "stars"
	}
	
	bbox = sf::st_bbox(shp)
	
	dtcols = setdiff(names(dt), "tmapID__")
	
	structure(list(shp = shp, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name), class = "tmapShape")
}


#' @method tmapShape sf
#' @export
tmapShape.sf = function(shp, is.main, crs, bbox, unit, shp_name) {
	if (!is.null(crs) && sf::st_crs(shp) != crs) {
		shp = sf::st_transform(shp, crs = crs)
	}
	
	sfc = sf::st_geometry(shp)
	dt = as.data.table(sf::st_drop_geometry(shp))
	
	dtcols = copy(names(dt))
	
	bbox = sf::st_bbox(sfc)
	
	dt[, tmapID__ := 1L:nrow(dt)]
	
	structure(list(shp = sfc, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = "sfc", bbox = bbox, unit = unit, shp_name = shp_name), class = "tmapShape")
}
