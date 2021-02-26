#' @import data.table
tmapShape = function(...) {
	UseMethod("tmapShape")
}

#' @method tmapShape stars
#' @export
tmapShape.stars = function(shp, is.main, crs, bbox, unit, shp_name) {
	if (!has_raster(shp)) {
		dimnms = dimnames(shp)
		
		dimvals = lapply(1:length(dimnms), function(i) st_get_dimension_values(shp, i))
		dimsfc = vapply(dimvals, inherits, what = "sfc", FUN.VALUE = logical(1))
		
		if (!any(dimsfc)) {
			stop("stars object ", shp_name, " is a stars object without raster and doens't have a geometry dimension")
		} else {
			dimid = which(dimsfc)
			geoms = dimvals[[dimid]]
			dimnms_new = dimnms
			dimnms_new[dimid] = "tmapID__"
			shpnames = names(shp)
			shp = st_set_dimensions(shp, dimnms[dimid], values = 1L:length(geoms))
			shp = st_set_dimensions(shp, names = dimnms_new)
		}
		
		dt = as.data.table(shp)
		
		if (sf::st_crs(geoms) != crs) {
			shp = st_transform(shp, crs = crs)
		} else {
			shp = geoms
		}
		
		shpclass = "sfc"
	} else {
		shp = downsample_stars(shp, max.raster = 1e6)
		if (sf::st_crs(shp) != crs) {
			shp = transwarp(shp, crs = crs, raster.warp = TRUE)
		}
		
		dims = st_dimensions(shp)
		rst = attr(dims, "raster")
		
		dim_xy = get_xy_dim(shp)
		dimsxy = dims[names(dim_xy)]
		
		shp2 = st_set_dimensions(shp, rst$dimensions[1], values = 1L:nrow(shp))
		shp3 = st_set_dimensions(shp2, rst$dimensions[2], values = 1L:ncol(shp))
		
		dt = as.data.table(shp3, center = FALSE)
		
		setnames(dt, names(dim_xy)[1], "X__")
		setnames(dt, names(dim_xy)[2], "Y__")
		
		dt[, tmapID__ := as.integer((Y__-1) * nrow(shp) + X__)]
		dt[, X__:= NULL]
		dt[, Y__:= NULL]
		
		m = matrix(NA, nrow = nrow(shp), ncol = ncol(shp))
		
		shp = st_as_stars(list(values = m), dimensions = dimsxy)
		shpclass = "stars"
	}
	
	bbox = st_bbox(shp)
	
	dtcols = setdiff(names(dt), "tmapID__")
	
	structure(list(shp = shp, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name), class = "tmapShape")
}


#' @method tmapShape sf
#' @export
tmapShape.sf = function(shp, is.main, crs, bbox, unit, shp_name) {
	if (sf::st_crs(shp) != crs) {
		shp = st_transform(shp, crs = crs)
	}
	
	sfc = sf::st_geometry(shp)
	dt = as.data.table(sf::st_drop_geometry(shp))
	
	dtcols = copy(names(dt))
	
	bbox = st_bbox(sfc)
	
	dt[, tmapID__ := 1L:nrow(dt)]
	
	structure(list(shp = sfc, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = "sfc", bbox = bbox, unit = unit, shp_name = shp_name), class = "tmapShape")
}
