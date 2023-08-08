tmapSubsetShp = function(...) {
	UseMethod("tmapSubsetShp")
}

#' @method tmapSubsetShp stars
#' @export
tmapSubsetShp.stars = function(shp, vars) {
	ids = unique(c(which(names(shp) %in% vars),
				   which(names(shp) %in% vars)))
	shp2 = shp[ids]
	if (!length(vars)) {
		shp2$dummy__ = TRUE
	}
	shp2
}

#' @method tmapSubsetShp SpatRaster
#' @export
tmapSubsetShp.SpatRaster = function(shp, vars) {
	#v = setdiff(vars, names(shp))
	#if (length(v)) stop("Variable(s) not found: \"", paste(v, collapse = "\",\""), "\"")
	if (!length(vars)) {
		terra::rast(extent = terra::ext(shp), crs = terra::crs(shp), vals = TRUE, names = "dummy__")
	} else {
		shp[[vars]]
	}
}

#' @method tmapSubsetShp SpatRaster
#' @export
tmapSubsetShp.Raster = function(shp, vars) {
	tmapSubsetShp.SpatRaster(terra::rast(shp), vars)
}

#' @method tmapSubsetShp SpatVector
#' @export
tmapSubsetShp.SpatVector = function(shp, vars) {
	if ("AREA" %in% vars && !("AREA" %in% names(shp))) {
		shp$AREA = terra::expanse(shp)
	}
	tmapSubsetShp.sf(shp, vars)
}


#' @method tmapSubsetShp sf
#' @export
tmapSubsetShp.sf = function(shp, vars) {
	if ("AREA" %in% vars && !("AREA" %in% names(shp))) {
		shp$AREA = sf::st_area(shp)
	}
	if ("MAP_COLORS" %in% vars) {
		shp$MAP_COLORS = tmaptools::map_coloring(shp)
	}
	if (!length(vars)) {
		vars = "dummy__"
		shp$dummy__ = TRUE
	}
	shp[, vars]
}

#' @method tmapSubsetShp sfc
#' @export
tmapSubsetShp.sfc = function(shp, vars) {
	s = sf::st_sf(dummy__ = TRUE, geometry = shp)
	if ("AREA" %in% vars) {
		s$AREA = sf::st_area(shp)
	}
	if ("MAP_COLORS" %in% vars) {
		s$MAP_COLORS = tmaptools::map_coloring(s)
	}
	s
}
