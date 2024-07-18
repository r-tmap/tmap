#' Internal method that subsets data from shape objects
#' 
#' Internal method that subsets data from shape objects
#' 
#' @param shp shape
#' @param vars a vector of variable names
#' @export
#' @keywords internal
tmapSubsetShp = function(shp, vars) {
	UseMethod("tmapSubsetShp")
}

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

#' @export
tmapSubsetShp.Raster = function(shp, vars) {
	tmapSubsetShp.SpatRaster(terra::rast(shp), vars)
}

#' @export
tmapSubsetShp.Spatial = function(shp, vars) {
	tmapSubsetShp.sf(as(shp, "sf"), vars)
}


#' @export
tmapSubsetShp.SpatVector = function(shp, vars) {
	if ("AREA" %in% vars && !("AREA" %in% names(shp))) {
		shp$AREA = terra::expanse(shp)
	}
	if ("LENGTH" %in% vars && !("LENGTH" %in% names(shp))) {
		shp$LENGTH = terra::perim(shp)
	}
	tmapSubsetShp.sf(shp, vars)
}


#' @export
tmapSubsetShp.sf = function(shp, vars) {
	
	if ("AREA" %in% vars && !("AREA" %in% names(shp))) {
		shp$AREA = sf::st_area(shp)
	}
	if ("LENGTH" %in% vars && !("LENGTH" %in% names(shp))) {
		shp$LENGTH = sf::st_length(shp)
	}
	if ("MAP_COLORS" %in% vars) {
		shp$MAP_COLORS = as.factor(tmaptools::map_coloring(shp))
	}

	if (!length(vars)) {
		vars = "dummy__"
		shp$dummy__ = logical(nrow(shp))
	}
	shp[, vars]
}


#' @export
tmapSubsetShp.sfc = function(shp, vars) {
	s = sf::st_sf(dummy__ = TRUE, geometry = shp)
	if ("AREA" %in% vars) {
		s$AREA = sf::st_area(shp)
	}
	if ("LENGTH" %in% vars) {
		s$LENGTH = sf::st_length(shp)
	}
	if ("MAP_COLORS" %in% vars) {
		s$MAP_COLORS = tmaptools::map_coloring(s)
	}
	s
}
