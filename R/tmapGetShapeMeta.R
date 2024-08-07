#' Internal method that extracts meta data from shape objects
#' 
#' Internal method that extracts meta data from shape objects
#'
#' @param shp the shape object
#' @param o the list of options
#' @export
#' @keywords internal
tmapGetShapeMeta1 = function(shp, o) {
	UseMethod("tmapGetShapeMeta1")
}

#' Internal method that extracts more meta data from shape objects
#' 
#' Internal method that extracts meta data from shape objects
#'
#' @param shp the shape
#' @param smeta meta (from tmapGetShapeMeta1)
#' @param o the list of options
#' @export
#' @keywords internal
tmapGetShapeMeta2 = function(shp, smeta, o) {
	UseMethod("tmapGetShapeMeta2")
}


#' @export
tmapGetShapeMeta1.stars = function(shp, o) {
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
	
	vars_orig = names(shp)
	vars = vars_orig #make.names(vars_orig)
	names(vars) = vars_orig
	list(vars = vars,
		 dims = dims, 
		 dims_vals = dims_vals)
}

#' @export
tmapGetShapeMeta1.Raster = function(shp, o) {
	tmapGetShapeMeta1.SpatRaster(terra::rast(shp), o)
}

#' @export
tmapGetShapeMeta1.Spatial = function(shp, o) {
	tmapGetShapeMeta1.SpatRaster(as(shp, "sf"), o)
}

#' @export
tmapGetShapeMeta2.stars = function(shp, smeta, o) {
	smeta$vars_levs = lapply(seq_len(length(shp)), function(i) {
		get_fact_levels_na(shp[[i]], o)
	})
	names(smeta$vars_levs) = names(shp)
	smeta
}


#' @export
tmapGetShapeMeta2.SpatRaster = function(shp, smeta, o) {
	if (terra::ncell(shp) > o$raster.max.cells) {
		# NOTE: this option is not ideal, because categories may be undiscovered
		# NOTE2: maybe the same should be done with large stars?
		shp = terra::spatSample(shp, 1e5, method = "regular", as.raster = TRUE)
	}
	smeta$vars_levs = lapply(terra::values(shp, dataframe=TRUE), function(dat) {
		get_fact_levels_na(dat, o)
	})		

	names(smeta$vars_levs) = names(shp)
	smeta
}

#' @export
tmapGetShapeMeta2.SpatVector = function(shp, smeta, o) {
	
	# slow, needs to be improved with terra functions, e.g. unique and levels
	smeta$vars_levs = lapply(values(shp), function(dat) {
		get_fact_levels_na(dat, o)
	})
	names(smeta$vars_levs) = names(shp)
	smeta
}


#' @export
tmapGetShapeMeta2.sf = function(shp, smeta, o) {
	vars = setdiff(names(shp), attr(shp, "sf_column"))
	smeta$vars_levs = lapply(seq_along(vars), function(i) {
		get_fact_levels_na(shp[[i]], o)
	})
	names(smeta$vars_levs) = vars
	smeta
}


#' @export
tmapGetShapeMeta1.sf = function(shp, o) {
	vars = setdiff(names(shp), attr(shp, "sf_column"))
	names(vars) = vars
	#vars_levs = lapply(vars, function(v) {get_fact_levels_na(shp[[v]], o)})
	dims = character(0)
	dims_vals = list()
	
	list(vars = vars,
		 dims = dims, 
		 dims_vals = dims_vals)
}


#' @export
tmapGetShapeMeta2.sfc = function(shp, smeta, o) {
	vars = character(0)
	smeta$vars_levs = list()
	smeta
}


#' @export
tmapGetShapeMeta1.sfc = function(shp, o) {
	vars = character(0)
	dims = character(0)
	dims_vals = list()
	
	list(vars = vars,
		 dims = dims, 
		 dims_vals = dims_vals)
}





#' @export
tmapGetShapeMeta1.SpatRaster = function(shp, o) {
	vars = names(shp)
	names(vars) = vars
	
	dims = character(0)
	dims_vals = list()
	
	list(vars = vars,
		 dims = dims, 
		 dims_vals = dims_vals)
	
}

#' @export
tmapGetShapeMeta1.SpatVector = function(shp, o) {
	vars = names(shp)
	names(vars) = vars
	
	dims = character(0)
	dims_vals = list()
	
	list(vars = vars,
		 dims = dims, 
		 dims_vals = dims_vals)
	
}


#' Internal tmap function that gets factor levels with NA's
#' 
#' Internal tmap function that gets factor levels with NA's
#'
#' @param x vector
#' @param o options
#' @export
#' @keywords internal
get_fact_levels_na = function(x, o) {
	if (inherits(x, "sfc") || is.list(x)) {
		levs = NULL
	} else if (is.factor(x)) {
		if (o$drop.empty.facets) {
			tab = tabulate(x, nbins = nlevels(x))
			anyna = (sum(tab) != length(x)) # note that NA can already be included in the levels (in that case anyna = FALSE)
			levs = levels(x)[tab != 0]
		} else {
			anyna = anyNA(x)
			levs = levels(x)
		}

		if (!o$drop.NA.facets && anyna) {
			showNA = TRUE
			levs = c(levs, o$na.text)
		} else if (!o$drop.NA.facets && any(is.na(levs))) {
			showNA = TRUE
			levs[is.na(levs)] = o$na.text
		} else if (o$drop.NA.facets && any(is.na(levs))) {
			showNA = FALSE
			levs = levs[!is.na(levs)]
		} else {
			showNA = FALSE
		}
	} else {
		u = unique(as.vector(x))
		if (length(u) > o$facet.max) {
			levs = NULL
		} else {
			levs = as.character(sort(u))
			if (!o$drop.NA.facets && any(is.na(u))) {
				showNA = TRUE
				levs = c(levs, o$na.text)
			} else {
				showNA = FALSE
			}
		}
	} 
	if (!is.null(levs)) attr(levs, "showNA") = showNA
	levs
}
