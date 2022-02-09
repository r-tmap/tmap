tmapGetShapeMeta1 = function(...) {
	UseMethod("tmapGetShapeMeta1")
}

tmapGetShapeMeta2 = function(...) {
	UseMethod("tmapGetShapeMeta2")
}


#' @method tmapGetShapeMeta1 stars
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
	vars = make.names(vars_orig)
	names(vars) = vars_orig
	list(vars = vars,
		 dims = dims, 
		 dims_vals = dims_vals)
}


#' @method tmapGetShapeMeta2 stars
#' @export
tmapGetShapeMeta2.stars = function(shp, smeta, o) {
	smeta$vars_levs = lapply(seq_len(length(shp)), function(i) {
		get_fact_levels_na(shp[[i]], o)
	})
	names(smeta$vars_levs) = names(shp)
	smeta
}


#' @method tmapGetShapeMeta2 SpatRaster
#' @export
tmapGetShapeMeta2.SpatRaster = function(shp, smeta, o) {
	
	# slow, needs to be improved with terra functions, e.g. unique and levels
	smeta$vars_levs = lapply(values(shp, dataframe=TRUE), function(dat) {
		get_fact_levels_na(dat, o)
	})
	names(smeta$vars_levs) = names(shp)
	smeta
}

#' @method tmapGetShapeMeta2 sf
#' @export
tmapGetShapeMeta2.sf = function(shp, smeta, o) {
	vars = setdiff(names(shp), attr(shp, "sf_column"))
	smeta$vars_levs = lapply(seq_along(vars), function(i) {
		get_fact_levels_na(shp[[i]], o)
	})
	names(smeta$vars_levs) = vars
	smeta
}


#' @method tmapGetShapeMeta1 sf
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


#' @method tmapGetShapeMeta2 sfc
#' @export
tmapGetShapeMeta2.sfc = function(shp, smeta, o) {
	vars = character(0)
	smeta$vars_levs = list()
	smeta
}


#' @method tmapGetShapeMeta1 sfc
#' @export
tmapGetShapeMeta1.sfc = function(shp, o) {
	vars = character(0)
	dims = character(0)
	dims_vals = list()
	
	list(vars = vars,
		 dims = dims, 
		 dims_vals = dims_vals)
}





#' @method tmapGetShapeMeta1 SpatRaster
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


x = factor(sample(letters[1:4], 1e7, replace = TRUE), levels = letters[1:5])
x[1:10000] = NA
x = addNA(x)

get_fact_levels_na = function(x, o) {
	if (is.factor(x)) {
		if (o$drop.empty.facets) {
			tab = tabulate(x, nbins = nlevels(x))
			anyna = (sum(tab) != length(x)) # note that NA can already be included in the levels (in that case anyna = FALSE)
			levs = levels(x)[tab != 0]
		} else {
			anyna = any(is.na(x))
			levs = levels(x)
		}

		if (!o$drop.NA.facets && anyna) {
			showNA = TRUE
			levs = c(levs, o$label.na)
		} else if (!o$drop.NA.facets && any(is.na(levs))) {
			showNA = TRUE
			levs[is.na(levs)] = o$label.na
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
				levs = c(levs, o$label.na)
			} else {
				showNA = FALSE
			}
		}
	} 
	if (!is.null(levs)) attr(levs, "showNA") = showNA
	levs
}