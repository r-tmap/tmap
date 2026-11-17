#' @export
tmapReproject.sfc = function(shp, tmapID, bbox = NULL, ..., crs) {
	props = list(...)
	if (is.na(sf::st_crs(shp))) {
		shp2 = shp
	} else {
		crs0 = sf::st_crs(shp)
		tmapID0 = tmapID
		res = transform_ortho(shp, crs, tmapID)
		shp2 = res$shp
		tmapID = res$tmapID
		props = reproject_props(props, crs0 = crs0, crs = crs, tmapID0 = tmapID0, tmapID = tmapID)
	}

	if (!is.null(bbox$x)) {
		if (identical(bbox$x, "FULL")) {
			bbox = list(x = full_bbox(crs))
		} else {
			bbox = list(x = do.call(tmaptools::bb, c(bbox, list(projection = crs))))
		}
	}
	do.call(shapeTM, c(list(shp = shp2, tmapID = tmapID, bbox = bbox), props))
}

# The prop_ vectors (see tmapTransCentroid) are determined in the crs of step 3. When the plotting crs (step 4)
# is different, e.g. because of a basemap, the polygons and radii of label placement are reprojected as well,
# and features that are removed by the reprojection (orthographic projections) are removed from the prop_ vectors.
reproject_props = function(props, crs0, crs, tmapID0, tmapID) {
	isp = substr(names(props), 1, 5) == "prop_"
	if (!any(isp)) return(props)
	n0 = length(tmapID0)
	if (!identical(tmapID, tmapID0)) {
		idx = match(tmapID, tmapID0)
		for (p in names(props)[isp]) {
			if (length(props[[p]]) == n0) props[[p]] = props[[p]][idx]
		}
	}
	if (!is.null(props$prop_poly)) {
		ip = which(!vapply(props$prop_poly, is.null, logical(1)))
		if (length(ip)) {
			polys = sf::st_sfc(props$prop_poly[ip], crs = crs0)
			polys2 = tryCatch(suppressWarnings(sf::st_transform(polys, crs)), error = function(e) NULL)
			if (is.null(polys2)) {
				props$prop_poly[ip] = list(NULL)
			} else {
				props$prop_poly[ip] = lapply(polys2, function(p) p)
				if (!is.null(props$prop_poly_r)) {
					# local scale factor of the reprojection
					a = suppressWarnings(sf::st_point_on_surface(polys))
					r = props$prop_poly_r[ip]
					co = sf::st_coordinates(a)[, 1:2, drop = FALSE]
					dx = sf::st_sfc(lapply(seq_len(nrow(co)), function(i) sf::st_point(co[i, ] + c(r[i], 0))), crs = crs0)
					dy = sf::st_sfc(lapply(seq_len(nrow(co)), function(i) sf::st_point(co[i, ] + c(0, r[i]))), crs = crs0)
					tr = function(x) sf::st_coordinates(suppressWarnings(sf::st_transform(x, crs)))[, 1:2, drop = FALSE]
					co2 = tr(a)
					rx = sqrt(rowSums((tr(dx) - co2)^2))
					ry = sqrt(rowSums((tr(dy) - co2)^2))
					r2 = pmin(rx, ry)
					r2[!is.finite(r2)] = 0
					props$prop_poly_r[ip] = r2
				}
			}
		}
	}
	props
}

#' @export
tmapShape.sf = function(shp, is.main, crs, bbox, unit, filter, layer, shp_name, smeta, o, tmf) {
	if (identical(crs, "auto")) crs = auto_crs(shp, crs_extra = o$crs_extra, crs_global = o$crs_global) else crs = sf::st_crs(crs)
	reproj = (!is.null(crs) && !is.na(crs) && sf::st_crs(shp) != crs)

	if (reproj) {
		shp = transform_ortho(shp, crs)
	}

	sfc = sf::st_geometry(shp)

	if (inherits(sfc[[1]], c("XYZ", "XYM", "XYZM"))) {
		sfc = sf::st_zm(sfc)
	}

	if (o$check_and_fix) sfc = check_fix(sfc, shp_name, reproj, o$show.messages)

	# if check_fix fails, is_valid contains the valid ids
	isv = attr(sfc, "is_valid")
	if (!is.null(isv)) {
		shp = shp[isv, ]
	}
	df = sf::st_drop_geometry(shp)

	# Encode expression columns (e.g. created with parse()) as character strings
	# so they survive the data.table-based data pipeline. They are decoded again
	# at plot time (tm_text / tm_labels) to render as plotmath expressions.
	is_expr = vapply(df, is.expression, FUN.VALUE = logical(1))
	if (any(is_expr)) {
		df[is_expr] = lapply(df[is_expr], encode_expr)
	}

	dt = data.table::as.data.table(df)

	dtcols = copy(names(dt))


	if (is.null(filter)) filter = rep_len(TRUE, nrow(dt))
	dt[, ':='(tmapID__ = 1L:.N, sel__ = filter)]

	make_by_vars(dt, tmf, smeta)

	shpTM = shapeTM(shp = sfc, tmapID = 1L:(length(sfc)), bbox = bbox)

	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = "sfc", bbox = bbox, unit = unit, shp_name = shp_name, smeta = smeta, type_ids = NULL, type_vars = NULL), class = "tmapShape")
}

#' @export
tmapSubsetShp.sf = function(shp, vars) {

	if ("AREA_" %in% vars && !("AREA_" %in% names(shp))) {
		shp$AREA_ = sf::st_area(shp)
	}
	if ("LENGTH_" %in% vars && !("LENGTH_" %in% names(shp))) {
		shp$LENGTH_ = sf::st_length(shp)

	}
	if ("MAP_COLORS" %in% vars) {
		shp$MAP_COLORS = as.factor(do.call(tmaptools::map_coloring, c(list(shp), map_colors_spec_get())))
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
		s$AREA_ = sf::st_area(shp)
	}
	if ("LENGTH" %in% vars) {
		s$LENGTH_ = sf::st_length(shp)
	}
	if ("MAP_COLORS" %in% vars) {
		s$MAP_COLORS = as.factor(do.call(tmaptools::map_coloring, c(list(shp), map_colors_spec_get())))
	}
	s
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
tmapGetShapeMeta1.sf = function(shp, layer, o) {
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
tmapGetShapeMeta1.sfc = function(shp, layer, o) {
	vars = character(0)
	dims = character(0)
	dims_vals = list()

	list(vars = vars,
		 dims = dims,
		 dims_vals = dims_vals)
}
