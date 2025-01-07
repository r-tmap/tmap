#' @export
tmapReproject.sfc = function(shp, tmapID, bbox = NULL, ..., crs) {
	if (is.na(sf::st_crs(shp))) {
		shp2 = shp
		#sf::st_crs(shp2) = crs
		#warning("Setting missing CRS to ", as.character(crs))
	} else {
		shp2 = sf::st_transform(shp, crs)
	}
	if (!is.null(bbox$x)) bbox = list(x = do.call(tmaptools::bb, c(bbox, list(projection = crs))))
	shapeTM(shp2, tmapID, bbox, ...)
}

#' @export
tmapShape.sf = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	if (identical(crs, "auto")) crs = auto_crs(shp, crs_extra = o$crs_extra, crs_global = o$crs_global) else crs = sf::st_crs(crs)
	reproj = (!is.null(crs) && !is.na(crs) && sf::st_crs(shp) != crs)

	if (reproj) {
		if (crs_is_ortho(crs)) {
			tryCatch({
				suppressWarnings({
					shp4326 = sf::st_transform(shp, 4326)
					visible = crs_ortho_visible(crs, projected = FALSE)
					if (!sf::st_is_valid(visible)) visible = sf::st_make_valid(visible)
					shp = suppressMessages(sf::st_intersection(shp4326, visible))
				})
			}, error = function(e) {
				shp
			})
		}
		shp = sf::st_transform(shp, crs = crs)
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
	dt = data.table::as.data.table(sf::st_drop_geometry(shp))

	dtcols = copy(names(dt))


	if (is.null(filter)) filter = rep_len(TRUE, nrow(dt))
	dt[, ':='(tmapID__ = 1L:nrow(dt), sel__ = filter)]

	make_by_vars(dt, tmf, smeta)

	shpTM = shapeTM(shp = sfc, tmapID = 1L:(length(sfc)), bbox = bbox)

	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = "sfc", bbox = bbox, unit = unit, shp_name = shp_name, smeta = smeta), class = "tmapShape")
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
