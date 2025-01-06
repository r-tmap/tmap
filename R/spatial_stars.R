#' @export
tmapReproject.stars = function(shp, tmapID, bbox = NULL, ..., crs) {
	shp[[1]] = tmapID

	args = list(...)
	raster.warp = if ("raster.warp" %in% names(args)) args$raster.warp else TRUE

	shp2 = transwarp(shp, crs, raster.warp = raster.warp)
	tmapID2 = shp2[[1]]
	shp2[[1]] = NA

	if (!is.null(bbox$x)) bbox = list(x = do.call(tmaptools::bb, c(bbox, list(projection = crs))))

	shapeTM(shp2, tmapID2, bbox, ...)
}

#' @export
tmapReproject.dimensions = function(shp, tmapID, bbox = NULL, ..., crs) {
	s = structure(list(tmapID = matrix(tmapID, nrow = nrow(shp))), dimensions = shp, class = "stars")

	args = list(...)
	raster.warp = if ("raster.warp" %in% names(args)) args$raster.warp else TRUE

	if (is.na(sf::st_crs(shp))) {
		shp2 = s
	} else {
		shp2 = transwarp(s, crs, raster.warp = raster.warp)
	}

	tmapID2 = shp2[[1]]

	d2 = stars::st_dimensions(shp2)
	if (!is.null(bbox$x)) bbox = list(x = do.call(tmaptools::bb, c(bbox, list(projection = crs))))

	shapeTM(d2, tmapID2, bbox, ...)
}


#' @export
tmapShape.stars = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	if (identical(crs, "auto")) crs = auto_crs(shp, crs_extra = o$crs_extra, crs_global = o$crs_global)

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
			dimvls = lapply(dimcols, function(d) stars::st_get_dimension_values(shp, d))
			shpnames = names(shp)
			shp = stars::st_set_dimensions(shp, dimnms[dimid], values = seq_along(geoms))
			shp = stars::st_set_dimensions(shp, names = dimnms_new)
		}

		dt = as.data.table(shp)

		if (!is.null(crs) && sf::st_crs(geoms) != crs) {
			shp = sf::st_transform(shp, crs)
		} else {
			shp = geoms
		}
		shpTM = shapeTM(shp = shp, tmapID = seq_along(shp), bbox = bbox)


		shpclass = "sfc"
	} else {
		shp = downsample_stars(shp, max.raster = max_cells(o$raster.max_cells) / (o$fn[1] * o$fn[2]))
		if (!is.null(crs) && sf::st_crs(shp) != crs) {
			shp = transwarp(shp, crs, raster.warp = o$raster.warp)
		}

		dims = stars::st_dimensions(shp)
		rst = attr(dims, "raster")

		dim_xy = get_xy_dim(shp)
		dimsxy = dims[names(dim_xy)]

		if (rst$curvilinear) {
			shp3 = shp
			attr(shp3, "dimensions")[[rst$dimensions[1]]]$values = 1L:nrow(shp)
			attr(shp3, "dimensions")[[rst$dimensions[2]]]$values = 1L:ncol(shp)
			attr(attr(shp3, "dimensions"), "raster")$curvilinear = FALSE
		} else {
			shp2 = stars::st_set_dimensions(shp, rst$dimensions[1], values = 1L:nrow(shp))
			shp3 = stars::st_set_dimensions(shp2, rst$dimensions[2], values = 1L:ncol(shp))
		}


		dt = as.data.table(shp3, center = FALSE)

		if (!all(names(shp3) %in% names(dt))) {
			subst_names = tail(names(dt), length(shp3))
			for (i in 1L:length(shp3)) {
				setnames(dt, subst_names[i], names(shp3)[i])
			}
		}

		setnames(dt, names(dim_xy)[1], "X__")
		setnames(dt, names(dim_xy)[2], "Y__")

		dt[, tmapID__ := as.integer((Y__-1) * nrow(shp) + X__)]
		dt[, X__:= NULL]
		dt[, Y__:= NULL]

		data.table::setorder(dt, cols = "tmapID__")

		shp = dimsxy
		shpclass = "stars"


		shpTM = shapeTM(shp = shp, tmapID = 1L:(nrow(shp) * ncol(shp)), bbox = bbox)

	}

	make_by_vars(dt, tmf, smeta)


	dtcols = setdiff(names(dt), "tmapID__")

	filter = if (is.null(filter)) {
		rep(TRUE, nrow(dt))
	} else filter[dt$tmapID__]
	dt[, ':='(sel__ = filter)] # tmapID__ = 1L:nrow(dt),

	for (nm in dtcols) {
		if (is.factor(dt[[nm]])) {
			lvls = levels(dt[[nm]])
			cols = attr(dt[[nm]], "colors")
			ids = seq_along(lvls)[lvls!="NA"]
			if (!is.null(cols)) {
				#if (any(lvls=="NA")) {
				dt[[nm]] = factor(as.integer(dt[[nm]]), levels = ids, labels = paste(lvls[ids], cols[ids], sep = "=<>="))
				#}
				#dt[[nm]] = factor()
			}
		}
	}

	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name, smeta = smeta), class = "tmapShape")
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
tmapGetShapeMeta2.stars = function(shp, smeta, o) {
	smeta$vars_levs = lapply(seq_len(length(shp)), function(i) {
		get_fact_levels_na(shp[[i]], o)
	})
	names(smeta$vars_levs) = names(shp)
	smeta
}

