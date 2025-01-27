#' @export
tmapShape.SpatRaster = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	#tmapShape.stars(stars::st_as_stars(shp), is.main, crs, bbox, unit, filter, shp_name)
	rlang::check_installed("terra")

	shp = downsample_SpatRaster(shp, max.raster = max_cells(o$raster.max_cells) / (o$fn[1] * o$fn[2]))


	ctabs = terra::coltab(shp)
	cats = terra::levels(shp)

	minmax = as.list(as.data.frame(terra::minmax(shp, compute = all(!terra::hasMinMax(shp)))))

	dt = data.table::setDT(terra::as.data.frame(shp, na.rm=FALSE))
	dt[, tmapID__:=1L:nrow(dt)]
	#dt = data.table::melt(dt, id.vars = "tmapID__", variable.name = "layer", value.name = "value")

	xy_dim = dim(shp)[1:2]
	b = sf::st_bbox(shp)

	crs = sf::st_crs(shp)

	shp = structure(list(x = structure(list(from = 1, to = xy_dim[2], offset = b[1], delta = (b[3] - b[1]) / xy_dim[2], refsys = crs, point = FALSE, values = NULL), class = "dimension"),
						 y = structure(list(from = 1, to = xy_dim[1], offset = b[4], delta = (b[2] - b[4]) / xy_dim[1], refsys = crs, point = FALSE, values = NULL), class = "dimension")), class = "dimensions")
	attr(shp, "raster") = structure(list(affine = c(0, 0), dimensions = c("x", "y"), curvilinear = FALSE), class = "stars_raster")


	shpclass = "stars"

	dtcols = setdiff(names(dt), "tmapID__")

	names(ctabs) = dtcols
	names(cats) = dtcols
	names(minmax) = dtcols

	if (!is.null(tmf)) make_by_vars(dt, tmf, smeta)


	if (is.null(filter)) filter = rep(TRUE, nrow(dt))
	dt[, ':='(sel__ = filter)] # tmapID__ = 1L:nrow(dt),

	shpTM = shapeTM(shp = shp, tmapID = 1L:(nrow(shp) * ncol(shp)), bbox = bbox)

	for (nm in dtcols) {
		if (!is.null(ctabs[[nm]])) {
			# in case of predefined colors: if categories are associated (cats), use tm_scale_categorical, otherwise tm_

			ct = ctabs[[nm]]
			lt = cats[[nm]]
			if (is.factor(dt[[nm]])) {
				#levels(dt[[nm]])

				ids = match(lt[match(levels(dt[[nm]]), lt[,2]), 1], ct$value)
				cti = ct[ids,]

				cls = rgb(cti$red, cti$green, cti$blue, cti$alpha, maxColorValue = 255)

				levels(dt[[nm]]) = paste(levels(dt[[nm]]), cls, sep = "=<>=")
			} else if ("value" %in% names(ct)) {
				cls = rgb(ct$red, ct$green, ct$blue, ct$alpha, maxColorValue = 255)

				rng = minmax[[nm]]
				sel = ct$value %in% seq(rng[1], rng[2])

				#until = anyDuplicated(cls) - 1L

				cls = cls[sel]
				ct = ct[sel, ]
				dt[[nm]] = factor(dt[[nm]], levels = ct$value, labels = paste(ct$value, cls, sep = "=><="))
			}

		}
	}

	structure(list(shpTM = shpTM, dt = dt, is.main = is.main, dtcols = dtcols, shpclass = shpclass, bbox = bbox, unit = unit, shp_name = shp_name, smeta = smeta), class = "tmapShape")

}

#' @export
tmapShape.SpatVector = function(shp, is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf) {
	tmapShape.sf(sf::st_as_sf(shp), is.main, crs, bbox, unit, filter, shp_name, smeta, o, tmf)
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
tmapGetShapeMeta2.SpatRaster = function(shp, smeta, o) {
	if (terra::ncell(shp) > max_cells(o$raster.max_cells)) {
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


