select_band = function(x, band) {
	nms = stars::st_get_dimension_values(x, "band")
	if (!(band %in% nms)) stop("band not found")
	r = x[, , , which(band == nms), drop = TRUE]
	names(r) = band
	r
}

trim_stars = function(x) {
	dim1 = apply(x[[1]], 1, function(x) !all(is.na(x)))
	dim1 = which(dim1)
	dim1 = dim1[1]:dim1[length(dim1)]
	dim2 = apply(x[[1]], 2, function(x) !all(is.na(x)))
	dim2 = which(dim2)
	dim2 = dim2[1]:dim2[length(dim2)]
	x = x[, dim1, dim2]
	x = sf::st_normalize(x)
	return(x)
}

# from stars package
is_regular_grid = function (x) {
	has_raster(x) && !(has_rotate_or_shear(x) || is_rectilinear(x) ||
					   	is_curvilinear(x))
}

has_raster = function (x) {
	if (inherits(x, "stars"))
		x = stars::st_dimensions(x)
	!is.null(r <- attr(x, "raster")) && all(r$dimensions %in%
												names(x))
}

has_rotate_or_shear = function (x) {
	dimensions = stars::st_dimensions(x)
	if (has_raster(x)) {
		r = attr(dimensions, "raster")
		!anyNA(r$affine) && any(r$affine != 0)
	}
	else FALSE
}

is_curvilinear = function (x) {
	d = stars::st_dimensions(x)
	has_raster(x) && isTRUE(attr(d, "raster")$curvilinear)
}

is_rectilinear = function (x) {
	d = stars::st_dimensions(x)
	if (has_raster(x) && !is_curvilinear(x)) {
		xy = attr(d, "raster")$dimensions
		dimx = d[[xy[1]]]
		dimy = d[[xy[2]]]
		(is.na(dimx$delta) || is.na(dimy$delta)) && (!regular_intervals(dimx$values) ||
													 	!regular_intervals(dimy$values))
	}
	else FALSE
}

regular_intervals = function (x, epsilon = 1e-10) {
	if (length(x) <= 1) {
		FALSE
	} else {
		ud = if (is.atomic(x))
			unique(diff(x))
		else {
			if (identical(tail(x$end, -1), head(x$start, -1)))
				x$end - x$start
			else return(FALSE)
		}
		abs(diff(range(ud))/mean.default(ud)) < epsilon
	}
}

get_downsample = function(dims, px = round(dev.size("px") * (graphics::par("fin")[1] / dev.size()[1]))) {
	floor(sqrt(prod(dims) / prod(px)))
}

# st_downsample = function (x, n, fill_out = TRUE) {
# 	stopifnot(all(n >= 0))
# 	d = dim(x)
# 	n = rep(n, length.out = length(d))
# 	dims = stars::st_dimensions(x)
# 	regular = is_regular_grid(x)
# 	if (!all(n <= 1)) {
# 		args = rep(list(rlang::missing_arg()), length(d) + 1)
# 		for (i in seq_along(d)) {
# 			if (n[i] > 1) {
# 				sq = seq(1, d[i], n[i])
# 				args[[i + 1]] = sq
# 				if (!is.null(dims[[i]]$values))
# 					dims[[i]]$values = dims[[i]]$values[sq]
# 			}
# 		}
# 		x = eval(rlang::expr(x[!!!args]))
# 		if (fill_out && regular) {
# 			d_new = stars::st_dimensions(x)
# 			for (i in seq_along(d)) {
# 				dims[[i]]$delta = dims[[i]]$delta * n[i]
# 				dims[[i]]$from = d_new[[i]]$from
# 				dims[[i]]$to = d_new[[i]]$to
# 			}
# 			x = structure(x, dimensions = dims)
# 		}
# 	}
# 	x
# }

############# other functions


# st_is_merc = function(x) {
# 	crs = sf::st_crs(x)
# 	if (is.na(crs)) {
# 		NA
# 	} else {
# 		isTRUE(crs$proj == "merc") || isTRUE(crs$epsg == 3857)
# 	}
# }

get_xy_dim = function(x) {
	d = stars::st_dimensions(x)
	dxy = attr(d, "raster")$dimensions
	dim(x)[dxy]
}

transwarp = function(x, crs, raster.warp) {
	# NOTE: dropped colors after st_warp fixed in stars 0.4-2
	shpcolors = attr(x[[1]], "colors")
	if (raster.warp) {
		y = tryCatch({
			if (is_curvilinear(x)) {
				sf::st_transform(x, crs = crs)
			} else {
				stars::st_warp(x, crs = crs)
			}
		}, error = function(e) {
			cli::cli_warn(c("!" = "Unable to warp stars. Stars will be transformed now (which will take some time)."))
			tryCatch({
				sf::st_transform(x, crs = crs)
			}, error = function(e) {
				cli::cli_abort("Also unable to transform stars", call = NULL)
			})
		})
	} else {
		y = sf::st_transform(x, crs = crs)
	}

	if (!is.null(shpcolors)) attr(y[[1]], "colors") = shpcolors
	y
}

has_rotate_or_shear = function (x)
{
	dimensions = stars::st_dimensions(x)
	if (has_raster(x)) {
		r = attr(dimensions, "raster")
		!anyNA(r$affine) && any(r$affine != 0)
	}
	else FALSE
}
