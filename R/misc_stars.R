select_band <- function(x, band) {
	nms <- stars::st_get_dimension_values(x, "band")
	if (!(band %in% nms)) stop("band not found")
	r <- x[,,,which(band==nms), drop = TRUE]
	names(r) <- band
	r
}

# from stars package
is_regular_grid <- function (x) {
	has_raster(x) && !(has_rotate_or_shear(x) || is_rectilinear(x) || 
					   	is_curvilinear(x))
}

has_raster <- function (x) {
	if (inherits(x, "stars")) 
		x = st_dimensions(x)
	!is.null(r <- attr(x, "raster")) && all(r$dimensions %in% 
												names(x))
}

has_rotate_or_shear <- function (x) {
	dimensions = st_dimensions(x)
	if (has_raster(x)) {
		r = attr(dimensions, "raster")
		!any(is.na(r$affine)) && any(r$affine != 0)
	}
	else FALSE
}

is_curvilinear <- function (x) {
	d = st_dimensions(x)
	has_raster(x) && isTRUE(attr(d, "raster")$curvilinear)
}

is_rectilinear <- function (x) {
	d = st_dimensions(x)
	if (has_raster(x) && !is_curvilinear(x)) {
		xy = attr(d, "raster")$dimensions
		dimx = d[[xy[1]]]
		dimy = d[[xy[2]]]
		(is.na(dimx$delta) || is.na(dimy$delta)) && (!regular_intervals(dimx$values) || 
													 	!regular_intervals(dimy$values))
	}
	else FALSE
}

regular_intervals <- function (x, epsilon = 1e-10) {
	if (length(x) <= 1) 
		FALSE
	else {
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

get_downsample = function(dims, px = dev.size("px")) { 
	floor(sqrt(prod(dims) / prod(px)))
}

st_downsample = function (x, n, fill_out = TRUE) 
{
	stopifnot(all(n >= 0))
	d = dim(x)
	n = rep(n, length.out = length(d))
	dims = st_dimensions(x)
	regular = is_regular_grid(x)
	if (!all(n <= 1)) {
		args = rep(list(rlang::missing_arg()), length(d) + 1)
		for (i in seq_along(d)) if (n[i] > 1) 
			args[[i + 1]] = seq(1, d[i], n[i])
		x = eval(rlang::expr(x[!!!args]))
		if (fill_out && regular) {
			d_new = st_dimensions(x)
			for (i in seq_along(d)) {
				dims[[i]]$delta = dims[[i]]$delta * n[i]
				dims[[i]]$from = d_new[[i]]$from
				dims[[i]]$to = d_new[[i]]$to
			}
			x = structure(x, dimensions = dims)
		}
	}
	x
}



############# other functions


st_is_merc <- function(x) {
	crs = st_crs(x)
	if (is.na(crs)) { 
		NA
	} else {
		isTRUE(crs$proj == "merc") || isTRUE(crs$epsg == 3857)
	}
}

get_xy_dim <- function(x) {
	d = st_dimensions(x)
	dxy = attr(d, "raster")$dimensions
	dim(x)[dxy]
}

downsample_stars <- function(x, max.raster) {
	xy_dim <- get_xy_dim(x)
	asp <- xy_dim[1] / xy_dim[2]
	
	y_new <- sqrt(max.raster / asp)
	x_new <- y_new * asp
	
	downsample <- xy_dim[1] / x_new
	
	
	if (inherits(x, "stars_proxy")) {
		y <- st_as_stars(x, downsample = downsample - 1) # downsample is number of pixels to skip, instead of multiplier
		message("stars_proxy object shown at ", paste(get_xy_dim(y), collapse = " by "), " cells.")
	} else if (prod(xy_dim) > max.raster) {
		n <- dim(x) * 0 + 1
		n[names(xy_dim)] <- downsample
		y <- st_downsample(x, n)
		message("stars object downsampled to ", paste(get_xy_dim(y), collapse = " by "), " cells. See tm_shape manual (argument raster.downsample)")
	} else {
		y <- x
	}
	y
}

transwarp <- function(x, crs, raster.warp) {
	# NOTE: dropped colors after st_warp fixed in stars 0.4-2
	shpcolors <- attr(x[[1]], "colors")
	if (raster.warp) {
		y <- tryCatch({
			stars::st_warp(x, crs = crs)
		}, error = function(e) {
			stop("Unable to warp stars. You could try with raster.warp = FALSE (argument of tm_shape)", call. = FALSE)	
		})
	} else {
		y <- tryCatch({
			sf::st_transform(x, crs = crs)
		}, error = function(e) {
			stop("Unable to transform stars", call. = FALSE)	
		})
	}
	if (!is.null(shpcolors)) attr(y[[1]], "colors") <- shpcolors
	y
}
