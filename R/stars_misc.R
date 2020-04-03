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

############# other functions


st_is_merc <- function(x) {
	crs = st_crs(x)
	if (is.na(crs)) { 
		NA
	} else {
		isTRUE(crs$proj == "merc") || isTRUE(crs$epsg == 3857)
	}
}

# temp solution to https://github.com/r-spatial/mapview/issues/256
# cut_world_edges <- function(x) {
# 	if (!sf::st_is_longlat(x) || is_curvilinear(x)) {
# 		x
# 	} else {
# 		dims <- attr(st_dimensions(x), "raster")$dimensions
# 
# 		xvalues <- round(st_get_dimension_values(x, dims[1]))
# 		yvalues <- round(st_get_dimension_values(x, dims[2]))
# 		
# 		xminid <- which(xvalues > -180)[1]
# 		xmaxid <- tail(which(xvalues < 180), 1)
# 		
# 		yminid <- which(yvalues < 90)[1]
# 		ymaxid <- tail(which(yvalues > -90), 1)
# 		x[,xminid:xmaxid, yminid:ymaxid]
# 	}
# }


