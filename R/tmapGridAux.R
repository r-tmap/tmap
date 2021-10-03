findZoom = function(b) {
	## calculate zoom level	
	# borrowed from https://github.com/dkahle/ggmap/blob/master/R/calc_zoom.r
	lon_diff = b[3] - b[1]
	lat_diff = b[4] - b[2]
	
	zoomlon = ceiling(log2(360 * 2/lon_diff))
	zoomlat = ceiling(log2(180 * 2/lat_diff))
	zoom = as.integer(min(zoomlon, zoomlat))
}

tmapGridBasemapPrep = function(a, bs, o) {
	g = get("g", envir = .TMAP_GRID)
	
	if (!requireNamespace("maptiles")) stop("maptiles package is required", call. = FALSE)
	
	
	crs = sf::st_crs(bs[[1]])
	
	isproj = !sf::st_is_longlat(crs)
	
	if (isproj) {
		bs_orig = bs
		bs = lapply(bs, function(b) {
			sf::st_bbox(sf::st_transform(sf::st_as_sfc(b), crs = "EPSG:4326"))
		})
	}
	
	bs = lapply(bs, function(b) {
		bb_ll_valid(bb_asp(b, g$fasp))
	})
	
	
	zs = vapply(bs, findZoom, FUN.VALUE = integer(1))

	xs = mapply(function(b, z) {

		tryCatch({
			maptiles::get_tiles(x = b, provider = a$server, zoom = z, crop = FALSE)	
		}, error = function(e) {
			tryCatch({
				maptiles::get_tiles(x = b, provider = a$server, zoom = z - 1, crop = FALSE)	
			}, error = function(e) {
				NULL
			})
		})
	}, bs, zs, SIMPLIFY = FALSE)

	if (isproj) xs = mapply(function(x,b) {
		ex = terra::ext(as.vector(b[c(1,3,2,4)]))
		asp = (ex[2] - ex[1]) / (ex[4] - ex[3])
		
		tot = terra::ncell(x) * 2
		
		nc = round(sqrt(tot * asp))
		nr = round(tot / nc)

		r = terra::rast(ex, nrows = nr, ncols = nc, crs = crs$wkt)
		terra::project(x, r, method = "near")
	}, xs, bs_orig, SIMPLIFY = FALSE)
	
	
	ss = lapply(xs, function(x) {
		if (is.null(x)) NULL else do.call(tmapShape, list(shp = x, is.main = FALSE, crs = crs, bbox = NULL, unit=NULL, filter=NULL, shp_name = "x", o = o))
	})
	
	srgb = tm_scale_rgb(maxValue = 255, value.na = "#FFFFFF")
	
	
	ds = lapply(ss, function(s) {
		if (is.null(s)) return(NULL)
		d = s$dt
		d[, c("col", "ord", "legnr") := do.call(srgb$FUN, list(x1 = red, x2 = green, x3 = blue, scale = srgb, legend = list(), opt = o, aes = "col", layer = "raster", sortRev = NA))]
		d[, col_alpha:=1L]
		d
	})
	#d[, c("col", "legnr") := do.call(srgb$FUN, c(unname(.SD), list(scale = srgb, legend = list(), opt = o, aes = "fill", layer = "raster", sortRev = NA))), .SDcols = c("red", "green", "blue")]
	

	shpTMs = lapply(ss, function(s) {
		if (is.null(s)) NULL else s$shpTM	
	})


	
	g$bmaps_shpTHs = shpTMs
	g$bmaps_dts = ds

	assign("g", g, envir = .TMAP_GRID)
	
}

tmapGridGridPrep = function(a, b, o) {
}


tmapGridBasemap = function(bi, bbx, facet_row, facet_col, facet_page, id, o) {
	g = get("g", envir = .TMAP_GRID)

	dt = g$bmaps_dts[[bi]]
	shpTM = g$bmaps_shpTHs[[bi]]
	gp = list()
	
	if (!is.null(dt)) tmapGridRaster(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, o)	
}

tmapGridGrid = function(a, b) {
	
}
