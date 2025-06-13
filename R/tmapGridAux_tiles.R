
#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPrepare.tm_aux_basemap = function(a, bs, id, o) {
	tmapGridAuxPrepare.tm_aux_tiles(a, bs, id, o)
}

#' @export
#' @keywords internal
#' @rdname tmapGridLeaflet
tmapGridAuxPrepare.tm_aux_tiles = function(a, bs, id, o) {
	g = get("g", envir = .TMAP_GRID)
	rlang::check_installed("maptiles")

	crs = sf::st_crs(bs[[1]])

	crs3857 = (crs == 3857 || crs == st_crs(3857))

	isproj = !sf::st_is_longlat(crs)

	if (isproj) {
		# plain lat-lon to find zoom levels
		bs_orig = bs
		bs = lapply(bs, function(b) {
			sf::st_bbox(sf::st_transform(sf::st_as_sfc(b), crs = "EPSG:4326"))
		})
	}

	# tiles are in mercator
	bs3857 = lapply(bs, sf::st_transform, crs = "EPSG:3857")

	bs = lapply(bs, function(b) {
		# not sure why needed
		bb_ll_valid(bb_asp(b, g$fasp))
	})

	if (is.na(a$zoom)) {
		zs = vapply(bs, findZoom, FUN.VALUE = integer(1))
	} else {
		zs = rep(a$zoom, length(bs))
	}

	serv = a$server[1]

	if (substr(serv, 1, 4) == "http") {
		if (serv %in% .TMAP_GRID$maptiles_urls) {
			tile_id = which(serv == .TMAP_GRID$maptiles_urls)[1]
		} else {
			tile_id = length(.TMAP_GRID$maptiles_urls) + 1L
			.TMAP_GRID$maptiles_urls = c(.TMAP_GRID$maptiles_urls, serv)
		}
		sub = if (is.na(a$sub)) NA else strsplit(a$sub, split = "")[[1]]
		serv = maptiles::create_provider(paste0("id_", tile_id), url = serv, citation = "", sub = sub)
		api = if (!is.null(a$api)) {
			a$api
		} else NULL
	} else if (substr(serv, 1, 6) %in% c("Thunde", "Stadia")) {
		is_stadia = substr(serv, 1, 6) == "Stadia"
		api = if (!is.null(a$api)) {
			a$api
		} else if (is_stadia) {
			Sys.getenv("STADIA_MAPS")
		} else {
			Sys.getenv("THUNDERFOREST_MAPS")
		}
		if (api == "") message_basemaps(is_stadia)
	} else {
		api = NULL
	}

	xs = mapply(function(b, z) {
		m = tryCatch({
			if (is.null(api)) {
				maptiles::get_tiles(x = b, provider = serv, zoom = z, crop = FALSE)
			} else {
				maptiles::get_tiles(x = b, provider = serv, apikey = api, zoom = z, crop = FALSE)
			}
		}, error = function(e) {
			tryCatch({
				if (is.null(api)) {
					maptiles::get_tiles(x = b, provider = serv, zoom = z - 1, crop = FALSE)
				} else {
					maptiles::get_tiles(x = b, provider = serv, apikey = api, zoom = z - 1, crop = FALSE)
				}
			}, error = function(e) {
				NULL
			})
		})
		if (!is.null(m)) {
			names(m)[1:3] = c("red", "green", "blue")
			if (terra::nlyr(m) == 4) names(m)[4] = "alpha"

		} else {
			message_basemaps_none(serv, z)
		}
		m
	}, bs3857, zs, SIMPLIFY = FALSE)

	if (isproj && !crs3857) {
		if (!all(vapply(xs, is.null, FUN.VALUE = logical(1)))) {
			message_basemaps_blurry(serv)
			xs = mapply(function(x,b) {
				if (is.null(x)) return(NULL)

				ex = terra::ext(as.vector(b[c(1,3,2,4)]))
				asp = (ex[2] - ex[1]) / (ex[4] - ex[3])

				tot = terra::ncell(x) * 2

				nc = round(sqrt(tot * asp))
				nr = round(tot / nc)

				r = terra::rast(ex, nrows = nr, ncols = nc, crs = crs$wkt)
				terra::project(x, r, method = "near")
			}, xs, bs_orig, SIMPLIFY = FALSE)
		}
	}

	ss = lapply(xs, function(x) {
		if (is.null(x)) NULL else do.call(tmapShape, list(shp = x, is.main = FALSE, crs = crs, bbox = NULL, unit=NULL, filter=NULL, shp_name = "x", smeta = list(), o = o, tmf = NULL))
	})

	srgb = tm_scale_rgb(max_color_value = 255, value.na = "#FFFFFF")


	ds = lapply(ss, function(s) {
		if (is.null(s)) return(NULL)
		d = s$dt
		d[, c("col", "legnr", "crtnr") := do.call(srgb$FUN, list(x1 = red, x2 = green, x3 = blue, scale = srgb, legend = list(), o = o, aes = "col", layer = "raster", layer_args = opt_tm_rgb(interpolate = TRUE)$mapping.args, sortRev = NA, bypass_ord = TRUE))]
		if ("alpha" %in% names(d)) {
			d[, col_alpha:=alpha/255 * a$alpha]
			d[is.na(col_alpha), col_alpha:=0]
		} else {
			d[, col_alpha:=a$alpha]
		}
		d
	})

	shpTMs = lapply(ss, function(s) {
		if (is.null(s)) NULL else s$shpTM
	})


	bmaps_shpTHs = structure(list(shpTMs), names = id)
	bmaps_dts = structure(list(ds), names = id)

	g$bmaps_shpTHs = c(g$bmaps_shpTHs, bmaps_shpTHs)
	g$bmaps_dts = c(g$bmaps_dts, bmaps_dts)

	assign("g", g, envir = .TMAP_GRID)
	paste0(a$server, collapse = "__")
}
