crs_reproject_shpTM = function(s, crs, raster.warp) {
	# crs can be a list per class (due to leaflet EPSG:3857 requirement for raster images)
	crs2 = sf::st_crs(get_option_class(crs, class = class(s$shp)))
	if (sf::st_crs(s$shp) != crs2) {
		s = do.call(tmapReproject, c(s, list(crs = crs2, raster.warp = raster.warp)))
	}
	s
}

trans_sf = function(sf1, sf2, nframes, ease) {
	df1 = data.frame(geo = sf::st_geometry(sf1))
	df2 = data.frame(geo = sf::st_geometry(sf2))
	x = transformr::tween_sf(df1, df2, ease = ease, nframes = nframes)
	sf::st_sf(.frame = x$.frame, geometry = x$geometry, crs = sf::st_crs(sf1))
}

trans_shpTM = function(shpTM1, shpDT, nframes, ease) {
	rlang::check_installed(c("transformr", "gifski"))
	shpTM2 = shpDT$shpTM[[1 ]]
	s = trans_sf(shpTM1$shp, shpTM2$shp[match(shpTM1$tmapID, shpTM2$tmapID)], nframes, ease)

	data.table::rbindlist(lapply(1:nframes, function(i) {
		x = shpDT
		x$shpTM[[1]]$shp = s$geometry[s$.frame == i]
		x$shpTM[[1]]$tmapID = shpTM1$tmapID
		x$by3__ = i
		x
	}))

	# data.table(shpTM = lapply(1:nframes, function(i) {
	# 	shpTM1$shp = s$geometry[s$.frame == i]
	# 	shpTM1
	# }), by3__ = 1:nframes)
}

step3_trans = function(tm) {
	ad = tm$tmo
	o = tm$o
	aux = tm$aux
	cmp = tm$cmp
	prx = tm$prx

	bd = lapply(ad, function(adi) {
		shpDT = adi$shpDT


		trans_shp = function(al, shpDT) {
			transDT = al$trans_dt

			plot.order = al$plot.order
			bycols = names(transDT)[substr(names(transDT), 1, 2) == "by"]
			sdcols = setdiff(names(transDT), "frame")

			if (length(transDT)) {
				y = transDT[, .(shp = do.call(do_trans, list(tdt = .SD, FUN = al$trans_fun, shpDT = shpDT, plot.order = plot.order, args = al$trans_args, scale = o$scale))), by = bycols, .SDcols = sdcols]

				if ("by3__" %in% bycols && all(is.na(y$by3__))) {
					shpDT = data.table::rbindlist(lapply(y$shp, function(yi) {
						trans_shpTM(shpDT$shpTM[[1]], yi, nframes = o$nframes, ease = "cubic-in-out")
					}))


				} else {
					shpDT = data.table(rbindlist(y$shp))
				}
			} else {
				shpDT$shpTM = lapply(shpDT$shpTM, function(s) do.call(al$trans_fun, list(shpTM = s, plot.order = plot.order, args = al$trans_args)))
			}
			shpDT
		}

		crs_step4 = o$crs_step4
		crs_step3 = o$crs_step3



		# step 3.a : reproject crs to main_crs
		shpDT$shpTM = lapply(shpDT$shpTM, crs_reproject_shpTM, crs = crs_step3, raster.warp = o$raster.warp)

		# step 3.b: apply all global transformation functions
		appl = vapply(adi$layers, "[[", "trans_apply", FUN.VALUE = character(1))

		is_global = which(appl == "all")

		if (length(is_global) > 1) {
			stop("Number of 'global transformation' layers is larger than 1", call. = FALSE)
		} else if (length(is_global) == 1) {
			shpDT = trans_shp(adi$layers[[is_global]], shpDT)
		}

		e = environment()

		shpDT_along = shpDT

		adi$layers = lapply(adi$layers, function(al) {
			if (al$trans_apply == "all") {
				al$shpDT = shpDT
			} else {
				al$shpDT = trans_shp(al, shpDT_along)
			}
			if (al$trans_apply == "this_following") {
				assign("shpDT_along", al$shpDT, envir = e)
			}

			# step 3.c2 : reproject crs to crs (for plotting)
			al$shpDT$shpTM = lapply(al$shpDT$shpTM, crs_reproject_shpTM, crs = crs_step4, raster.warp = o$raster.warp)

			al[c("trans_dt", "trans_args", "trans_apply", "tp")] = NULL
			al
		})
		if (!length(adi$layers)) {
			adi$bbx = tmaptools::bb(shpDT$shpTM[[1]]$shp)
		}
		adi$shpDT = NULL

		adi
	})

	list(tmo = bd, aux = aux, cmp = cmp, prx = prx, o = o)
}

