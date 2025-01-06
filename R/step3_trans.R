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
			sdcols = names(transDT)

			if (length(transDT)) {
				y = transDT[, .(shp = do.call(do_trans, list(tdt = .SD, FUN = al$trans_fun, shpDT = shpDT, plot.order = plot.order, args = al$trans_args, scale = o$scale))), by = bycols, .SDcols = sdcols]
				shpDT = rbindlist(y$shp)
			} else {
				shpDT$shpTM = lapply(shpDT$shpTM, function(s) do.call(al$trans_fun, list(shpTM = s, plot.order = plot.order, args = al$trans_args)))
			}
			shpDT
		}

		crs_step4 = o$crs_step4
		crs_step3 = o$crs_step3

		crs_reproject_shpTM = function(s, crs) {
			# crs can be a list per class (due to leaflet EPSG:3857 requirement for raster images)
			crs2 = sf::st_crs(get_option_class(crs, class = class(s$shp)))
			if (sf::st_crs(s$shp) != crs2) {
				s = do.call(tmapReproject, c(s, list(crs = crs2, raster.warp = o$raster.warp)))
			}
			s
		}

		# step 3.a : reproject crs to main_crs
		shpDT$shpTM = lapply(shpDT$shpTM, crs_reproject_shpTM, crs = crs_step3)

		# step 3.b: apply all global transformation functions
		for (al in adi$layers) {
			if (al$trans_isglobal) shpDT = trans_shp(al, shpDT)
		}


		adi$layers = lapply(adi$layers, function(al) {
			# step 3.c1: apply non global transformation function
			if (al$trans_isglobal) {
				al$shpDT = shpDT
			} else {
				al$shpDT = trans_shp(al, shpDT)
			}

			# step 3.c2 : reproject crs to crs (for plotting)
			al$shpDT$shpTM = lapply(al$shpDT$shpTM, crs_reproject_shpTM, crs = crs_step4)

			al[c("trans_dt", "trans_args", "trans_isglobal", "tp")] = NULL
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

