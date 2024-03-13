step3_trans = function(tm) {
	ad = tm$tmo
	o = tm$o
	aux = tm$aux
	cmp = tm$cmp
	
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
		
		crs = o$crs
		crs_main = o$crs_main
		
		#crs = if (is.na(o$crs[1])) get_crs(shpDT$shpTM[[o$main]]) else o$crs
	
		crs_reproject_shpTM = function(s, crs) {
			# crs can be a list per class (due to leaflet EPSG:3857 requirement for raster images)
			crs2 = sf::st_crs(get_option_class(crs, class = class(s$shp)))
			if (sf::st_crs(s$shp) != crs2) {
				s = do.call(tmapReproject, c(s, list(crs = crs2)))
			}
			s
		}
		
		# step 3.a : reproject crs to main_crs
		shpDT$shpTM = lapply(shpDT$shpTM, crs_reproject_shpTM, crs = crs_main)

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
			al$shpDT$shpTM = lapply(al$shpDT$shpTM, crs_reproject_shpTM, crs = crs)
			
			al[c("trans_dt", "trans_args", "trans_isglobal", "tp")] = NULL
			al
		})
	

		adi$shpDT = NULL

		adi
	})
	
	list(tmo = bd, aux = aux, cmp = cmp, o = o)
}

