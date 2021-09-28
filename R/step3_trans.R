step3_trans = function(tm) {
	ad = tm$tmo
	meta = tm$meta
	
	bd = lapply(ad, function(adi) {
		shpDT = adi$shpDT
		

		trans_shp = function(al, shpDT) {
			transDT = al$trans_dt
			
			plot.order = al$plot.order
			
			bycols = names(transDT)[substr(names(transDT), 1, 2) == "by"]
			sdcols = names(transDT)
			
			if (length(transDT)) {
				y = transDT[, .(shp = do.call(do_trans, list(tdt = .SD, FUN = al$trans_fun, shpDT = shpDT, plot.order = plot.order, args = al$trans_args))), by = bycols, .SDcols = sdcols]	
				shpDT = rbindlist(y$shp)
			} else {
				shpDT$shpTM = lapply(shpDT$shpTM, function(s) do.call(al$trans_fun, list(shpTM = s, plot.order = plot.order)))
			}
			shpDT
		}
		
		# first apply all global transformation functions
		for (al in adi$layers) {
			if (al$trans_isglobal) shpDT = trans_shp(al, shpDT)
		}
		
		crs = if (is.na(meta$crs[1])) get_crs(shpDT$shpTM[[1]]) else meta$crs
		
		#crs = meta$crs
		
		
		
		
		shpDT$shpTM = lapply(shpDT$shpTM, function(s) {
			# crs can be a list per class (due to leaflet EPSG:3857 requirement for raster images)
			crs = get_option_class(crs, class = class(s$shp))
			# if (!inherits(crs, "crs")) {
			# 	crsnms = names(crs)
			# 	crsi = which(vapply(crsnms, function(crsnm) {
			# 		if (crsnm == "") TRUE else inherits(s$shp, crsnm)
			# 	}, logical(1)))[1]
			# 	crs = crs[[crsi]]
			# }

			if (sf::st_crs(s$shp) != crs) {
				s = do.call(tmapReproject, c(s, list(crs = crs)))
			}
			s
		})
		
		
		adi$layers = lapply(adi$layers, function(al) {
			if (al$trans_isglobal) {
				al$shpDT = shpDT
			} else {
				al$shpDT = trans_shp(al, shpDT)
			}
			al
		})
		
		adi$shpDT = NULL
		
		adi	
	})	
	#attributes(bd) = attributes(ad)
	#attr(bd, "bbox") = stm_bbox(ad[[attr(bd, "main")]]$shpDT$shpTM[[1]])
	list(tmo = bd, meta = meta)
}

