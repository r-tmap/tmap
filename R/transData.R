transData = function(ad) {
	bd = lapply(ad, function(adi) {
		#adi = ad[[2]]
		shpDT = adi$shpDT
		
		#al = adi$layers[[1]]
		
		trans_shp = function(al, shpDT) {
			transDT = al$trans_dt
			
			bycols = names(transDT)[substr(names(transDT), 1, 2) == "by"]
			sdcols = names(transDT)#[c(1L, ncol(transDT))]
			
			if (length(transDT)) {
				y = transDT[, .(shp = do.call(do_trans, list(tdt = .SD, FUN = al$trans_fun, shpDT = shpDT))), by = bycols, .SDcols = sdcols]	
				shpDT = rbindlist(y$shp)
			} else {
				shpDT$shpTM = lapply(shpDT$shpTM, function(s) do.call(al$trans_fun, list(shpTM = s)))
			}
			shpDT
		}
		
		
		for (al in adi$layers) {
			if (al$trans_isglobal) shpDT = trans_shp(al, shpDT)
		}
		
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
	attributes(bd) = attributes(ad)
	bd
}
