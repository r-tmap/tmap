updateData = function(tmo) {
	lapply(tmo, function(tmg) {
		# determine number of multiples
		tmg$tmls = lapply(tmg$tmls, tmapLayer)
		#browser()
		
		# add by1__, by2__, and along__ variables to dt
		dt = tmg$tms$dt
		dtcols = tmg$tms$dtcols
		
		by = tmg$tmf$by
		along = tmg$tmf$along
		
		if (is.null(by)) {
			dt[, by1__ := 1L]
			dt[, by2__ := 1L]
		} else {
			dt[, by1__ := get(..by[1])]
			if (length(by) == 2) {
				dt[, by2__ := get(..by[2])]
			} else {
				dt[, by2__ := 1L]
			}
		}
		if (is.null(along)) {
			dt[, along__ := 1L]
		} else {
			dt[, along__ := get(..along__)]
		}
		# NOTE tmg_tms$dt = dt not needed, because data.table works by reference
		
		
		lapply(seq_along(tmg$tmls), function(k) {
			tml = tmg$tmls[[k]]
			dname = paste0("dv__l", k)
			vname = paste0("vv__l", k)
			mapply(function(nm, vs) {
				for (i in seq_along(vs)) {
					vsi = vs[[i]]
					if (!all(vsi %in% names(dt))) {
						vname2 = paste(vname, nm, i, sep = "__")
						if (length(vsi) > 1L) warning("multiple constant values (with MV) not supported")
						dt[, (vname2) := vsi[1]]	
					} else {
						for (j in seq_along(vsi)) {
							vsj = vsi[j]
							dname2 = paste(dname, nm, i, j, sep = "__")
							dt[, (dname2) := get(vsj)]	
						}
					}
				}
				invisible(NULL)
			}, names(tml$mapping.aes), lapply(tml$mapping.aes, "[[", "value"), SIMPLIFY = FALSE, USE.NAMES = FALSE)
		})
		
		dt[, (dtcols):= NULL]
		invisible(NULL)
	})
	invisible(NULL)
}
