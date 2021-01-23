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
		
		calc_aes = function(aes, nm) {
			aes = tml$mapping.aes[[1]]

			nm = names(tml$mapping.aes)[1]
			m = length(aes$value)
			if (aes$free) {
				if (!inherits(aes$setup, "tm_aes")) {
					setup = rep(aes$setup, length.out = m)
				} else {
					setup = rep(list(aes$setup), length.out = m)
				}
				
				if (!inherits(aes$fun, "function")) {
					fun = rep(aes$fun, length.out = m)
				} else {
					fun = rep(list(aes$fun), length.out = m)
				}
				
				
				f = function(value, fun, setup) {
					do.call(fun, list(x1 = value, setup = setup))
				}
				
				mapply(f, aes$value, fun, setup, SIMPLIFY = FALSE, USE.NAMES = FALSE)
				
				do.call(aes$setup)
				
			} else {
				if (!inherits(aes$setup, "tm_aes")) {
					setup = aes$setup[[1]]
				}
			}
		}
		
		
		lapply(seq_along(tmg$tmls), function(k) {
			tml = tmg$tmls[[k]]
			
			tml$trans.aes
			tml$mapping.aes
			
			
			mapply(calc_aes, names(tml$mapping.aes), tml$mapping.aes, SIMPLIFY = FALSE, USE.NAMES = FALSE)
			
			
			dname = paste0("l", k, "__data")
			vname = paste0("l", k, "__vis")
			tname = paste0("l", k, "__trans")
			mapply(repl_vars, names(tml$mapping.aes), lapply(tml$mapping.aes, "[[", "value"), MoreArgs = list(name_from = dname, name_to = vname), SIMPLIFY = FALSE, USE.NAMES = FALSE)
			mapply(repl_vars, names(tml$trans.aes), lapply(tml$trans.aes, "[[", "value"), MoreArgs = list(name_from = dname, name_to = tname), SIMPLIFY = FALSE, USE.NAMES = FALSE)
		})
		
		
		
		repl_vars = function(nm, vs, name_from, name_to) {
			for (i in seq_along(vs)) {
				vsi = vs[[i]]
				if (!all(vsi %in% names(dt))) {
					vname2 = paste(name_to, nm, i, sep = "__")
					if (length(vsi) > 1L) warning("multiple constant values (with MV) not supported")
					dt[, (vname2) := vsi[1]]	
				} else {
					for (j in seq_along(vsi)) {
						vsj = vsi[j]
						dname2 = paste(name_from, nm, i, j, sep = "__")
						dt[, (dname2) := get(vsj)]	
					}
				}
			}
			invisible(NULL)
		}
		

		lapply(seq_along(tmg$tmls), function(k) {
			tml = tmg$tmls[[k]]
			dname = paste0("l", k, "__data")
			vname = paste0("l", k, "__vis")
			tname = paste0("l", k, "__trans")
			mapply(repl_vars, names(tml$mapping.aes), lapply(tml$mapping.aes, "[[", "value"), MoreArgs = list(name_from = dname, name_to = vname), SIMPLIFY = FALSE, USE.NAMES = FALSE)
			mapply(repl_vars, names(tml$trans.aes), lapply(tml$trans.aes, "[[", "value"), MoreArgs = list(name_from = dname, name_to = tname), SIMPLIFY = FALSE, USE.NAMES = FALSE)
		})

		dt[, (dtcols):= NULL]
		invisible(NULL)
	})
	invisible(NULL)
}
