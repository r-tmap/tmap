updateData = function(tmo) {
	lapply(tmo, function(tmg) {
		# determine number of multiples
		tmg$tmls = lapply(tmg$tmls, tmapLayer)
		#browser()
		
		# add by variables to the data
		dt = tmg$tms$dt
		dtcols = tmg$tms$dtcols

		wrp = tmg$tmf$is.wrap
		
		if (tmg$tmf$is.wrap) {
			by1 = tmg$tmf$wrap
			by2 = NULL
			by3 = NULL
			
			# if wrap by is a variable, limit number of vars to 1 (in case multiple variables have been defined in the layer function)
			limitvars = (by1 != "VARS__")
			limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
		} else {
			by1 = tmg$tmf$rows
			by2 = tmg$tmf$columns
			by3 = tmg$tmf$pages
			
			limitvars = FALSE
			limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
			
			## try to assign VARS__ to one dimension. If not possible, limit number of vars to 1 (in case multiple variables have been defined in the layer function)
			if (!identical(by1, "VARS__") && !identical(by2, "VARS__") && !identical(by3, "VARS__")) {
				if (is.null(by1)) {
					by1 = "VARS__"
				} else if (is.null(by2)) {
					by2 = "VARS__"
				} else if (is.null(by3)) {
					by3 = "VARS__"
				} else {
					limitvars = TRUE
				}
			}
		}
		
		
		# which dimensions are used?
		which_by_vars = which(c(by1, by2, by3) == "VARS__")[1]
		which_by_def = setdiff(which(!vapply(list(by1, by2, by3), is.null, FUN.VALUE = logical(1))), which_by_vars)

		# total number of dimensions
		ndims = length(which_by_vars) + length(which_by_def)
		
		# make new by columns by1__ etc with factors
		if (any(which_by_def)) {
			for (w in which_by_def) {
				byvar = get(paste0("by", w))
				byname = paste0("by", w, "__")
				dt[, (byname) := factor(get(..byvar))]
			}
		}
		
		# create dummy by1__ variables
		if (ndims < 3) {
			for (w in setdiff(1L:3L, which_by_def)) {
				byname = paste0("by", w, "__")
				dt[, (byname) := factor(1L, levels = 1L)]
			}
		}
		
		bys = paste0("by", 1L:3L, "__")
		byv = bys[setdiff(1L:3L, which_by_vars)]
		
		
		# TODO
		tml = tmg$tmls[[1]]
		aes = tml$mapping.aes[[1]]
		nm = names(tml$mapping.aes)[1]
		val = aes$value
		nvars = length(aes$value) #m
		nvari = vapply(val, length, integer(1))
		
		
		
		relevant_vars = c("tmapID__", unlist(aes$value), bys)
		dtl = copy(dt[, relevant_vars, with = FALSE])

		
		# edit free argument. If NA, it is set to FALSE, and for the vars dimension to TRUE.
		fr = rep(aes$free, length.out = 3)
		if (any(is.na(fr))) {
			fr = rep(FALSE, 3)
			if (length(which_by_vars)) fr[which_by_vars] = TRUE
		}
		
		if (fr[which_by_vars] && !all(nvari == nvari[1])) stop("number of variables per aesthetic should be consistent when free = FALSE", call. = FALSE)
		#nvari = nvari[1]

		# multiple variables
		if (nvars > 1) {
			if (limitvars) {
				# not allowed: take first one
				warning(limitvars_warn, call. = FALSE)
				val = val[1]
			} else {
				if (!fr[which_by_vars]) {
					nms = aes$value[[1]]
					
					dtlks = lapply(1L:nvari[1], function(k) {
						vk = vapply(val, "[", character(1), k)
						melt(dtl, id.vars = c("tmapID__", byv), measure.vars = vk, variable.name = paste0("by", which_by_vars, "__"), value.name = nms[k])	
					})
					dtl = dtlks[[1]]
					if (nvari[1] > 1) {
						for (i in 2L:nvari[1]) {
							dtl[, (nms[i]) := dtlks[[i]][[nms[i]]]]
						}
					}
					val = val[[1]]
				}
			}
		}
		
		grp = bys[fr]
		
		if (fr[which_by_vars]) {
			if (!inherits(aes$setup, "tm_aes")) {
				setup = rep(aes$setup, length.out = nvars)
			} else {
				setup = rep(list(aes$setup), length.out = nvars)
			}
			
			varnames = paste(nm, 1L:nvars, sep = "_")
			mapply(function(s, v, varname) {
				f = s$FUN
				s$FUN = NULL
				dtl[, (varname) := do.call(f, c(unname(.SD), list(setup = s))), grp, .SDcols = v]
				NULL
			}, setup, val, varnames)
			
			byvarname = paste0("by", which_by_vars, "__")
			dtl[, (byvarname) := NULL]
			
			dtl = melt(dtl, id.vars = c("tmapID__", byv), measure.vars = varnames, variable.name = byvarname, value.name = nm)
			levels(dtl[[byvarname]]) = vapply(val, "[", character(1), 1)
		} else {
			if (inherits(aes$setup, "tm_aes")) {
				s = aes$setup
			} else {
				s = aes$setup[[1]]
			}
			f = s$FUN
			s$FUN = NULL
			dtl[, (nm) := do.call(f, c(unname(.SD), list(setup = s))), grp, .SDcols = val]
		}
		
		fr
		
		
				
		if (wrp) {
			
			if (by1 == "VARS__") {
			#dtl[,]
			}
		}
		
		
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
			
			for (i in 1:m) {
				dti = copy(dt)
			}
		}
		
		
		
		
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
				
				for (i in 1:m) {
					dti = copy(dt)
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
