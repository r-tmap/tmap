step2_data = function(tmo) {
	groupnames = paste0("group", seq_along(tmo))
	fl = list(1L, 1L, 1L)
	e = environment()
	
	# function to update the levels fl per facet dimension
	update_fl = function(k, lev = NULL, m = NULL) {
		fl = get("fl", envir = e)
		fk = fl[[k]]
		n =  if (is.character(fk)) length(fk) else fk
		
		if (!missing(lev)) {
			m = length(lev)
			if (m == 1L && n == 1L) {
				fk = lev
			} else if (m > 1L && n > 1L && m != n) {
				stop("Inconsistent number of facets in the ", k, " dimension.", call. = FALSE)
			} else if (m > n) {
				fk = lev
			}
		} else if (!missing(m)) {
			if (m > 1L && n > 1L && m != n) {
				stop("Inconsistent number of facets in the ", k, " dimension.", call. = FALSE)
			} else if (m > n) {
				fk = m
			}
		}
		fl[[k]] = fk
		assign("fl", fl, envir = e)
	}
	
	
	grps = lapply(tmo, function(tmg) {
		dt = tmg$tms$dt

		### Specify 'by' variables
		if (tmg$tmf$is.wrap) {
			# facet wrap: only use by1
			by1 = tmg$tmf$wrap
			by2 = NULL
			by3 = NULL
			
			# By default, facets are created over the aes variables ("VARS__"). If wrap is specified in tm_facets, limit number of variables to 1.
			limitvars = (by1 != "VARS__")
			limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
		} else {
			# facet grid
			by1 = tmg$tmf$rows
			by2 = tmg$tmf$columns
			by3 = tmg$tmf$pages
			
			## Try to assign VARS__ to one dimension. If not possible, limit number of variables to 1.
			limitvars = FALSE
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
			limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
		}
		
		
		# v is variable by-dimension, b are the group-by by-dimensions
		v = which(c(by1, by2, by3) == "VARS__")
		b = setdiff(which(!vapply(list(by1, by2, by3), is.null, FUN.VALUE = logical(1))), v)
		
		n = length(v) + length(b)

		by123 = paste0("by", 1L:3L) 
		by123__ = paste0("by", 1L:3L, "__")
		
		byvarname = by123__[v]

		grp_b = by123__[b]
		

		# create byx__ columns for which_by_spec
		if (length(b)) {
			for (w in b) {
				byvar = by123[w]
				byname = by123__[w]
				dt[, (byname) := factor(get(get(..byvar)))]
				update_fl(k = w, lev = levels(dt[[byname]]))
				dt[, (byname) := as.integer(get(..byname))]
			}
		}

		layernames = paste0("layer", seq_along(tmg$tmls))
		lrs = lapply(tmg$tmls, function(tml) {
			getdts = function(aes, nm) {
				
				val = aes$value
				nvars = length(aes$value) #m
				nvari = vapply(val, length, integer(1))
				
				vars = unlist(aes$value)
				
				# active grouping variables (to keep)
				grp_bv = by123__[sort(c({if (nvars > 1) v else integer(0)}, b))]
				
				
				#print(vars)

				if (!all(vars %in% names(dt))) {
					# constant values (take first value (of possible MV) per facet)
					if (any(nvari) > 1) warning("Aesthetic values considered as direct visual variables, which cannot be used with MV", call. = FALSE)
					val1 = sapply(val, "[[", 1, USE.NAMES = FALSE)
					dtl = copy(dt[, c("tmapID__", by123__[b]), with = FALSE])
					
					if (nvars > 1 && limitvars) {
						# not allowed: take first one
						warning(limitvars_warn, call. = FALSE)
						val1 = val1[1]
						nvars = 1L
					}
					vnames = paste0("value", sprintf("%02d", 1L:nvars))
					for (i in 1L:nvars) {
						vname = vnames[i]
						dtl[, (vname) := val1[i]]
					}
					
					if (length(v)) update_fl(k = v, m = nvars)
					
					if (nvars > 1) {
						dtl = melt(dtl, id.vars = c("tmapID__", by123__[b]), measure.vars = vnames, variable.name = byvarname, value.name = nm)
						dtl[, (byvarname) := as.integer(get(..byvarname))]
					} else {
						setnames(dtl, vnames[1], nm)
					}
					#dtl[, legend := vector("list", length = nrow(dtl))]
					dtl_leg = NULL
				} else {
					relevant_vars = c("tmapID__", vars, by123__[b])
					
					dtl = copy(dt[, relevant_vars, with = FALSE])
					
					
					# edit free argument. If NA, it is set to FALSE, and for the vars dimension to TRUE.
					fr = rep(aes$free, length.out = 3)
					if (any(is.na(fr))) {
						fr = rep(FALSE, 3)
						if (length(v)) fr[v] = TRUE
					}
					
					# group by variables with free scales
					grp_b_fr = by123__[intersect(which(fr), b)]
					grp_bv_fr = by123__[sort(c({if (nvars > 1) v else integer(0)}, intersect(which(fr), b)))]

					if (length(v) && fr[v] && !all(nvari == nvari[1])) stop("number of variables per aesthetic should be consistent when free = FALSE", call. = FALSE)

					# multiple variables
					if (nvars > 1) {
						if (limitvars) {
							# not allowed: take first one
							warning(limitvars_warn, call. = FALSE)
							val = val[[1]]
						} else {
							if (!fr[v]) {
								# multiple variables, !free scale => stack all variable columns in long format
								nms = aes$value[[1]]
								
								dtlks = lapply(1L:nvari[1], function(k) {
									vk = vapply(val, "[", character(1), k)
									melt(dtl, id.vars = c("tmapID__", by123__[b]), measure.vars = vk, variable.name = by123__[v], value.name = nms[k])	
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
					} else {
						val = val[[1]]
					}
					
					if (length(v)) update_fl(k = v, lev = vars)
					
					if (length(v) && fr[v] && nvars > 1) {
						# apply aes function for each var column
						if (!inherits(aes$setup, "tm_aes")) {
							setup = rep(aes$setup, length.out = nvars)
						} else {
							setup = rep(list(aes$setup), length.out = nvars)
						}
						
						varnames = paste(nm, 1L:nvars, sep = "_")
						legnames = paste("legend", 1L:nvars, sep = "_")
						mapply(function(s, v, varname, legname) {
							f = s$FUN
							s$FUN = NULL
							dtl[, c(varname, legname) := do.call(f, c(unname(.SD), list(setup = s))), grp_b_fr, .SDcols = v]
							NULL
						}, setup, val, varnames, legnames)
						
						dtl_leg = melt(dtl, id.vars = c("tmapID__", grp_b), measure.vars = legnames, variable.name = byvarname, value.name = "legend")
						dtl = melt(dtl, id.vars = c("tmapID__", grp_b), measure.vars = varnames, variable.name = byvarname, value.name = nm)
						
						dtl[, (byvarname) := as.integer(get(..byvarname))]
						dtl_leg[, (byvarname) := as.integer(get(..byvarname))]
						
						sel = !vapply(dtl_leg$legend, is.null, logical(1))
						dtl_leg = dtl_leg[sel, c(grp_bv_fr, "legend"), with = FALSE]
					} else {
						# apply aes function to the (only) var column
						if (inherits(aes$setup, "tm_aes")) {
							s = aes$setup
						} else {
							s = aes$setup[[1]]
						}
						f = s$FUN
						s$FUN = NULL
						dtl[, c(nm, "legend") := do.call(f, c(unname(.SD), list(setup = s))), grp_b_fr, .SDcols = val]
						
						sel = !vapply(dtl$legend, is.null, logical(1))
						dtl_leg = dtl[sel, c(grp_bv_fr, "legend"), with = FALSE]
					}
				}
				list(dt = dtl[, c("tmapID__", grp_bv, nm), with = FALSE],
					 leg = dtl_leg)
			}
			
			trans = mapply(getdts, tml$trans.aes, names(tml$trans.aes), SIMPLIFY = FALSE)
			mapping = mapply(getdts, tml$mapping.aes, names(tml$mapping.aes), SIMPLIFY = FALSE)
			
			dts_trans = cbind_dts(lapply(trans, function(x) x$dt))
			trans_legend = lapply(trans, function(x) x$leg)
			
			dts_mapping = cbind_dts(lapply(mapping, function(x) x$dt))
			mapping_legend = lapply(mapping, function(x) x$leg)
			
			list(trans_dt = dts_trans, 
				 trans_legend = trans_legend, 
				 trans_fun = tml$trans.fun,
				 trans_isglobal = tml$trans.isglobal,
				 mapping_dt = dts_mapping, 
				 mapping_legend = mapping_legend,
				 mapping_fun = tml$mapping.fun)
		})
		names(lrs) = layernames
		
		shpDT = data.table(shpTM = list(list(shp = tmg$tms$shp, tmapID = dt$tmapID__)))
		list(layers = lrs, shpDT = shpDT)
	})
	names(grps) = groupnames
	attr(grps, "fl") = fl
	attr(grps, "main") = attr(tmo, "main")
	grps
}

cbind_dts = function(dts) {
	if (!length(dts)) return(list())
	id = which.max(vapply(dts, ncol, FUN.VALUE = integer(1)))
	
	dt = dts[[id]]
	
	if (length(dts) > 1L) {
		for (i in setdiff(seq_along(dts), id)) {
			dti = dts[[i]]
			dt = dt[dti, on = names(dti)[1L:(ncol(dti)-1)]]
		}
	}
	dt
}
