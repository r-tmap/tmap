updateData = function(tmo) {
	groupnames = paste0("group", seq_along(tmo))
	fl = list(1L, 1L, 1L)
	e = environment()
	
	
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
		
		
		### which dimensions are used?
		which_by_vars = which(c(by1, by2, by3) == "VARS__") # the dimension for aes variables (length is 0 or 1)
		which_by_spec = setdiff(which(!vapply(list(by1, by2, by3), is.null, FUN.VALUE = logical(1))), which_by_vars) # specified 'by' dimensions (up to 3)
		ndims = length(which_by_vars) + length(which_by_spec) # total number of dimensions
		
		bys2 = paste0("by", 1L:3L)
		bys = paste0("by", 1L:3L, "__")
		byv = bys[setdiff(1L:3L, which_by_vars)]
		byvarname = bys[which_by_vars]
		
		
		# create byx__ columns for which_by_spec
		if (any(which_by_spec)) {
			for (w in which_by_spec) {
				byvar = bys2[w]
				byname = bys[w]
				dt[, (byname) := factor(get(get(..byvar)))]
				
				update_fl(k = w, lev = levels(dt[[byname]]))
				
				dt[, (byname) := as.integer(get(..byname))]
			}
		}

		# create dummy byx__ columns for !which_by_spec
		if (ndims < 3) {
			for (w in setdiff(1L:3L, which_by_spec)) {
				byname = paste0("by", w, "__")
				dt[, (byname) := 1L]
			}
		}
		
		
		
		layernames = paste0("layer", seq_along(tmg$tmls))
		lrs = lapply(tmg$tmls, function(tml) {
			getdts = function(aes, nm) {
				
				val = aes$value
				nvars = length(aes$value) #m
				nvari = vapply(val, length, integer(1))
				
				vars = unlist(aes$value)

				if (!all(vars %in% names(dt))) {
					# constant values (take first value (of possible MV) per facet)
					if (any(nvari) > 1) warning("Aesthetic values considered as direct visual variables, which cannot be used with MV", call. = FALSE)
					val1 = sapply(val, "[[", 1, USE.NAMES = FALSE)
					dtl = copy(dt[, c("tmapID__", bys), with = FALSE])
					
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
					
					update_fl(k = which_by_vars, m = nvars)
					
					
					dtl = melt(dtl, id.vars = c("tmapID__", byv), measure.vars = vnames, variable.name = byvarname, value.name = nm)
					dtl[, (byvarname) := as.integer(get(..byvarname))]
					#dtl[, legend := vector("list", length = nrow(dtl))]
					dtl_leg = NULL
				} else {
					relevant_vars = c("tmapID__", vars, bys)
					
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
							val = val[[1]]
						} else {
							if (!fr[which_by_vars]) {
								# pivot
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
					} else {
						val = val[[1]]
					}
					
					update_fl(k = which_by_vars, lev = vars)
					
					
					grp = bys[fr]
					
					if (length(which_by_vars) && fr[which_by_vars] && nvars > 1) {
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
							dtl[, c(varname, legname) := do.call(f, c(unname(.SD), list(setup = s))), grp, .SDcols = v]
							NULL
						}, setup, val, varnames, legnames)
						
						dtl[, (byvarname) := NULL]
						
						dtl_leg = melt(dtl, id.vars = c("tmapID__", byv), measure.vars = legnames, variable.name = byvarname, value.name = "legend")
						dtl = melt(dtl, id.vars = c("tmapID__", byv), measure.vars = varnames, variable.name = byvarname, value.name = nm)
						
						dtl[, (byvarname) := as.integer(get(..byvarname))]
						dtl_leg[, (byvarname) := as.integer(get(..byvarname))]
						
						#levels(dtl[[byvarname]]) = vapply(val, "[", character(1), 1)
						#levels(dtl_leg[[byvarname]]) = vapply(val, "[", character(1), 1)
						
						dtl$legend
						sel = !vapply(dtl_leg$legend, is.null, logical(1))
						dtl_leg = dtl_leg[sel, c(grp, "legend"), with = FALSE]
					} else {
						if (inherits(aes$setup, "tm_aes")) {
							s = aes$setup
						} else {
							s = aes$setup[[1]]
						}
						f = s$FUN
						s$FUN = NULL
						dtl[, c(nm, "legend") := do.call(f, c(unname(.SD), list(setup = s))), grp, .SDcols = val]
						
						sel = !vapply(dtl$legend, is.null, logical(1))
						dtl_leg = dtl[sel, c(grp, "legend"), with = FALSE]
						
					}
				}
				list(dt = dtl[, c("tmapID__", bys, nm), with = FALSE],
					 leg = dtl_leg)
			}
			
			trans = mapply(getdts, tml$trans.aes, names(tml$trans.aes), SIMPLIFY = FALSE)
			mapping = mapply(getdts, tml$mapping.aes, names(tml$mapping.aes), SIMPLIFY = FALSE)
			
			dts_trans = lapply(trans, function(x) x$dt)
			trans_legend = lapply(trans, function(x) x$leg)
			
			dts_mapping = lapply(mapping, function(x) x$dt)
			mapping_legend = lapply(mapping, function(x) x$leg)
			
			# 
			# if (length(dts_trans) > 0) {
			# 	dts_trans[[1]]
			# }
			# 
			# get_legend = function(dt) {
			# 	sel = !vapply(dt$legend, is.null, logical(1))
			# 	if (!any(sel)) NULL else dt[sel, c("by1__", "by2__", "by3__", "legend"), with = FALSE]	
			# } 
			# 
			# if (length(dts_trans) > 0) {
			# 	trans_legend = lapply(dts_trans, get_legend)
			# 	for (dt in dts_trans) dt[, legend:= NULL]
			# } else {
			# 	trans_legend = NULL
			# 	dts_trans = NULL
			# }
			# mapping_legend = lapply(dts_mapping, get_legend)
			# for (dt in dts_mapping) dt[, legend:= NULL]
			# 
			list(trans = dts_trans, trans_legend = trans_legend, mapping = dts_mapping, mapping_legend = mapping_legend)
		})
		names(lrs) = layernames
		lrs
	})
	names(grps) = groupnames
	attr(grps, "fl") = fl
	grps
}
