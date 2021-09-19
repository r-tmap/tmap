getdts = function(aes, nm, p, q, o, dt, layer) {
	dev = getOption("tmap.devel.mode")
	
	if (dev) timing_add(s4 = paste0("aes ", aes$aes))
	
	
	nm = aes$aes
	
	val = aes$value
	
	with(q, {
		if (inherits(val, "tmapOption")) val = getAesOption(val[[1]], o, aes = aes$aes, layer = layer)
		
		
		nvars = length(val) #m
		nvari = vapply(val, length, integer(1))
		
		vars = unlist(val)
		
		# active grouping variables (to keep)
		grp_bv = by123__[sort(c({if (nvars > 1) v else integer(0)}, b))]
		
		
		#print(vars)
		
		if (!all(vars %in% names(dt))) {
			#cat("step2_grp_lyr_aes_const\n")
			# constant values (take first value (of possible tm_mv per facet)
			if (any(nvari) > 1) warning("Aesthetic values considered as direct visual variables, which cannot be used with tm_mv", call. = FALSE)
			val1 = sapply(val, "[[", 1, USE.NAMES = FALSE)
			dtl = copy(dt[, c("tmapID__", "sel__", by123__[b]), with = FALSE])
			
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
				dtl = melt(dtl, id.vars = c("tmapID__", "sel__", by123__[b]), measure.vars = vnames, variable.name = var__, value.name = nm)
				dtl[, (var__) := as.integer(get(..var__))]
			} else {
				setnames(dtl, vnames[1], nm)
			}
			
			# impute null (filter argument of tm_shape) with value.null					
			if (any(!dtl$sel__) || !q$drop.units) {
				# also needed for drop.units later on
				cls = data_class(dtl[[nm]])
				value.null = getAesOption("value.null", o, aes$aes, layer, cls = cls)
				
				dtl[sel__==FALSE, (nm) := value.null]
				
				if (!q$drop.units) {
					imp = structure(value.null, names = nm)
					dtl = completeDT(dtl, cols = c("tmapID__", grp_bv), defs = imp)
				}
				
			}
			#dtl[, sel__:= NULL]
			
			
			dtl[, legnr := vector("integer", length = nrow(dtl))]
			#dtl_leg = NULL
			#sel = !vapply(dtl$legend, is.null, logical(1))
			
			
			dtl_leg = dtl[, .SD[1], by = c(grp_bv)][, tmapID__ := NULL][, legnr := (vapply(get(..nm), function(s) legend_save(list(vneutral = s)), FUN.VALUE = integer(1)))][, (nm) := NULL]
			#dtl_leg = dtl[, .SD[1], by = c(grp_bv)][, tmapID__ := NULL][, legend := list(lapply(get(..nm), function(s) list(vneutral = s)))][, (nm) := NULL]
			
			#dtl_leg = NULL
		} else {
			#cat("step2_grp_lyr_aes_var\n")
			
			relevant_vars = c("tmapID__", "sel__" , vars, by123__[b])
			
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
					#cat("step2_grp_lyr_aes_var_limiter\n")
					# not allowed: take first one
					warning(limitvars_warn, call. = FALSE)
					val = val[[1]]
				} else {
					if (!fr[v]) {
						#cat("step2_grp_lyr_aes_var_multi_vars_!free_scale\n")
						
						# multiple variables, !free scale => stack all variable columns in long format
						nms = val[[1]]
						
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
						
						for (col in by123__[v]) {
							dtl[, (col) := as.integer(get(..col))]
						}
						
						vars = vapply(val, "[[", character(1), 1)
						val = val[[1]]
					} else {
						#cat("step2_grp_lyr_aes_var_multi_vars_free_scale\n")
						vars = vapply(val, "[[", character(1), 1)
						
					}
				}
			} else {
				#cat("step2_grp_lyr_aes_var_one_var\n")
				val = val[[1]]
				vars = vars[1] # only needed for update_fl?
			}
			
			if (length(v)) update_fl(k = v, lev = vars)
			
			
			apply_scale = function(s, l, v, varname, legname) {
				# update legend defaults from options
				tmp = names(o)[substr(names(o), 1, 6) == "legend"]
				
				# update legend format
				l$format = process_legend_format(l$format, o$legend.format)
				
				# update other legend options
				opt_leg = setdiff(intersect(substr(tmp, 8, nchar(tmp)), names(l)), "format")
				l[opt_leg] = mapply(function(x, nm) {
					if (is.na(x[1])) o[[paste0("legend.", nm)]] else x
				}, l[opt_leg], opt_leg, SIMPLIFY = FALSE)
				
				if (length(s) == 0) stop("mapping not implemented for aesthetic ", nm, call. = FALSE)
				f = s$FUN
				s$FUN = NULL
				cls = data_class(dtl[[v[1]]])
				#if (is.na(s$legend$title)) s$legend$title = v
				if (is.na(l$title)) l$title = paste0(v, attr(cls, "units"))
				#aesname = aes$aes
				value.null = if ("value.null" %in% names(s)) s$value.null else {
					getAesOption("value.null", o, aes$aes, layer, cls = cls)
				}
				if (!all(dtl$sel__)) {
					dtl[, c(varname, legname) := list(value.null, 0L)]
					if (is.na(value.null)) stop("value.null not specified for aesthetic ", nm, call. = FALSE)
					dtl[sel__ == TRUE, c(varname, legname) := do.call(f, c(unname(.SD), list(scale = s, legend = l, opt = o, aes = aes$aes, layer = layer, p = names(p)[match(paste0("__", aes$aes), p)]))), grp_b_fr, .SDcols = v]
				} else {
					dtl[, c(varname, legname) := do.call(f, c(unname(.SD), list(scale = s, legend = l, opt = o, aes = aes$aes, layer = layer, p = names(p)[match(paste0("__", aes$aes), p)]))), grp_b_fr, .SDcols = v]
				}
				
				if (!q$drop.units) {
					imp = structure(list(value.null, 0L), names = c(nm, legname))
					dtl = completeDT(dtl, cols = c("tmapID__", grp_bv), defs = imp)
				}
				
				dtl
			}
			
			if (length(v) && fr[v] && nvars > 1) {
				#cat("step2_grp_lyr_aes_var_multi_aes_columns\n")
				# apply aes function for each var column
				if (inherits(aes$scale, "tm_scale")) {
					scale = rep(list(aes$scale), length.out = nvars)
				} else if (islistof(aes$scale, "tm_scale")) {
					scale = rep(aes$scale, length.out = nvars)
				} else {
					stop("incorrect scale specification")
				}
				
				if (inherits(aes$legend, "tm_legend")) {
					legend = rep(list(aes$legend), length.out = nvars)
				} else if (islistof(aes$legend, "tm_legend")) {
					legend = rep(aes$legend, length.out = nvars)
				} else {
					stop("incorrect legend specification")
				}
				
				varnames = paste(nm, 1L:nvars, sep = "_")
				legnames = paste("legnr", 1L:nvars, sep = "_")
				#mapply(apply_scale, scale, legend, val, varnames, legnames)
				
				for (i in 1L:nvars) {
					#mapply(apply_scale, scale, legend, val, varnames, legnames) does not work because of completeDT
					dtl = apply_scale(scale[[i]], legend[[i]], val[[i]], varnames[[i]], legnames[[i]])
				}
				
				
				
				dtl_leg = melt(dtl, id.vars = c("tmapID__", by__), measure.vars = legnames, variable.name = var__, value.name = "legnr")
				dtl = melt(dtl, id.vars = c("tmapID__", by__), measure.vars = varnames, variable.name = var__, value.name = nm)
				
				dtl[, (var__) := as.integer(get(..var__))]
				dtl_leg[, (var__) := as.integer(get(..var__))]
				
				#sel = !vapply(dtl_leg$legend, is.null, logical(1))
				dtl_leg = dtl_leg[legnr != 0, c(grp_bv_fr, "legnr"), with = FALSE]
			} else {
				#cat("step2_grp_lyr_aes_var_one_aes_column\n")
				
				# apply aes function to the (only) var column
				if (inherits(aes$scale, "tm_scale")) {
					s = aes$scale
				} else if (islistof(aes$scale, "tm_scale")) {
					s = aes$scale[[1]]
				} else {
					stop("incorrect scale specification")
				}
				
				if (length(s) == 0) stop("mapping not implemented for aesthetic ", nm, call. = FALSE)
				
				if (inherits(aes$legend, "tm_legend")) {
					l = aes$legend
				} else if (islistof(aes$legend, "tm_legend")) {
					warning("multiple legends are specified, while only one is required; the first will be used")
					l = aes$legend[[1]]
				} else {
					stop("incorrect legend specification")
				}
				
				dtl = apply_scale(s, l, val, nm, "legnr")
				#sel = !vapply(dtl$legend, is.null, logical(1))
				dtl_leg = dtl[legnr != 0L, c(grp_bv_fr, "legnr"), with = FALSE]
			}
		}
		
		list(dt = dtl[, c("tmapID__", grp_bv, nm), with = FALSE],
			 leg = dtl_leg)
	})
}
