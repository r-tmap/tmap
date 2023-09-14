update_l = function(o, l, v, mfun) {
	# update legend options
	oltype = o[c("legend.design", "legend.orientation")]
	names(oltype) = c("design", "orientation")
	if (all(v %in% c("AREA", "LENGTH", "MAP_COLORS")) && is.null(l$show)) {
		l$show = FALSE
	}
	
	call = names(l)
	
	l = complete_options(l, oltype)
	oleg = o[names(o)[substr(names(o), 1, 6) == "legend" & substr(names(o), 1, 15) != "legend.settings"]]
	names(oleg) = substr(names(oleg), 8, nchar(names(oleg)))
	settings_name = paste0("legend.settings.", l$design, ".", l$orientation)
	oleg = c(oleg, o[[settings_name]])
	
	
	if ("position" %in% names(l) && is.character(l$position)) l$position = str2pos(l$position)
	if ("position" %in% names(l) && is.numeric(l$position)) l$position = num2pos(l$position)
	
	
	
	l = complete_options(l, oleg)
	l$call = call
	l$mfun = mfun
	
	# update legend class
	class(l) = c(paste0("tm_legend_", l$design, ifelse(!is.null(l$orientation), paste0("_", l$orientation), "")), class(l)) 
	l
}


getdts = function(aes, unm, p, q, o, dt, shpvars, layer, mfun, args, plot.order) {
	dev = getOption("tmap.devel.mode")
	
	nm = aes$aes
	nm__ord = paste0(nm, "__ord")
	
	# should the results of the data (needed for the plotting function)?
	# sorting order will be plotting order
	# -1 for NULL features (filtered out, or dropped units)
	# 0 for NA features
	# 1-n for features based on scale values (n for latest=plotted on top)
	# 1 for features with non-NA value for not-selected aes
	sortRev = if (plot.order$aes == "DATA") NULL else if (plot.order$aes == unm) plot.order$reverse else NA
	
	
	bypass_ord = plot.order$aes == "DATA"
	
	val = aes$value
	if (q$limitvars) val = val[1]

	with(q, {
		get_num_facets = function(bys) {
			k = as.integer(substr(bys, 3, 3))
			o$fn[k]
		}
		
		nvars = length(val) #m
		nvari = vapply(val, length, integer(1))
		
		vars = unlist(val, recursive = FALSE)
		
		# active grouping variables (to keep)
		grp_bv = by123__[sort(c({if (nvars > 1) v else integer(0)}, b))]
		
		sfun = paste0("tmapValuesScale_", unm)
		cfun = paste0("tmapValuesColorize_", unm)
		
		#print(vars)
		if (!aes$data_vars && !aes$geo_vars) {
			# cat("step2_grp_lyr_aes_const", unm," \n")
			# constant values (take first value (of possible tm_mv per facet)
			if (any(nvari) > 1) warning("Aesthetic values considered as direct visual variables, which cannot be used with tm_mv", call. = FALSE)
			val1 = sapply(vars, "[[", 1, USE.NAMES = FALSE)
			
			check_fun = paste0("tmapValuesCheck_", unm)
			if (!do.call(check_fun, list(x = val1))) {
				# to do: add "layer" name e.g. tm_fill is still "polygons" and not "fill"
				warning("Visual values used for the variable, \"", unm, "\" of layer function \"tm_", layer[1], "\" are incorrect.", call. = FALSE)
				warning("This error was converted to a warning for testing.")
				return(NULL)
			}
			
			val1 = do.call(sfun, list(x = val1, scale = o$scale))
			val1 = do.call(cfun, list(x = val1, pc = o$pc))
			
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
			if (!bypass_ord) dtl[, (nm__ord) := 1L]
			
			if (any(!dtl$sel__) || !q$drop.units) {
				# also needed for drop.units later on
				cls = data_class(dtl[[nm]])
				value.null = getAesOption("value.null", o, unm, layer, cls = cls)
				value.null = do.call(sfun, list(x = value.null, scale = o$scale))
				value.null = do.call(cfun, list(x = value.null, pc = o$pc))
				
				# todo: combine these:
				dtl[sel__==FALSE, (nm) := value.null]
				if (!bypass_ord) dtl[sel__==FALSE, (nm__ord) := -1L]
				
				if (!q$drop.units) {

					imp = structure(list(value.null, -1L, FALSE), names = c(nm, {if (bypass_ord) NULL else nm__ord}, "sel__"))
					levs = lapply(get_num_facets(grp_bv), seq.int, from = 1)
					names(levs) = grp_bv
					dtl = completeDT2(dtl, cols = c(list("tmapID__" = unique(dtl$tmapID__)), levs), defs = imp)
				}
				
			}

			dtl[, legnr := vector("integer", length = nrow(dtl))]

			
			dtl_leg = dtl[, .SD[1], by = c(grp_bv)][, tmapID__ := NULL][, legnr := (vapply(get(..nm), function(s) legend_save(list(vneutral = s)), FUN.VALUE = integer(1)))][, (nm) := NULL]
		} else {
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
			grp_bv_fr = by123__[sort(c({if (nvars > 1 && fr[v]) v else integer(0)}, intersect(which(fr), b)))]
			#grp_bv_fr = by123__[intersect(which(fr), b)]
			
			if (length(v) && fr[v] && !all(nvari == nvari[1])) stop("number of variables per aesthetic should be consistent when free = FALSE", call. = FALSE)
			
			# multiple variables
			if (nvars > 1) {
				if (limitvars) {
					#cat("step2_grp_lyr_aes_var_limiter\n")
					# not allowed: take first one
					warning(limitvars_warn, call. = FALSE)
					val = val[[1]]
					val_orig = val_orig[[1]]
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
						names(val) = val[1]
					} else {
						#cat("step2_grp_lyr_aes_var_multi_vars_free_scale\n")
						vars = vapply(val, "[[", character(1), 1)
						val = lapply(val, function(vl) {names(vl) = vl[1]; vl})
					}
				}
			} else {
				#cat("step2_grp_lyr_aes_var_one_var\n")
				val_name = names(val)[1]
				val = val[[1]]
				names(val) = val_name
				vars = vars[1] # only needed for update_fl?
			}
			
			if (length(v)) update_fl(k = v, lev = vars)
			
			apply_scale = function(s, l, v, varname, ordname, legname, sortRev, bypass_ord) {
				l = update_l(o = o, l = l, v = v, mfun = mfun)
		
				
				
				if (length(s) == 0) stop("mapping not implemented for aesthetic ", nm, call. = FALSE)
				f = s$FUN
				s$FUN = NULL
				# update label.format
				s$label.format = process_label_format(s$label.format, o$label.format)

				cls = data_class(dtl[[v[1]]])
				#if (is.na(s$legend$title)) s$legend$title = v
				
				# for bivariate legends
				if (length(v) > 1L) {
					cls2 = data_class(dtl[[v[2]]])
					if (is.ena(l$xlab)) l$xlab = paste0(v[2], attr(cls, "units"))
					if (is.ena(l$ylab)) l$ylab = paste0(v[1], attr(cls2, "units"))
					if (is.ena(l$title)) l$title = ""
				} else {
					if (length(l$title) > 1) {
						warning("This probably shouldn't happen. Please check code.")
					}
					# Error in if (is.ena(l$title)) l$title = paste0(names(v), attr(cls, "units")) : 
					# the condition has length > 1
					# Calls: <Anonymous> ... with -> with.default -> eval -> eval -> apply_scale
					# Detected in types of titles. # Example to illustrate the type of titles
					if (all(is.ena(l$title))) l$title = paste0(names(v), attr(cls, "units"))
				}
				
				
				
				#aesname = aes$aes
				value.null = if ("value.null" %in% names(s)) s$value.null else {
					vn = getAesOption("value.null", o, unm, layer, cls = cls)
					vn = do.call(sfun, list(x = vn, scale = o$scale))
					do.call(cfun, list(x = vn, pc = o$pc))
				}
				
				arglist = list(scale = s, legend = l, o = o, aes = unm, 
							   layer = layer, 
							   layer_args = args,
							   sortRev = sortRev, 
							   bypass_ord = bypass_ord,
							   submit_legend = TRUE)
				if (!all(dtl$sel__)) {
					if (bypass_ord) {
						dtl[, c(varname, legname) := list(value.null, 0L)]
					} else {
						dtl[, c(varname, ordname, legname) := list(value.null, -1L, 0L)]	
					}
					
					if (is.na(value.null)) stop("value.null not specified for aesthetic ", nm, call. = FALSE)
					if (bypass_ord) {
						dtl[sel__ == TRUE, c(varname, legname) := do.call(f, c(unname(.SD), arglist)), grp_b_fr, .SDcols = v]
					} else {
						dtl[sel__ == TRUE, c(varname, ordname, legname) := do.call(f, c(unname(.SD), arglist)), grp_b_fr, .SDcols = v]
					}
				} else {
					if (bypass_ord) {
						dtl[, c(varname, legname) := do.call(f, c(unname(.SD), arglist)), grp_b_fr, .SDcols = v]
					} else {
						dtl[, c(varname, ordname, legname) := do.call(f, c(unname(.SD), arglist)), grp_b_fr, .SDcols = v]
					}
				}
				if (!q$drop.units) {
					imp = structure(list(value.null, 0L, TRUE), names = c(nm, legname, "sel__"))
					levs = lapply(get_num_facets(grp_bv), seq.int, from = 1)
					names(levs) = grp_bv
					dtl = completeDT2(dtl, cols = c(list("tmapID__" = unique(dtl$tmapID__)), levs), defs = imp)
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
				ordnames = paste(nm__ord, 1L:nvars, sep = "_")
				legnames = paste("legnr", 1L:nvars, sep = "_")

				for (i in 1L:nvars) {
					dtl = apply_scale(scale[[i]], legend[[i]], val[[i]], varnames[[i]], ordnames[[i]], legnames[[i]], sortRev = sortRev, bypass_ord = bypass_ord)
				}
				
				
				
				dtl_leg = melt(dtl, id.vars = c("tmapID__", by__), measure.vars = legnames, variable.name = var__, value.name = "legnr")
				if (!bypass_ord) dtl_ord = melt(dtl, id.vars = c("tmapID__", by__), measure.vars = ordnames, variable.name = var__, value.name = nm__ord)
				dtl = melt(dtl, id.vars = c("tmapID__", by__), measure.vars = varnames, variable.name = var__, value.name = nm)
				if (!bypass_ord) dtl[, (nm__ord) := dtl_ord[[nm__ord]]]
				
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
				
				dtl = apply_scale(s, l, val, nm, nm__ord, "legnr", sortRev, bypass_ord)
				#sel = !vapply(dtl$legend, is.null, logical(1))
				dtl_leg = dtl[legnr != 0L, c(grp_bv_fr, "legnr"), with = FALSE]
			}
		}
		if (dev) timing_add(s4 = paste0("aes ", aes$aes))
		
		if (bypass_ord) {
			list(dt = dtl[, c("tmapID__", grp_bv, nm), with = FALSE],
				 leg = dtl_leg)
		} else {
			list(dt = dtl[, c("tmapID__", grp_bv, nm, nm__ord), with = FALSE],
				 leg = dtl_leg)
		}
	})
}
