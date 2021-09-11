# function to update the levels fl per facet dimension
update_fl = function(k, lev = NULL, m = NULL) {
	fl = get("fl", envir = .TMAP)
	# print("test")
	# print(fl)
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
	assign("fl", fl, envir = .TMAP)
}


step2_data_grp_prepare = function(tmf, dt) {
	### Specify 'by' variables
	if (tmf$is.wrap) {
		# facet wrap: only use by1
		by1 = tmf$by
		by2 = NULL
		by3 = NULL
		
		# By default, facets are created over the aes variables ("VARS__"). If wrap is specified in tm_facets, limit number of variables to 1.
		limitvars = (by1 != "VARS__")
		limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
	} else {
		# facet grid
		by1 = tmf$rows
		by2 = tmf$columns
		by3 = tmf$pages
		
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
	
	#n = length(v) + length(b)
	
	by123 = paste0("by", 1L:3L) 
	by123__ = paste0("by", 1L:3L, "__")
	
	var__ = by123__[v]
	by__ = by123__[b]
	
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
	
	
	list(v = v, b = b, by123__ = by123__, var__ = var__, by__ = by__, limitvars = limitvars, limitvars_warn = limitvars_warn)
	
}

preprocess_meta_step2 = function(meta) {
	within(meta, {
		
		pc = list(sepia.intensity=sepia.intensity, saturation=saturation)
		sepia.intensity = NULL
		saturation = NULL
		
		title.size = title.size * scale
		legend.title.size = legend.title.size * scale
		legend.text.size = legend.text.size * scale

		panel.label.size = panel.label.size * scale
		
		space.color = ifelse(is.null(space.color), bg.color, space.color[1])
		earth.boundary.color = ifelse(is.null(earth.boundary.color), attr.color, earth.boundary.color[1])
		legend.text.color =  ifelse(is.null(legend.text.color), attr.color, legend.text.color[1])
		legend.title.color = ifelse(is.null(legend.title.color), attr.color, legend.title.color[1])
		title.color = ifelse(is.null(title.color), attr.color, title.color[1])
		
		legend.inside.box = if (!is.logical(legend.frame)) TRUE else legend.frame
		if (identical(title.bg.color, TRUE)) title.bg.color = bg.color
		
		if (identical(frame, TRUE)) frame = attr.color else if (identical(frame, FALSE)) frame = NA 
		
		if (is.logical(legend.frame)) if (identical(legend.frame, TRUE)) legend.frame = attr.color else legend.frame = NA 
		# 		
		# 		between.margin.in <- convertHeight(unit(between.margin, "lines") * scale, "inch", valueOnly=TRUE)
		# 		
		# 		between.margin.y <-convertHeight(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$nrow
		# 		between.margin.x <-convertWidth(unit(between.margin.in, "inch"), "npc", valueOnly=TRUE) * gf$ncol
		# 		
		
		outer.margins <- rep(outer.margins, length.out = 4)

		if (is.list(inner.margins)) {
			inner.margins = lapply(inner.margins, rep, length.out = 4)
		}
		
		attr.color.light = is_light(attr.color)

		title.color = do.call("process_color", c(list(col=title.color), pc))
		main.title.color = do.call("process_color", c(list(col=main.title.color), pc))
		legend.text.color = do.call("process_color", c(list(col=legend.text.color), pc))
		legend.title.color = do.call("process_color", c(list(col=legend.title.color), pc))
		if (!is.na(frame)) frame = do.call("process_color", c(list(col=frame), pc))
		if (!is.na(legend.frame)) legend.frame = do.call("process_color", c(list(col=legend.frame), pc))
		
		panel.label.color = do.call("process_color", c(list(col=panel.label.color), pc))
		panel.label.bg.color = do.call("process_color", c(list(col=panel.label.bg.color), pc))
		
		earth.boundary.color = do.call("process_color", c(list(col=earth.boundary.color), pc))
		
		bg.color = do.call("process_color", c(list(col=bg.color), pc))
		
		if (!is.null(outer.bg.color)) outer.bg.color = do.call("process_color", c(list(col=outer.bg.color), pc))
		
		if (is.na(legend.bg.color)) legend.bg.color = !is.na(legend.frame)
		if (!is.na(legend.bg.color)) {
			legend.bg.color = if (identical(legend.bg.color, FALSE)) {
				NA
			} else if (identical(legend.bg.color, TRUE)) {
				bg.color
			} else {
				do.call("process_color", c(list(col=legend.bg.color, alpha=legend.bg.alpha), pc))				}
		} 
		if (!is.na(title.bg.color)) title.bg.color = do.call("process_color", c(list(col=title.bg.color, alpha=title.bg.alpha), pc))
		if (!is.na(earth.boundary.color)) earth.boundary.color = do.call("process_color", c(list(col=earth.boundary.color), pc))
		space.color = do.call("process_color", c(list(col=space.color), pc))
		
		earth.bounds = if (is.logical(earth.boundary)) {
			c(-180, -90, 180, 90)
		} else {
			as.vector(bb(earth.boundary))
		}
		earth.boundary = !identical(earth.boundary, FALSE)
		
		earth.boundary.lwd = earth.boundary.lwd * scale
		frame.lwd = frame.lwd * scale
		
		# set font face and family
		
		if (is.null(legend.title.fontface)) legend.title.fontface = fontface
		if (is.null(legend.title.fontfamily)) legend.title.fontfamily = fontfamily
		
		if (is.null(legend.text.fontface)) legend.text.fontface = fontface
		if (is.null(legend.text.fontfamily)) legend.text.fontfamily = fontfamily
		
		if (is.null(title.fontface)) title.fontface = fontface
		if (is.null(title.fontfamily)) title.fontfamily = fontfamily
		
		if (is.null(main.title.fontface)) main.title.fontface = fontface
		if (is.null(main.title.fontfamily)) main.title.fontfamily = fontfamily
		
		if (is.null(panel.label.fontface)) panel.label.fontface = fontface
		if (is.null(panel.label.fontfamily)) panel.label.fontfamily = fontfamily
		

	})
}


step2_data = function(tm) {
	
	tmo = tm$tmo
	meta = tm$meta
	
	
	meta = preprocess_meta_step2(meta)
	
	groupnames = paste0("group", seq_along(tmo))
	fl = list(1L, 1L, 1L)
	assign("fl", fl, envir = .TMAP)

	grps = lapply(tmo, function(tmg) {
		#cat("step2_grp==================================\n")
		dt = tmg$tms$dt
		
		# step2_data_grp_prepare
		tmf_meta = step2_data_grp_prepare(tmg$tmf, dt)
		v = tmf_meta$v
		b = tmf_meta$b
		by123__ = tmf_meta$by123__
		var__ = tmf_meta$var__
		by__ = tmf_meta$by__
		limitvars = tmf_meta$limitvars
		limitvars_warn = tmf_meta$limitvars_warn

		layernames = paste0("layer", seq_along(tmg$tmls))
		lrs = lapply(tmg$tmls, function(tml) {
			#cat("step2_grp_lyr======================\n")
			getdts = function(aes, nm, p) {
				#cat("step2_grp_lyr_aes_", nm, "---------\n")
				#print(paste0("name ", nm))
				#print(paste0("aes ", aes$aes))
				
				nm = aes$aes
				
				val = aes$value

				if (inherits(val, "tmapOption")) val = getAesOption(val[[1]], meta, aes = aes$aes, layer = tml$layer)
				
				
				nvars = length(val) #m
				nvari = vapply(val, length, integer(1))
				
				vars = unlist(val)
				
				# active grouping variables (to keep)
				grp_bv = by123__[sort(c({if (nvars > 1) v else integer(0)}, b))]
				
				
				#print(vars)

				if (!all(vars %in% names(dt))) {
					#cat("step2_grp_lyr_aes_const\n")
					# constant values (take first value (of possible MV) per facet)
					if (any(nvari) > 1) warning("Aesthetic values considered as direct visual variables, which cannot be used with MV", call. = FALSE)
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
					if (any(!dtl$sel__) || !tmg$tmf$drop.units) {
						# also needed for drop.units later on
						cls = data_class(dtl[[nm]])
						value.null = getAesOption("value.null", meta, aes$aes, tml$layer, cls = cls)
						
						dtl[sel__==FALSE, (nm) := value.null]
						
						if (!tmg$tmf$drop.units) {
							imp = structure(value.null, names = nm)
							dtl = completeDT(dtl, cols = c("tmapID__", grp_bv), defs = imp)
						}
						
					}
					#dtl[, sel__:= NULL]
					

					dtl[, legend := vector("list", length = nrow(dtl))]
					#dtl_leg = NULL
					#sel = !vapply(dtl$legend, is.null, logical(1))
					dtl_leg = dtl[, .SD[1], by = c(grp_bv)][, tmapID__ := NULL][, legend := list(lapply(get(..nm), function(s) list(vneutral = s)))][, (nm) := NULL]

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
						tmp = names(meta)[substr(names(meta), 1, 6) == "legend"]
						
						# update legend format
						l$format = process_legend_format(l$format, meta$legend.format)
						
						# update other legend options
						opt_leg = setdiff(intersect(substr(tmp, 8, nchar(tmp)), names(l)), "format")
						l[opt_leg] = mapply(function(o, nm) {
							if (is.na(o[1])) meta[[paste0("legend.", nm)]] else o
						}, l[opt_leg], opt_leg, SIMPLIFY = FALSE)
						
						if (length(s) == 0) stop("mapping not implemented for aesthetic ", nm, call. = FALSE)
						f = s$FUN
						s$FUN = NULL
						#if (is.na(s$legend$title)) s$legend$title = v
						if (is.na(l$title)) l$title = v
						#aesname = aes$aes
						value.null = if ("value.null" %in% names(s)) s$value.null else {
							cls = data_class(dtl[[v[1]]])
							getAesOption("value.null", meta, aes$aes, tml$layer, cls = cls)
						}
						if (!all(dtl$sel__)) {
							dtl[, c(varname, legname) := list(value.null, list(NULL))]
							if (is.na(value.null)) stop("value.null not specified for aesthetic ", nm, call. = FALSE)
							dtl[sel__ == TRUE, c(varname, legname) := do.call(f, c(unname(.SD), list(scale = s, legend = l, opt = meta, aes = aes$aes, layer = tml$layer, p = names(p)[match(paste0("__", aes$aes), p)]))), grp_b_fr, .SDcols = v]
						} else {
							dtl[, c(varname, legname) := do.call(f, c(unname(.SD), list(scale = s, legend = l, opt = meta, aes = aes$aes, layer = tml$layer, p = names(p)[match(paste0("__", aes$aes), p)]))), grp_b_fr, .SDcols = v]
						}
						
						if (!tmg$tmf$drop.units) {
							imp = structure(list(value.null, list()), names = c(nm, "legend"))
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
						legnames = paste("legend", 1L:nvars, sep = "_")
						#mapply(apply_scale, scale, legend, val, varnames, legnames)
						
						for (i in 1L:nvars) {
							#mapply(apply_scale, scale, legend, val, varnames, legnames) does not work because of completeDT
							dtl = apply_scale(scale[[i]], legend[[i]], val[[i]], varnames[[i]], legnames[[i]])
						}
						
						
						
						dtl_leg = melt(dtl, id.vars = c("tmapID__", by__), measure.vars = legnames, variable.name = var__, value.name = "legend")
						dtl = melt(dtl, id.vars = c("tmapID__", by__), measure.vars = varnames, variable.name = var__, value.name = nm)
						
						dtl[, (var__) := as.integer(get(..var__))]
						dtl_leg[, (var__) := as.integer(get(..var__))]
						
						sel = !vapply(dtl_leg$legend, is.null, logical(1))
						dtl_leg = dtl_leg[sel, c(grp_bv_fr, "legend"), with = FALSE]
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
						
						dtl = apply_scale(s, l, val, nm, "legend")
						
						sel = !vapply(dtl$legend, is.null, logical(1))
						dtl_leg = dtl[sel, c(grp_bv_fr, "legend"), with = FALSE]
					}
				}
				
				
				# if (!tmg$tmf$drop.units) {
				# 	imp = structure(list(value.null, list()), names = c(nm, "legend"))
				# 	completeDT(dtl, cols = c("tmapID__", grp_bv), defs = imp)
				# }
				

				list(dt = dtl[, c("tmapID__", grp_bv, nm), with = FALSE],
					 leg = dtl_leg)
			}
			
			gp = tml$gpar
			tp = tml$tpar
			
			
			#cat("step2_grp_lyr_trans_______________\n")
			
			trans = mapply(getdts, tml$trans.aes, names(tml$trans.aes), SIMPLIFY = FALSE, MoreArgs = list(p = tp))
			
			#cat("step2_grp_lyr_mapping_____________\n")
			
			mapping = mapply(getdts, tml$mapping.aes, names(tml$mapping.aes), SIMPLIFY = FALSE, MoreArgs = list(p = gp))
			
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
				 mapping_fun = tml$mapping.fun,
				 gp = gp,
				 tp = tp)
		})
		names(lrs) = layernames
		
		shpDT = data.table(shpTM = list(list(shp = tmg$tms$shp, tmapID = dt$tmapID__)))
		list(layers = lrs, shpDT = shpDT)
	})
	names(grps) = groupnames
	#attr(grps, "fl") = fl
	#attr(grps, "main") = attr(tmo, "main")
	#attr(grps, "crs") = attr(tmo, "crs")
	
	tmf = get_tmf(lapply(tmo, function(tmoi) tmoi$tmf))
	
	tmf$fl = get("fl", envir = .TMAP)
	
	meta = c(meta, tmf)
	
	# attr(grps, "is.wrap") = tmo[[1]]$tmf$is.wrap
	# attr(grps, "nrows") = tmo[[1]]$tmf$nrows
	# attr(grps, "ncols") = tmo[[1]]$tmf$ncols
	
	list(tmo = grps, meta = meta)
}


get_tmf = function(tmfs) {
	# Get tmf object: start with the one that is called, and add 'calls'
	
	nf = length(tmfs)
	
	# find first tmf that has been called
	fid = which(vapply(tmfs, function(tmf){
		"calls" %in% names(tmf)
	}, FUN.VALUE = logical(1)))[1]
	
	if (is.na(fid)) fid = 1L
	
	tmf = tmfs[[fid]]
	
	if (fid < nf) {
		for (i in (fid+1):nf) {
			args = tmfs[[i]]$calls
			tmf[args] = tmfs[[i]][args]
		}
	}
	tmf
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
