# function to update the levels fl per facet dimension
update_grp_vars = function(lev = NULL, m = NULL) {
	vl = get("vl", envir = .TMAP)
	vn = get("vn", envir = .TMAP)

	if (!missing(lev)) {
		m = length(lev)
		if (m == 1L && vn == 1L) {
			if (is.null(vl)) {
				vl = lev
				vn = length(lev)
			}
		} else if (m > 1L && vn > 1L && m != vn) {
			stop("Inconsistent number of aesthetic variables.", call. = FALSE)
		} else if (m > vn) {
			vl = lev
			vn = length(lev)
		}
	} else if (!missing(m)) {
		if (m > 1L && vn > 1L && m != vn) {
			stop("Inconsistent number of aesthetic variables.", call. = FALSE)
		} else if (m > vn) {
			vn = m
			vl = NULL
		}
	}
	assign("vl", vl, envir = .TMAP)
	assign("vn", vn, envir = .TMAP)
}

add_used_vars = function(v) {
	used_vars = get("used_vars", envir = .TMAP)
	used_vars = unique(c(used_vars, v))
	assign("used_vars", used_vars, envir = .TMAP)
}

# ## estimate number of facets
step1_rearrange_facets = function(tmo) {
	o = tmap_options_mode()
	
	
	# get the final tm_faets object (ignoring group specific args: is.wrap, by, rows, columns, pages)
	
	#fl = list(1L, 1L, 1L)
	#assign("fl", fl, envir = .TMAP)
	
	#assign("fl", list(NULL, NULL, NULL), envir = .TMAP)
	#assign("fn", c(1L, 1L, 1L), envir = .TMAP)

	tmo = lapply(tmo, function(tmg) {
		
		shp = tmg$tms$shp
		smeta = tmapGetShapeMeta1(shp, c(o, tmg$tmf))

		assign("vl", NULL, envir = .TMAP)
		assign("vn", 1L, envir = .TMAP)
		
		assign("used_vars", character(0), envir = .TMAP)
		
		precheck_aes = function(a, layer, shpvars) {
			print(a$aes)
			within(a, {
				if (length(value) && is.na(value[[1]][1])) {
					# NA -> value.blank
					value = tmapVars(getAesOption("value.blank", o, aes = aes, layer = layer))
				}
				
				if (inherits(value, "tmapOption")) {
					value_orig = getAesOption(value[[1]], o, aes = aes, layer = layer)
					if (!is.list(value_orig)) value = list(value_orig)
					names(value) = sapply(value, "[", 1)
				} else if (inherits(value, "tmapShpVars")) {
					value = as.list(shpvars)
				} else {
					value_orig = value
					value = lapply(value_orig, make.names)
					names(value) = value_orig
					
					if (inherits(value_orig, "tmapAsIs")) {
						if (inherits(scale, "tm_scale_auto")) scale = tm_scale_asis()
					}
				}

				nvars = length(value) #m
				nvari = vapply(value, length, integer(1))
				
				vars = unlist(value)
				
				data_vars = all(make.names(vars) %in% shpvars)
				
				nflvar = nvars
				if (data_vars) {
					flvar = names(value)#vapply(value, "[[", 1, FUN.VALUE = character(1))
					update_grp_vars(lev = flvar)
					add_used_vars(vars)
				} else {
					value = value_orig
					update_grp_vars(m = nflvar)
				}
			})
		}
		
		
		# preprocess layers: check aes values
		tmg$tmls = lapply(tmg$tmls, function(tml) {
			within(tml, {
				if (length(trans.aes)) trans.aes = lapply(trans.aes, precheck_aes, layer = tml$layer, shpvars = smeta$vars)
				if (length(mapping.aes)) mapping.aes = lapply(mapping.aes, precheck_aes, layer = tml$layer, shpvars = smeta$vars)
			})
		})
	
		vl = get("vl", envir = .TMAP)
		vn = get("vn", envir = .TMAP)
		
		
		nrsd = length(smeta$dims) # number of required shape dimensions
		nrvd = as.integer(vn > 1L) # number of required variable dimensions (0 or 1)
		nrd = nrsd + nrvd # number of required by-dimensions

		
		tmg$tmf = within(tmg$tmf, {
			#fl = flvar
			#nfl = nflvar
			if (is.na(type)) type = if (nrd <= 1L) "wrapstack" else "grid"
			
			if (type %in% c("wrapstack", "wrap", "stack")) {
				by1 = by
				by2 = NULL
				by3 = NULL

				nsbd = as.integer(!is.null(by1) && !by1 == "VARS__" && !(by1 %in% smeta$dims)) # number of required by="varX" dimensions (0 or 1)
				
				
				
				if (nrd > 1L) {
					if (nrsd > 1L) stop("Cannot use tm_facets_wrap/tm_facets_stack, because there are several dimensions. Pleae use tm_facets_grid instead", call. = FALSE)
					# so there is exactly 1 shape dim
					nrvd = 0L
					nrd = 1L
					if (nsbd == 1L) {
						warning("by variable specified and multiple variables while there is a shape dimensions which cannot be ignored. The by variable will therefore be ignored. Also, only the first variable is shown. Use tm_facet_grid to show multiple variables", call. = FALSE)
					}
					by1 = NULL
					limitvars = TRUE
				} else if (nrd == 1L) {
					if (nrsd == 1L) {
						if (nsbd == 1L) {
							warning("by variable specified while there is a shape dimension which cannot be ignored. The by variable will therefore be ignored", call. = FALSE)
						}	
						by1 = NULL
						limitvars = FALSE
					} else {
						if (nsbd == 1L) {
							warning("by variable specified while there are multiple variables. Therefore, only the first variable is taken. Please use tm_facet_grid to combine multiple variables with a by variable", call. = FALSE)
							nrvd = 0L
							limitvars = TRUE
						} else {
							limitvars = FALSE
						}
					}
				} else {
					limitvars = FALSE
				}
				if (is.null(by1)) {
					by1 = if (nrsd == 1L) {
						smeta$dims[1]
					} else {
						"VARS__"
					}
				}
				
			} else {
				by1 = rows
				by2 = columns
				by3 = pages
				
				if (nrd > 3L) {
					if (nrsd > 3L) stop("The shape object has more than 3 dimensions, so even tm_facets_grid cannot be used.", call. = FALSE)
					nrvd = 0L
					nrd = 3L
					limitvars = TRUE
				} else {
					limitvars = FALSE
					#todo
					if (nrvd == 1L && !identical(by1, "VARS__") && !identical(by2, "VARS__") && !identical(by3, "VARS__")) {
						if (is.null(by1)) {
							by1 = "VARS__"
						} else if (is.null(by2)) {
							by2 = "VARS__"
						} else if (is.null(by3)) {
							by3 = "VARS__"
						}
					}
				}
				unsigned = setdiff(smeta$dims, c(by1, by2, by3))
				
				if (length(unsigned)) {
					if (is.null(by1)) by1 = unsigned[1] else if (is.null(by2)) by2 = unsigned[1] else by3 = unsigned[1] 
					if (length(unsigned) > 1) {
						if (is.null(by2)) by2 = unsigned[2] else by3 = unsigned[2] 	
					}
					if (length(unsigned) == 3) {
						by3 = unsigned[3]
					}
				}
				
			}
			
			bys = c(by1, by2, by3)
			if (length(bys)) {
				byvars = intersect(smeta$vars, bys)
				if (length(byvars)) add_used_vars(byvars)
			}
			if (!all(bys %in% c("VARS__", smeta$vars, smeta$dims))) stop("unknown facet variables", call. = FALSE)
		})

		smeta$vars = get("used_vars", envir = .TMAP)
		
		shp = tmapSubsetShp(shp, smeta$vars)
		
		smeta = tmapGetShapeMeta2(shp, smeta, c(o, tmg$tmf))
		
		
		
		tmg$tmf = within(tmg$tmf, {
			
			
			
			gl = list(NULL, NULL, NULL)
			gn = c(1L, 1L, 1L)

			# assign("gl", gl, envir = .TMAP)
			# assign("gn", gn, envir = .TMAP)
			# assign("gisf", is.wrap, envir = .TMAP)
			
			for (i in 1L:3L) {
				byi = get(paste0("by", i))
				if (!is.null(byi)) {
					if (byi == "VARS__") {
						if (!is.null(vl)) gl[[i]] = vl
						gn[i] = vn
					} else if (byi %in% smeta$vars) {
						gl[[i]] = smeta$vars_levs[[byi]]
						gn[i] = length(gl[[i]])
					} else if (byi %in% smeta$dims) {
						gl[[i]] = smeta$dims_val[[match(byi, smeta$dims)]]	
						gn[i] = length(gl[[i]])
					} 
				}
			}
			

			if (is.na(free.coords)) {
				if (type %in% c("wrapstack", "wrap", "stack")) {
					free.coords = rep(!any(c(by1, by2, by3) == "VARS__"), 3)
				} else {
					free.coords = c((!is.null(rows) && (rows != "VARS__")), (!is.null(columns)) && (columns != "VARS__"), (!is.null(pages)) && (pages != "VARS__"))
				}
			} else {
				free.coords = rep(free.coords, length.out = 3)
			}
			
			
			v = which(c(by1, by2, by3) == "VARS__")
			b = setdiff(which(!vapply(list(by1, by2, by3), is.null, FUN.VALUE = logical(1))), v)
			
			#n = length(v) + length(b)
			
			by123 = paste0("by", 1L:3L) 
			by123__ = paste0("by", 1L:3L, "__")
			
			var__ = by123__[v]
			by__ = by123__[b]
			
			
			#fl1 = 
		})
		tmg$tms$shp = shp
		tmg$tms$smeta = smeta

		tmg
	})

	tmf = get_tmf(lapply(tmo, function(tmoi) tmoi$tmf))
	tmo$tmf_global = tmf
	tmo
	
}

