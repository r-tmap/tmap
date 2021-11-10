step1_rearrange = function(tmel) {
	# find shape, (aesthetic) layer, facet, and other elements
	is_tms = vapply(tmel, inherits, "tm_shape", FUN.VALUE = logical(1))
	is_tml = vapply(tmel, inherits, "tm_layer", FUN.VALUE = logical(1))
	is_tmf = vapply(tmel, inherits, "tm_facets", FUN.VALUE = logical(1))
	is_other = !is_tml & !is_tms & !is_tmf

	is_aux = vapply(tmel, inherits, "tm_aux_layer", FUN.VALUE = logical(1))
	
	# find layer id numbers (needed to plot layers in correct order, which is not trivial due to the two layer types)
	lay_id = cumsum(is_tml | is_aux)
	lay_id[!is_tml & !is_aux] = 0L
	
	# create groups, for each group: tms (tmap shape), tmls (tmap layers), tmf (tmap facets)
	ids = cumsum(is_tms)
	ids[is_other] = 0L
	tmel_spl = split(tmel, f = ids)
	lay_id_spl = split(lay_id, f = ids)
	if (any(is_other)) {
		oth = tmel_spl[[1]]
		oth_lay_id = lay_id_spl[[1]]
		tmel_spl = tmel_spl[-1]
		lay_id_spl = lay_id_spl[-1]
	} else {
		oth = list()
		oth_lay_id = list()
	}
	
	# organize groups, 1 tm_shape, at least 1 tm_layers, 1 tm_facets
	tmo = mapply(function(tmg, lid) {
		is_tms = vapply(tmg, inherits, "tm_shape", FUN.VALUE = logical(1))
		is_tml = vapply(tmg, inherits, "tm_layer", FUN.VALUE = logical(1))
		is_tmf = vapply(tmg, inherits, "tm_facets", FUN.VALUE = logical(1))
		
		# make sure there is exactly one tm_facets per group (if there are none, add one, if there are mutple, take last)
		if (!any(is_tmf)) {
			tmf = tm_facets()[[1]]
		} else {
			# get last tm_facets element
			k = sum(is_tmf)
			if (k < 1) warning("Multiple tm_facets defined per layer group. Only the last one is processed", call. = FALSE)
			tmf = tmg[[which(is_tmf)[k]]]
		}
		
		# extract layers and add layer id number
		tmls = mapply(function(l, i) {
			l$lid = i
			l
		}, tmg[is_tml], lid[is_tml], SIMPLIFY = FALSE)
		
		structure(list(tms = tmg[[1]], tmls = tmls, tmf = tmf), class = c("tmapGroup", "list"))
	}, tmel_spl, lay_id_spl, SIMPLIFY = FALSE)
	
	
	is_aux = vapply(oth, inherits, "tm_aux_layer", FUN.VALUE = logical(1))
	
	if (any(is_aux)) {
		aux = mapply(function(l, i) {
			l$lid = i
			l
		}, oth[is_aux], oth_lay_id[is_aux], SIMPLIFY = FALSE)
	} else {
		aux = list()
	}
	
	# get the final tm_faets object (ignoring group specific args: is.wrap, by, rows, columns, pages)
	tmf = get_tmf(lapply(tmo, function(tmoi) tmoi$tmf))
	
	
	# get options (mode specific)
	opt = tmap_options_mode()
	
	
	# ## estimate number of facets
	tmo = lapply(tmo, function(tmg) {
		
		shp = tmg$tms$shp
		smeta = tmapGetShapeMeta(shp)
		
		
		
		precheck_aes = function(a, layer, shpvars) {
			within(a, {
				if (inherits(value, "tmapOption")) value = getAesOption(value[[1]], opt, aes = aes, layer = layer)
				if (inherits(value, "tmapShpVars")) value = as.list(shpvars)
				
				nvars = length(value) #m
				nvari = vapply(value, length, integer(1))
				
				vars = unlist(value)
				
				data_vars = (all(vars %in% shpvars))
				
				nfl = nvars
				if (data_vars) {
					fl = vapply(value, "[[", 1, FUN.VALUE = character(1))
				} else {
					fl = NULL
				}
			})
		}
		
		
		
		
		# preprocess layers: check aes values
		tmg$tmls = lapply(tmg$tmls, function(tml) {
			within(tml, {
				if (length(trans.aes)) trans.aes = lapply(trans.aes, precheck_aes, layer = tml$layer, shpvars = smeta$vars)
				if (length(mapping.aes)) mapping.aes = lapply(mapping.aes, precheck_aes, layer = tml$layer, shpvars = smeta$vars)
				
				fl = NULL
				nfl = 1L
				
				for (a in c(trans.aes, mapping.aes)) {
					if (a$nfl > nfl) {
						fl = a$fl
						nfl = a$nfl
					} else if (a$nfl == nfl && is.null(fl) && !is.null(a$fl)) {
						fl = a$fl
					}
				}
				rm(a)
				
			})
		})
		
		fl = NULL
		nfl = 1L
		
		for (tml in tmg$tmls) {
			if (tml$nfl > nfl) {
				fl = tml$fl
				nfl = tml$nfl
			} else if (tml$nfl == nfl && is.null(fl) && !is.null(tml$fl)) {
				fl = tml$fl
			}
		}
		
		if (is.null(fl)) fl = as.character(seq_len(nfl))
		

		
		# determine number of variables per aesthetic

		
		nrsd = length(smeta$dims) # number of required shape dimensions
		nrvd = as.integer(nfl > 1L) # number of required variable dimensions (0 or 1)
		nrd = nrsd + nrvd
				

			
			# number of specified by dimensions
			
		
		tmg$tmf = within(tmg$tmf, {
			fl = fl
			nfl = nfl
			if (is.na(is.wrap)) is.wrap = (nrd <= 1L)
			
			if (is.wrap) {
				by1 = by
				by2 = NULL
				by3 = NULL
				
				
				nsbd = as.integer(!is.null(by1) && !by1 == "VARS__" && !(by1 %in% smeta$dims))
					
				
				if (nrd > 1L) {
					if (nrsd > 1L) stop("Cannot use tm_facets_wrap, because there are several dimensions. Pleae use tm_facets_grid instead", call. = FALSE)
					warning("")
					nrvd = 0L
					nrd = 1L
					if (nsbd == 1L) {
						warning("by variable specified while there are shape dimensions which cannot be ignored", call. = FALSE)
						by1 = NULL
					}
					limitvars = TRUE
				} else if (nrd == 1L) {
					if (nrsd == 1L) {
						if (nsbd == 1L) {
							warning("by variable specified while there are shape dimensions which cannot be ignored", call. = FALSE)
							by1 = NULL
						}	
						limitvars = FALSE
					} else {
						if (nsbd == 1L) {
							warning("by variable specified while there are shape dimensions which cannot be ignored", call. = FALSE)
							nrvd = 0L
							limitvars = TRUE
						} else {
							limitvars = FALSE
						}
					}
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
			
			cat("by1 ", by1, "\n")
			cat("by2 ", by2, "\n")
			cat("by3 ", by3, "\n")
			
			cat("fl ", fl, "\n")
			cat("nfl ", nfl, "\n")
			
			cat("limitvars ", limitvars, "\n")
			
			if (is.na(free.coords)) {
				if (is.wrap) {
					free.coords = rep((by != "VARS__"), 3)
				} else {
					free.coords = c((!is.null(rows) && (rows != "VARS__")), (!is.null(columns)) && (columns != "VARS__"), (!is.null(pages)) && (pages != "VARS__"))
				}
			} else {
				free.coords = rep(free.coords, length.out = 3)
			}
			
			#fl1 = 
			
			
		})
		
		# if (tmf$is.wrap) {
		# 	# facet wrap: only use by1
		# 	by1 = tmf$by
		# 	by2 = NULL
		# 	by3 = NULL
		# 	
		# 	# By default, facets are created over the aes variables ("VARS__"). If wrap is specified in tm_facets, limit number of variables to 1.
		# 	limitvars = (by1 != "VARS__")
		# 	limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
		# } else {
		# 	# facet grid
		# 	by1 = tmf$rows
		# 	by2 = tmf$columns
		# 	by3 = tmf$pages
		# 	
		# 	## Try to assign VARS__ to one dimension. If not possible, limit number of variables to 1.
		# 	limitvars = FALSE
		# 	if (!identical(by1, "VARS__") && !identical(by2, "VARS__") && !identical(by3, "VARS__")) {
		# 		if (is.null(by1)) {
		# 			by1 = "VARS__"
		# 		} else if (is.null(by2)) {
		# 			by2 = "VARS__"
		# 		} else if (is.null(by3)) {
		# 			by3 = "VARS__"
		# 		} else {
		# 			limitvars = TRUE
		# 		}
		# 	}
		# 	limitvars_warn = "Multiple variables have been specified in a layer function. However, since the 'by' argument of tm_facets_wrap has been specified, only the first variable is used"
		# }

		tmg
	})
	
	
# 
# 	fl  = list(1L, 1L, 1L)
# 	for (tmg in tmo) {
# 		
# 	}
	 
	
	# find the 'main' group: this is the group for which tm_shape is used for CRS and bbox. By default, take the first, unless is.main is set to TRUE.
	# is.main can be set multiple times: the CRS will be taken from the first, but the bbox from all
	ids = get_main_ids(tmo)
	
	
	# get main crs (option or extracted from first main shape)
	crs_option = opt$crs
	tms = tmo[[ids[1]]]$tms
	crs = if (is.na(crs_option[1])) get_crs(tms) else crs_option
	main_class = get_class(tms)

	# update options with tm_option elements
	is_opt = sapply(oth, inherits, "tm_options")
	if (any(is_opt)) for (id in which(is_opt)) {
		nms = intersect(names(opt), names(oth[[id]]))
		if (length(nms)) opt[nms] = oth[[id]][nms]
	}

	# to be used later
	opt$main = ids # to determine total bounding box in step 4
	opt$main_class = main_class # inner.margins are class specific (preprecess_meta)
	opt$crs = crs # in step 3, when other shapes are transformed to this crs
	
	# process shapes: put non-spatial data in data.table, keep spatial data separately 
	tmo = structure(lapply(tmo, function(tmg) {
		tmg$tms = do.call(tmapShape, c(tmg$tms, list(o = opt)))
		tmg
	}), names = paste0("group", seq_len(length(tmo))), class = c("tmapObject", "list"))
	
	list(tmo = tmo, aux = aux, meta = opt)
}

# see above
get_main_ids = function(tmo) {
	is_main = vapply(tmo, function(tmg) {
		identical(tmg$tms$is.main, TRUE)
	}, FUN.VALUE = logical(1), USE.NAMES = FALSE)
	
	if (any(is_main)) which(is_main) else 1L
}


get_crs = function(tms) {
	if (is.null(tms$crs)) sf::st_crs(tms$shp) else tms$crs
}

get_class = function(tms) {
	class(tms$shp)
}


