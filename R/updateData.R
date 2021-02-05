updateData = function(tmo) {
	groupnames = paste0("group", seq_along(tmo))
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
		
		# create byx__ columns for which_by_spec
		if (any(which_by_spec)) {
			for (w in which_by_spec) {
				byvar = get(paste0("by", w))
				byname = paste0("by", w, "__")
				dt[, (byname) := factor(get(..byvar))]
			}
		}

		# create dummy byx__ columns for !which_by_spec
		if (ndims < 3) {
			for (w in setdiff(1L:3L, which_by_spec)) {
				byname = paste0("by", w, "__")
				dt[, (byname) := factor(1L, levels = 1L)]
			}
		}
		
		bys = paste0("by", 1L:3L, "__")
		byv = bys[setdiff(1L:3L, which_by_vars)]
		
		layernames = paste0("layer", seq_along(tmg$tmls))
		lrs = lapply(tmg$tmls, function(tml) {
			print("layer")
			if (length(tml$trans.aes)) {
				aesnames = paste0("trans_", names(tml$trans.aes))
			} else {
				aesnames = character(0)	
			}
			aesnames = c(aesnames, paste0("mapping_", names(tml$mapping.aes)))
			aess = mapply(function(aes, nm) {
				print(paste("aes", nm))
				
				#if (nm == "mapping_fill") browser()
				
				val = aes$value
				nvars = length(aes$value) #m
				nvari = vapply(val, length, integer(1))
				
				vars = unlist(aes$value)

				#browser()
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
					dtl = melt(dtl, id.vars = c("tmapID__", byv), measure.vars = vnames, variable.name = paste0("by", which_by_vars, "__"), value.name = nm)
					dtl[, legend := vector("list", length = nrow(dtl))]
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
						
						byvarname = paste0("by", which_by_vars, "__")
						dtl[, (byvarname) := NULL]
						
						dtl_leg = melt(dtl, id.vars = c("tmapID__", byv), measure.vars = legnames, variable.name = byvarname, value.name = "legend")
						dtl = melt(dtl, id.vars = c("tmapID__", byv), measure.vars = varnames, variable.name = byvarname, value.name = nm)
						levels(dtl[[byvarname]]) = vapply(val, "[", character(1), 1)
						levels(dtl_leg[[byvarname]]) = vapply(val, "[", character(1), 1)
						dtl$legend = dtl_leg$legend
					} else {
						if (inherits(aes$setup, "tm_aes")) {
							s = aes$setup
						} else {
							s = aes$setup[[1]]
						}
						f = s$FUN
						s$FUN = NULL
						dtl[, c(nm, "legend") := do.call(f, c(unname(.SD), list(setup = s))), grp, .SDcols = val]
					}
				}
				dtl[, c("tmapID__", bys, nm, "legend"), with = FALSE]
			}, c(tml$trans.aes, tml$mapping.aes), aesnames, SIMPLIFY = FALSE)
			names(aess) = aesnames
			aess
		})
		names(lrs) = layernames
		lrs
	})
	names(grps) = groupnames
	grps
}
