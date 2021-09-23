step4_plot = function(tm) {
	tmx = tm$tmo
	o = tm$meta
	
	# get name of graphics engine (for function names e.g. tmapGridInit)
	gs = tmap_graphics_name()
	

	
	# collect legends
	dt_template = data.table::data.table(by1__ = integer(0), by2__ =  integer(0), by3__ =  integer(0), legend = list())
	legs = data.table::rbindlist(c(list(dt_template), lapply(tmx, function(tmxi) {
		data.table::rbindlist(lapply(tmxi$layers, function(tml) {
			
			legs_cached = get("legs", .TMAP)
			
			legs = c(tml$trans_legend, tml$mapping_legend)
			
			legs = lapply(legs, function(l) {
				l[, legend:=list(legs_cached[l$legnr])]
				l
			})
			
			legs2 = lapply(legs, function(legs_aes) {
				legs_aes$vneutral = unlist(lapply(legs_aes$legend, function(l) l$vneutral), use.names = FALSE)
				legs_aes
			})
			
			# find shared legends
			
			clones = vapply(legs2, function(l) {
				a = l$legend[[1]]$setup$aes
				if (!is.null(a)) a else ""
			}, FUN.VALUE = character(1))


			legs2b = mapply(function(l, lnm) {
				w = which(clones == lnm)
				if (length(w) > 0L) {
					legsclone = lapply(legs2[w], function(li) {
						li$legend
					})
					num_legends = vapply(legsclone, length, numeric(1))
					if (any(num_legends != length(l$legend))) warning("legends could not be shared; the aesthetics need the same .free specification", call. = FALSE)
					
					
					l$legend = do.call(mapply, args = c(list(FUN = function(l, ...) {
						clns = list(...)
						k = length(l$vvalues)
						l$clones = lapply(clns, function(cl) {
							vv = cl$vvalues
							if (k != length(vv)) stop("legends could not be shared; the number of legend items is different", call. = FALSE)
							vv
						})
						names(l$clones) = names(clones[w])
						l
					}, SIMPLIFY = FALSE), c(list(l$legend), legsclone)))
				}
				l
			}, legs2, names(legs2), SIMPLIFY = FALSE)
			
			
			copy_neutral = (length(legs) > 1)
			
			legs3 = mapply(function(legs_aes, legnm, i) {
				bvars = names(legs_aes)[substr(names(legs_aes), 1, 2) == "by"]

				if (!is.null(legs_aes)) {
					for (k in 1:nrow(legs_aes)) {
						if (length(legs_aes$legend[[k]]) == 1 || !legs_aes$legend[[k]]$setup$show) {
							legs_aes$legend[[k]] = list(NULL)
							next
						}
						
						bval = legs_aes[k, bvars, with = FALSE]
						gp = tml$gp
						
						# fill gp values with (scaled) aes data
						gpaid = which(paste0("__", legnm) == unlist(gp, use.names = FALSE))
						for (j in gpaid) gp[[j]] = legs_aes$legend[[k]]$vvalues
						
						# will gp with neutral values for other graphical variables
						if (copy_neutral) {
							nvalues = mapply(function(lclone, lname) {
								bvars2 = names(lclone)[substr(names(lclone), 1, 2) == "by"]
								
								bvars_int = intersect(bvars, bvars2)
								if (!length(bvars_int)) {
									lclone$vneutral[1]
								} else  {
									lclone[bval, on = bvars_int]$vneutral
								}
							}, legs2[-i], names(legs2[-i]), SIMPLIFY = FALSE)
							
							
							for (j in 1L:length(nvalues)) {
								gpid = which(paste0("__", names(nvalues)[j]) == sapply(gp, "[[", 1))
								for (jj in gpid) gp[[jj]] = nvalues[[j]]	
							}
							
						}
						
						
						leleg = legs_aes$legend[[k]]
						
						# get gp values from shared legends (slave)
						if ("clones" %in% names(leleg)) {
							clist = leleg$clones
							for (j in 1L:length(clist)) {
								cnm = names(clist)[j]
								gp[[cnm]] = clist[[j]]
							}
						}

						leleg$gp = gp
						leleg$vneutral = NULL
						leleg$vvalues = NULL
						
						if (length(leleg) == 1) {
							legs_aes$legend[[k]] = list(leleg)
						} else {
							legs_aes$legend[[k]] = leleg
						}
					}
				}
				legs_aes
			}, legs2b, names(legs2b), 1:length(legs2b), SIMPLIFY = FALSE)

			data.table::rbindlist(legs3, fill = TRUE)
		}), fill = TRUE)
	})), fill = TRUE)
	
	# remove empty legends
	legs = legs[vapply(legs$legend, length, FUN.VALUE = integer(1)) > 1, ][, vneutral := NULL]
	
	legs$class = lapply(legs$legend, function(l) l$setup$position$type)
	
	legs$h = lapply(legs$legend, function(l) l$setup$position$h)
	legs$v = lapply(legs$legend, function(l) l$setup$position$v)
	
	# legend.present.auto:
	#   find out whether there are legends for all facets, per row, per col
	#   use them to automatically determine meta.margins (in preprocess_meta)
	# # legend.present.fix
	#	find legend boxes that are assigned to outer margins
	if (nrow(legs) == 0) {
		o$legend.present.auto = c(all = FALSE, per_row = FALSE, per_col = FALSE)
		o$legend.present.fix = rep(FALSE, 4)
	} else {
		if (o$is.wrap) {
			#o$legend.present.auto = c(all = any(is.na(legs$by1__) & legs$class == "auto"), per_row = any(!is.na(legs$by1__) & legs$class == "auto"), per_col = FALSE)
			o$legend.present.auto = c(all = any(legs$class == "auto"), per_row = FALSE, per_col = FALSE)
		} else {
			o$legend.present.auto = c(all = any(is.na(legs$by1__) & is.na(legs$by2__) & legs$class == "auto"), per_row = any(!is.na(legs$by1__) & is.na(legs$by2__) & legs$class == "auto"), per_col = any(is.na(legs$by1__) & !is.na(legs$by2__) & legs$class == "auto"))
		}
		o$legend.present.fix = c(any(legs$class == "out" & legs$v == "bottom"), 
								 any(legs$class == "out" & legs$h == "left"),
								 any(legs$class == "out" & legs$v == "top"),
								 any(legs$class == "out" & legs$h == "right"))
	}
	
	
	
	o = preprocess_meta(o)
	
	get_shpTM = function(shpDT, by1, by2, by3) {
		b = list(by1, by2, by3)
		bynames = intersect(names(shpDT), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))
		
		sel = rep(TRUE, nrow(shpDT))
		if (length(bynames)) {
			for (i in 1L:length(bynames)) {
				sel = sel & shpDT[[bynames[i]]] %in% b[[byids[i]]]		
			}
		}
		shpDT$shpTM[which(sel)]
	}
	
	
	get_dt = function(dt, by1, by2, by3) {
		b = list(by1, by2, by3)
		bynames = intersect(names(dt), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))
		
		sel = rep(TRUE, nrow(dt))
		if (length(bynames)) {
			for (i in 1:length(bynames)) {
				sel = sel & dt[[bynames[i]]] %in% b[[byids[i]]]			
			}
		}
		dt[which(sel),]
	}
	
	tmain = tmx[[o$main]][[1]]
	
	
	d = data.table::data.table(do.call(expand.grid, lapply(structure(o$nby, names = c("by1", "by2", "by3")), seq_len)))
	d[, i := seq_len(nrow(d))]
	

	grps = c("by1", "by2", "by3")[o$free.coords]
	

	get_bbox = function(by1, by2, by3) {
		bbxs = lapply(tmain, function(tmi) {
			shpTM = get_shpTM(tmi$shpDT, by1, by2, by3)
			mdt = get_dt(tmi$mapping_dt, by1, by2, by3)
			bbxs2 = lapply(shpTM, stm_bbox, tmapID = mdt$tmapID__)
			bbx = stm_merge_bbox(bbxs2)
			if (is.na(bbx)) bbx else tmaptools::bb(bbx, asp.limit = 10)
		})
		list(list(bb_ext(stm_merge_bbox(bbxs), o$inner.margins)))
	}
	get_asp = function(bbxl) {
		vapply(bbxl, function(bbxi) {
			if (is.na(bbxi)) as.numeric(NA) else get_asp_ratio(bbxi)
		}, FUN.VALUE = numeric(1))
	}

		
	d[, bbox:=do.call(get_bbox, as.list(.SD)), by = grps, .SDcols = c("by1", "by2", "by3")]
	d[, asp:=get_asp(bbox)]
	
	#o$asp
	
	
	
	diff_asp = any(d$asp != d$asp[1])
	o$sasp = ifelse(diff_asp, NA, d$asp[1])

	o = process_meta(o)

	o$ng = length(tmx)

	if (o$panel.type == "xtab") {
		d[, row := as.integer((i - 1) %% o$nrows + 1)]
		d[, col := as.integer((((i - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
	} else {
		# wrap
		d[, col := as.integer((i - 1) %% o$ncols + 1)]
		d[, row := as.integer((((i - 1) %/% o$ncols + 1) - 1) %% o$nrows + 1)]
		
	}
	d[, page := as.integer(i - 1) %/% (o$nrows * o$ncols) + 1]
	
	
	FUNinit = paste0("tmap", gs, "Init")
	FUNrun = paste0("tmap", gs, "Run")
	FUNshape = paste0("tmap", gs, "Shape")
	FUNoverlay = paste0("tmap", gs, "Overlay")
	FUNwrap = paste0("tmap", gs, "Wrap")
	FUNxtab = paste0("tmap", gs, "Xtab")
	
	do.call(FUNinit, list(o = o))
	

	if (o$panel.type == "xtab") {
		for (k in 1:o$npages) {
			labrows = o$panel.labels[[1]]
			labcols = o$panel.labels[[2]]
			if (length(labrows) == o$nrows) for (i in 1:o$nrows) do.call(FUNxtab, list(label = labrows[i], facet_row = i, facet_page = k, o = o)) 
			if (length(labcols) == o$ncols) for (j in 1:o$ncols) do.call(FUNxtab, list(label = labcols[j], facet_col = j, facet_page = k, o = o)) 

		}
	}
	
	#d = d[!is.na(asp), ]

	for (i in seq_len(nrow(d))) {
 		bbx = d$bbox[[i]]
 		if (o$panel.type == "wrap") do.call(FUNwrap, list(label = o$panel.labels[[1]][d$i[i]], facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o)) 
 		if (!is.na(d$asp[i])) {
 			do.call(FUNshape, list(bbx = bbx, facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
			for (ig in 1L:o$ng) {
				tmxi = tmx[[ig]]
				nl = length(tmxi$layers)
				for (il in 1L:nl) {
	
					bl = tmxi$layers[[il]]
					shpTM = get_shpTM(bl$shpDT, d$by1[i], d$by2[i], d$by3[i])[[1]]
					mdt = get_dt(bl$mapping_dt, d$by1[i], d$by2[i], d$by3[i])
					
					id = paste0("f", sprintf("%03d", i), "g", sprintf("%02d", ig), "l", sprintf("%02d", il))
					
					if (nrow(mdt) != 0) {
						gp = bl$gp
						
						FUN = paste0("tmap", gs, bl$mapping_fun)
						
						do.call(FUN, list(shpTM = shpTM, dt = mdt, gp = gp, bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i], id = id, o = o))
					}
				}
				
			}
 		}
 		do.call(FUNoverlay, list(facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
	}

	toC = function(x) {
		paste(x, collapse = "_")
	}
	toI = function(x) {
		as.integer(strsplit(x, split = "_")[[1]])
	}
	
	


	
	legs[, ':='(facet_row = character(), facet_col = character())]
	legs$stack_auto = vapply(legs$legend, function(l) {
		s = l$setup$stack
		length(s) > 1
	}, FUN.VALUE = logical(1))
	legs$stack = vapply(legs$legend, function(l) {
		s = l$setup$stack
		if (length(s) > 1 && "manual" %in% names(s)) s["manual"] else s[1]
	}, FUN.VALUE = character(1))
	
	
	stacks = o$legend.stack

	# when facets are wrapped:
	if (o$is.wrap && o$n > 1) {
		if ((o$nrows > 1 && o$ncols > 1) || (o$nrows == 1 && o$legend.position.all$v == "center") || (o$ncols == 1 && o$legend.position.all$h == "center")) {
			# put all legends together (so ignoring col and row position) when 1) multiple rows and colums or 2) and 3) when facets for a row and there is still more place on the side than top/bottom (and likewise for one col)
			legs[class != "in", by1__ := NA]
			legs[class != "in", by2__ := NA]
		} else if (o$nrows == 1) {
			# -use by2 and not by1 when they form a row
			legs[, by2__ := by1__]
			legs[, by1__ := NA]
		} 
	}
			
	
	# update auto position (for 'all', 'rows', 'columns' legends)
	legs[is.na(by1__) & is.na(by2__) & class == "auto", ':='(h = o$legend.position.all$h, v = o$legend.position.all$v)]
	legs[!is.na(by1__) & is.na(by2__) & class == "auto", ':='(h = o$legend.position.sides$h, v = "by")]
	legs[is.na(by1__) & !is.na(by2__) & class == "auto", ':='(h = "by", v = o$legend.position.sides$v)]

	legs[is.na(by1__) & is.na(by2__) & class == "auto", ':='(stack = ifelse(stack_auto, ifelse(h == "center", stacks["per_row"], ifelse(v == "center", stacks["per_col"], stacks["all"])), stack))]
	legs[!is.na(by1__) & is.na(by2__) & class == "auto", ':='(stack = ifelse(stack_auto, stacks["per_row"], stack))]
	legs[is.na(by1__) & !is.na(by2__) & class == "auto", ':='(stack = ifelse(stack_auto, stacks["per_col"], stack))]
	
	
	legs[class == "auto", class := "out"]
	
	# manual outside legends -2 is top or left, -1 is bottom or right
	legs[class %in% c("auto", "out"), ':='(facet_row = ifelse(v == "center", toC(1:o$nrows), ifelse(v == "by", as.character(by1__), ifelse(v == "top", as.character(-2), as.character(-1)))),
										   facet_col = ifelse(h == "center", toC(1:o$ncols), ifelse(h == "by", as.character(by2__), ifelse(h == "left", as.character(-2), as.character(-1)))))]
	
	# manual in legends
	
	# find all facets
	
	
	
	is_in = legs$class == "in"
	if (any(is_in)) {
		legs_in = lapply(which(is_in), function(i) {
			d2 = data.table::copy(d)
			legsi = legs[i, ]
			if (is.na(legsi$by1__)) d2[, by1:= NA]
			if (is.na(legsi$by2__)) d2[, by2:= NA]
			if (is.na(legsi$by3__)) d2[, by3:= NA]
			legsi = merge(legsi, d2[, c("by1", "by2", "by3", "row", "col"), with = FALSE], by.x = c("by1__", "by2__", "by3__"), by.y = c("by1", "by2", "by3"))
			legsi[, ':='(facet_row = as.character(row), facet_col = as.character(col), row = NULL, col = NULL)]
			legsi
		})
		legs = data.table::rbindlist(c(list(legs[!is_in]), legs_in))
	}
	

	legfun = paste0("tmap", gs, "Legend")
	
	
	
	if (nrow(legs) > 0L) for (k in seq_len(o$npages)) {
		klegs = legs[is.na(by3__) | (by3__ == k), ]
		klegs[, do.call(legfun, args = list(legs = .SD$legend, o = o, facet_row = toI(.SD$facet_row[1]), facet_col = toI(.SD$facet_col[1]), facet_page = k, legend.stack = .SD$stack[1])), by = list(facet_row, facet_col), .SDcols = c("legend", "facet_row", "facet_col", "stack")]
	}
	
	do.call(FUNrun, list(o = o))
}
