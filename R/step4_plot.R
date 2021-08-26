get_nby = function(fl) {
	vapply(fl, function(f) {
		if (is.integer(f)) f else length(f)	
	}, integer(1))
}

get_row = function(i, nrows) {
	as.integer((i - 1) %% nrows + 1)
}

get_col = function(i, nrows, ncols) {
	as.integer((((i - 1) %/% nrows + 1) - 1) %% ncols + 1)
}

get_page = function(i, nrows, ncols) {
	as.integer(i - 1) %/% (nrows * ncols) + 1
}

get_i = function(ir, ic, ip, nby) {
	ir + (ic - 1) * nby[1] + (ip - 1) * prod(nby[1:2])
}

preprocess_meta = function(o) {
	within(o, {
		nby = get_nby(fl)
		n = prod(nby)
		if (is.na(panel.type)) panel.type = ifelse(n == 1 || (is.wrap && !is.character(fl[[1]])) || (!is.wrap && !is.character(fl[[1]]) && !is.character(fl[[2]])), "none", ifelse(is.wrap, "wrap", "xtab"))
		
		inner.margins = get_option_class(inner.margins, class = main_class)
		
	})
	
}

process_meta = function(o) {
	within(o, {
		
		devsize = dev.size()
		dasp = devsize[1] / devsize[2]
		
		
		# needed for spnc viewport (to retain aspect ratio)
		if (dasp > 1) {
			cw = dasp
			ch = 1
		} else {
			ch = 1/dasp
			cw = 1
		}
		
		lineH = grid::convertHeight(grid::unit(1, "lines"), unitTo = "npc", valueOnly = TRUE)
		lineW = grid::convertWidth(grid::unit(1, "lines"), unitTo = "npc", valueOnly = TRUE)
		
		bufferH = lineH / 2
		bufferW = lineW / 2
		
		# calculate space for margins, panels, etc
		
		meta.automatic = is.na(meta.margins[1])
		
		#one.row = (!is.na(o$nrows) && o$nrows == 1)
		#one.col = (!is.na(o$ncols) && o$ncols == 1)
		
		if (meta.automatic) meta.margins = c(0, 0, 0, 0) else meta.margins = rep(meta.margins, length.out = 4)

		meta.buffers = sign(meta.margins) * c(bufferH, bufferW, bufferH, bufferW) # outside and inside
		
		panel.xtab.size = if (panel.type == "xtab") {
			c(ifelse("bottom" %in% panel.xtab.pos, panel.label.height * lineH, 0),
			  ifelse("left" %in% panel.xtab.pos, panel.label.height * lineW, 0),
			  ifelse("top" %in% panel.xtab.pos, panel.label.height * lineH, 0),
			  ifelse("right" %in% panel.xtab.pos, panel.label.height * lineW, 0))
		} else c(0, 0, 0, 0)
		
		panel.wrap.size = if (panel.type == "wrap") {
			c(ifelse(panel.wrap.pos == "bottom", panel.label.height * lineH, 0),
			  ifelse(panel.wrap.pos == "left", panel.label.height * lineW, 0),
			  ifelse(panel.wrap.pos == "top", panel.label.height * lineH, 0),
			  ifelse(panel.wrap.pos == "right", panel.label.height * lineW, 0))
		} else c(0, 0, 0, 0)
		
		xylab.margins = rep(0, 4)
		if (xlab.show) xylab.margins[ifelse(xlab.pos == "bottom", 1, 3)] = xylab.height * lineH
		if (ylab.show) xylab.margins[ifelse(xlab.pos == "left", 2, 4)] = xylab.height * lineW
		
		
		grid.buffers = if (grid.show) {
			as.integer(grid.label.pos == c("bottom", "left", "top", "right")) * c(bufferH, bufferW, bufferH, bufferW)
		} else {
			rep(0, 4)
		}
		
		grid.margins = if (grid.show) {
			as.integer(grid.label.pos == c("bottom", "left", "top", "right")) * grid.mark.height * c(lineH, lineW, lineH, lineW)
		} else {
			rep(0, 4)
		}
		between.marginH = between.margin * lineH
		between.marginW = between.margin * lineW
		
		fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
		

	
		# prefered aspect ratio (just for this function): if asp is defined (not 0 or NA), use that, otherwise use sasp (shape asp) if available (if not; 1)
		pasp = if (is.na(sasp)) {
			if (!is.na(asp) && asp > 0) {
				asp
			} else {
				1
			}
		} else {
			if (!is.na(asp) && asp > 0) {
				asp
			} else {
				sasp
			}
		}
		
		masp = ((1 - sum(fixedMargins[c(2, 4)])) / (1 - sum(fixedMargins[c(1, 3)]))) * dasp
		
		
		
		
		# determine where to place automatic legends (i.e. legends with local legend.position = NA and with legend.position = tm_lp_auto() enabled)
		# this is also neede to find out which margins are taken from meta.auto.margins
		
		legend.position.sides = legend.position
		legend.position.all = legend.position
			
		## find position for all-facet legend
		
		if (legend.present.auto[1]) {
			if (!legend.present.auto[2] & !legend.present.auto[3]) {
				# only 'all facets' legends (either bottom or right)
				if ((n == 1 && pasp > masp) || (n > 1 && masp < 1) || (identical(nrows, 1) || (!is.na(ncols) && ncols >= n))) { # || one.row
					legend.position.all = list(h = "center", v = legend.position$v)
				} else {
					legend.position.all = list(h = legend.position$h, v = "center")
				}
			} else if (legend.present.auto[2] & !legend.present.auto[3]) {
				# central goes center bottom 
				legend.position.all = list(h = "center", v = legend.position$v)
			} else if (!legend.present.auto[2] & legend.present.auto[3]) {
				# central goes right center 
				legend.position.all = list(h = legend.position$h, v = "center")
			}
		}
		
		
		margins.used.all = c(legend.position.all$v == "bottom",
							 legend.position.all$h == "left",
							 legend.position.all$v == "top",
							 legend.position.all$h == "right") * legend.present.auto[1]
		
		margins.used.sides = c(legend.position.sides$v == "bottom",
							   legend.position.sides$h == "left",
							   legend.position.sides$v == "top",
							   legend.position.sides$h == "right") * legend.present.auto[c(3,2,3,2)]
		
		
		margins.used =  margins.used.all | margins.used.sides | legend.present.fix
		
		if (meta.automatic && any(margins.used)) {
			meta.auto.margins = rep(meta.auto.margins, length.out = 4)
			if (all(!margins.used[c(1,3)]) && n == 1) {
				# auto adjust left/right
				meta.margins[margins.used] =  pmax(meta.auto.margins[margins.used], (1 - pasp/masp - 2*bufferW)) / sum(margins.used)
			} else if (all(!margins.used[c(2,4)]) && n == 1) {
				# auto adjust top/right
				meta.margins[margins.used] =  pmax(meta.auto.margins[margins.used], (1 - masp/pasp - 2*bufferH)) / sum(margins.used) } 
			else {
				meta.margins[margins.used] = meta.auto.margins[margins.used]
			}
			
			# redo calculations
			meta.buffers = sign(meta.margins) * c(bufferH, bufferW, bufferH, bufferW) # outside and inside
			fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
		}
		

		

		# determine number of rows and cols
		if (!is.wrap) {
			nrows = nby[1]
			ncols = nby[2]
		} else {
			
			if (is.na(nrows) && !is.na(ncols)) {
				nrows = ceiling((nby[1] / ncols))
			} else if (!is.na(nrows) && is.na(ncols)) {
				ncols = ceiling((nby[1] / nrows))
			} else if (is.na(nrows) && is.na(ncols)) {
				
				# loop through col row combinations to find best nrow/ncol
				# b needed to compare landscape vs portrait. E.g if prefered asp is 2, 1 is equally good as 4
				ncols = which.min(vapply(1L:n, function(nc) {
					nr = ceiling(n / nc)
					# print("--")
					# print(nc)
					# print(nr)
					width = ((1 - sum(fixedMargins[c(2, 4)])) - (nc * sum(panel.wrap.size[c(2,4)])) - (nc - 1) * between.marginW) / nc
					height = ((1 - sum(fixedMargins[c(1, 3)])) - (nr * sum(panel.wrap.size[c(1,3)])) - (nr - 1) * between.marginH) / nr
					
					a = (width / height) * dasp
					b = ifelse(a<pasp, pasp/a, a/pasp)
					# print(a)
					# print(b)
					b
				}, FUN.VALUE = numeric(1)))

				
				nrows = ceiling(n / ncols)
			}
		}
		
		
		# if (n>1 && meta.automatic && !identical(asp, 0)) {
		# 	# redo meta margins calculations
		# 	width = ((1 - sum(fixedMargins[c(2, 4)])) - (ncols * sum(panel.wrap.size[c(2,4)])) - (ncols - 1) * between.marginW) / ncols
		# 	height = ((1 - sum(fixedMargins[c(1, 3)])) - (nrows * sum(panel.wrap.size[c(1,3)])) - (nrows - 1) * between.marginH) / nrows
		# 	a = (width / height) * dasp
		# 
		# 	if (a>pasp) {
		# 		meta.margins[c(2, 4)] = meta.margins[c(2, 4)] + (1 - (sum(fixedMargins[c(2, 4)]) + ((pasp * height) / dasp) * ncols + (ncols * sum(panel.wrap.size[c(2,4)])) + (ncols - 1) * between.marginW)) / max(1, sum(margins.used[c(2,4)]))
		# 	} else {
		# 		meta.margins[c(1, 3)] = meta.margins[c(1, 3)] + (1 - (sum(fixedMargins[c(1, 3)]) + ((width / pasp) * dasp) * nrows + (nrows * sum(panel.wrap.size[c(1,3)])) + (nrows - 1) * between.marginH)) / max(1, sum(margins.used[c(1,3)]))
		# 	}
		# 
		# }

		
		npages = ceiling(n / (nrows * ncols))	
	
		legend.position = NA
	})

}




step4_plot = function(tm) {
	tmx = tm$tmo
	o = tm$meta
	
	# get name of graphics engine (for function names e.g. tmapGridInit)
	gs = tmap_graphics_name()
	

	
	# collect legends
	dt_template = data.table::data.table(by1__ = integer(0), by2__ =  integer(0), by3__ =  integer(0), legend = list())
	legs = data.table::rbindlist(c(list(dt_template), lapply(tmx, function(tmxi) {
		data.table::rbindlist(lapply(tmxi$layers, function(tml) {
			
			legs = c(tml$trans_legend, tml$mapping_legend)
			
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
							
							#gpid = match(paste0("__", names(nvalues)), sapply(gp, "[[", 1))
							#gp[gpid] = nvalues
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
	
	
	
	# calculate margins (using grid system)
	
	
	
	
	
	
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
		#if (sum(sel) != 1L) stop("multiple shpTMs")
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
		if (!any(sel)) warning("empty dt: ", by1, " ", by2, " ", by3)#browser() #stop("empty dt")
		dt[which(sel),]
	}
	
	tmain = tmx[[o$main]][[1]]
	
	
	d = data.table::data.table(do.call(expand.grid, lapply(structure(o$nby, names = c("by1", "by2", "by3")), seq_len)))
	d[, i := seq_len(nrow(d))]
	

	grps = c("by1", "by2", "by3")[o$free.coords]
	
	
	#dtl[, c(nm, "legend") := do.call(f, c(unname(.SD), list(setup = s))), grp_b_fr, .SDcols = val]
	
	get_bbox = function(by1, by2, by3) {
		bbxs = lapply(tmain, function(tmi) {
			#if (by1[1] == 3) browser()
			shpTM = get_shpTM(tmi$shpDT, by1, by2, by3)
			mdt = get_dt(tmi$mapping_dt, by1, by2, by3)
			
			bbxs2 = lapply(shpTM, tmap::stm_bbox, tmapID = mdt$tmapID__)
			bbx = stm_merge_bbox(bbxs2)
			if (is.na(bbx)) bbx else tmaptools::bb(bbx, asp.limit = 10)
		})
		list(list(bb_ext(stm_merge_bbox(bbxs), o$inner.margins)))
	}
	get_asp = function(bbxl) {
		vapply(bbxl, function(bbx) {
			unname((bbx[3] - bbx[1]) / (bbx[4] - bbx[2]))
		}, FUN.VALUE = numeric(1))
	}

		
	d[, bbox:=do.call(get_bbox, as.list(.SD)), by = grps, .SDcols = c("by1", "by2", "by3")]
	d[, asp:=get_asp(bbox)]
	
	#o$asp
	
	
	
	diff_asp = any(d$asp != d$asp[1])
	o$sasp = ifelse(diff_asp, NA, d$asp[1])

	o = process_meta(o)

	o$ng = length(tmx)
	
	
	
	d[, row := as.integer((i - 1) %% o$nrows + 1)]
	d[, col := as.integer((((i - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
	d[, page := as.integer(i - 1) %/% (o$nrows * o$ncols) + 1]
	
	
	FUNinit = paste0("tmap", gs, "Init")
	FUNrun = paste0("tmap", gs, "Run")
	FUNshape = paste0("tmap", gs, "Shape")
	FUNwrap = paste0("tmap", gs, "Wrap")
	FUNxtab = paste0("tmap", gs, "Xtab")
	
	do.call(FUNinit, list(o = o))
	
	
	# if (!tmf$free.coords) {
	# 	bbxs = lapply(tmain, function(tmi) {
	# 		shpTMs = tmi$shpDT$shpTM
	# 		stm_merge_bbox(lapply(shpTMs, stm_bbox_all))
	# 	})
	# 	bbx = stm_merge_bbox(bbxs)
	# }
	
	if (o$panel.type == "xtab") {
		for (k in 1:o$npages) {
			labrows = o$fl[[1]]
			labcols = o$fl[[2]]
			if (length(labrows) == o$nrows) for (i in 1:o$nrows) do.call(FUNxtab, list(label = labrows[i], facet_row = i, facet_page = k)) 
			if (length(labcols) == o$ncols) for (j in 1:o$ncols) do.call(FUNxtab, list(label = labcols[j], facet_col = j, facet_page = k)) 

		}
	}
	


	d = d[!is.na(asp), ]
	
	
	
	for (i in seq_len(nrow(d))) {
 		bbx = d$bbox[[i]]
		#if (is.na(bbx)) next
 		if (o$panel.type == "wrap") do.call(FUNwrap, list(label = o$fl[[1]][i], facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i])) 
 		do.call(FUNshape, list(bbx = bbx, facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i], o = o))
		for (ig in 1L:o$ng) {
			tmxi = tmx[[ig]]
			nl = length(tmxi$layers)
			for (il in 1L:nl) {
				
				#if (ir == 2 && ig == 2 && il == 2) browser()
				
				bl = tmxi$layers[[il]]
				shpTM = get_shpTM(bl$shpDT, d$by1[i], d$by2[i], d$by3[i])[[1]]
				mdt = get_dt(bl$mapping_dt, d$by1[i], d$by2[i], d$by3[i])
				gp = bl$gp
				
				FUN = paste0("tmap", gs, bl$mapping_fun)
				
				#if (FUN == "tmapGridRaster") browser()
				do.call(FUN, list(shpTM = shpTM, dt = mdt, gp = gp, bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i]))
			}
			
		}
	}

	#print legends
	# 
	# if (o$is.wrap) {
	# 	print("legend.position")
	# 	print(o$legend.position)
	# 	if (o$ncols > 1 && o$nrows == 1 && o$legend.position[1] == "center") {
	# 		# multi col, 1 row wrap: align legends
	# 		legs[, by2__ := by1__]
	# 		legs[, by1__ := NA]
	# 	} else if (!(o$ncols == 1 && o$nrows > 1 && o$legend.position[2] == "center")) {
	# 		# (neg is default setting:  multi row, 1 col wrap: align legends)
	# 		# multi row&col wrap or misalignment meta margins: don't align legends
	# 		legs[, by1__ := NA]
	# 	}
	# }
	# 
	# if (o$is.wrap && o$legend.present.auto[2]) {
	# 	if (o$nrows == 1) {
	# 		legs[, by2__ := by1__]
	# 		legs[, by1__ := NA]
	# 	}
	# }
	
	#legs$position = lapply(legs$legend, FUN = function(l) l$setup$position)
	
	
	
	# to do
	# 1: per leg, find cell row and column:
	#   - auto legend: 
	#       - by1 NA by2 NA => take h and v, 
	#       - by1 !NA by2 NA take h set v to by1 cell 
	#       - by1 NA by2 !NA take v set h to by2 cell 
	
	# how to deal with grid row/col indices? make global?
	
	#legs[class == "inset", ':='(h2 = h, v2 = v)]
	
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
	
	if (o$is.wrap && o$n > 1) {
		if (o$nrows == 1) {
			legs[, by2__ := by1__]
			legs[, by1__ := NA]
		} else if (o$nrows > 1 && o$ncols > 1) {
			legs[class != "inset", by1__ := NA]
			legs[class != "inset", by2__ := NA]
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
	
	# manual inset legends
	
	# find all facets
	
	
	
	is_inset = legs$class == "inset"
	if (any(is_inset)) {
		legs_inset = lapply(which(is_inset), function(i) {
			d2 = data.table::copy(d)
			legsi = legs[i, ]
			if (is.na(legsi$by1__)) d2[, by1:= NA]
			if (is.na(legsi$by2__)) d2[, by2:= NA]
			if (is.na(legsi$by3__)) d2[, by3:= NA]
			legsi = merge(legsi, d2[, c("by1", "by2", "by3", "row", "col"), with = FALSE], by.x = c("by1__", "by2__", "by3__"), by.y = c("by1", "by2", "by3"))
			legsi[, ':='(facet_row = as.character(row), facet_col = as.character(col), row = NULL, col = NULL)]
			legsi
		})
		legs = data.table::rbindlist(c(list(legs[!is_inset]), legs_inset))
	}
	

	legfun = paste0("tmap", gs, "Legend")
	
	
	
	if (nrow(legs) > 0L) for (k in seq_len(o$npages)) {
		klegs = legs[is.na(by3__) | (by3__ == k), ]

		klegs[, do.call(legfun, args = list(legs = .SD$legend, o = o, facet_row = toI(.SD$facet_row[1]), facet_col = toI(.SD$facet_col[1]), facet_page = k, legend.stack = .SD$stack[1])), by = list(facet_row, facet_col), .SDcols = c("legend", "facet_row", "facet_col", "stack")]
	}
	
	
	# for (k in seq_len(o$npages)) {
	# 	# whole page legend
	# 	wlegs = legs[by3__ == k | is.na(by3__) & is.na(by1__) & is.na(by2__) & class == "auto", ]$legend
	# 	if (length(wlegs)>0) {
	# 		facet_row = if (o$legend.position[2] == "center") 1:o$nrows else if (o$legend.position[2] == "top") -Inf else Inf
	# 		facet_col = if (o$legend.position[1] == "center") 1:o$ncols else if (o$legend.position[1] == "left") -Inf else Inf
	# 
	# 		legend.stack = ifelse(o$legend.position[1] == "center", "horizontal", "vertical")
	# 		
	# 		tmapGridLegend(wlegs, o, facet_row = facet_row, facet_col = facet_col, facet_page = k, legend.stack = legend.stack)
	# 	}
	# 
	# 	# per row legend
	# 	ldf = legs[by3__ == k | is.na(by3__) & !is.na(by1__) & is.na(by2__) & is.na(position), ]
	# 	if (nrow(ldf)>0) for (i in seq_len(o$nrows)) {
	# 		wlegs = ldf[by1__ == i, ]$legend
	# 		facet_col = if (o$legend.position[1] == "left") -Inf else Inf
	# 		tmapGridLegend(wlegs, o, facet_row = i, facet_col = facet_col, facet_page = k, legend.stack = "horizontal")
	# 	}
	# 
	# 	# per col legend
	# 	ldf = legs[by3__ == k | is.na(by3__) & is.na(by1__) & !is.na(by2__) & is.na(position), ]
	# 	if (nrow(ldf)>0) for (j in seq_len(o$ncols)) {
	# 		wlegs = ldf[by2__ == j, ]$legend
	# 		facet_row = if (o$legend.position[2] == "top") -Inf else Inf
	# 		tmapGridLegend(wlegs, o, facet_row = facet_row, facet_col = j, facet_page = k, legend.stack = "vertical")
	# 	}
	# 
	# 	# per facet legend
	# 	ldf = legs[by3__ == k | is.na(by3__) & !is.na(by1__) & !is.na(by2__) & is.na(position), ]
	# 	if (nrow(ldf)>0) for (i in seq_len(o$ncols)) {
	# 		for (j in seq_len(o$ncols)) {
	# 			wlegs = ldf[by1__ == i & by2__ == j, ]$legend
	# 			tmapGridLegend(wlegs, o, facet_row = i, facet_col = j, facet_page = k, legend.stack = "vertical")
	# 		}
	# 	}
	# }

	
	do.call(FUNrun, list(o = o))
}
