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
		
		meta.automatic = is.na(meta.margins)
		
		if (meta.automatic) meta.margins = c(0, 0, 0, 0)
		
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
		
		if (meta.automatic) {
			if (!any(legend.present)) {
				# no central legends
				meta.margins = c(0, 0, 0, 0)
			} else if (legend.present[1] & !legend.present[2] & !legend.present[3]) {
				# only 'all facets' legends (either bottom or right)
				if ((n == 1 && pasp > masp) || (n > 1 && masp < 1)) {
					legend.position = c("center", "bottom")
					if (n == 1) {
						meta.margins = c(max(0.2, 1- masp/pasp - 2*bufferH), 0, 0, 0)
					} else {
						meta.margins = c(0.2, 0, 0, 0)
					}
					
				} else {
					legend.position = c("right", "center")
					if (n == 1) {
						meta.margins = c(0, 0, 0, max(0.2, 1 - pasp/masp - 2*bufferW))
					} else {
						meta.margins = c(0, 0, 0, 0.2)
					}
				}
			} else {
				# central goes bottom right
				legend.position = c("right", "bottom")
				meta.margins = c(0.2, 0, 0, 0.2)
			}
			
			# redo calculations
			meta.buffers = sign(meta.margins) * c(bufferH, bufferW, bufferH, bufferW) # outside and inside
			fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
			
		} else {
			if (legend.present[1] & !legend.present[2] & !legend.present[3]) {
				legend.position = c("right", "center")
			} else {
				legend.position = c("right", "bottom")
			}
			
			# todo: check if meta.margins are non-0 for required space
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
		
		if (n>1 && meta.automatic && !identical(asp, 0)) {
			# redo meta margins calculations
			width = ((1 - sum(fixedMargins[c(2, 4)])) - (ncols * sum(panel.wrap.size[c(2,4)])) - (ncols - 1) * between.marginW) / ncols
			height = ((1 - sum(fixedMargins[c(1, 3)])) - (nrows * sum(panel.wrap.size[c(1,3)])) - (nrows - 1) * between.marginH) / nrows
			a = (width / height) * dasp
			print("xxx")
			print(a)
			print(pasp)

			if (a>pasp) {
				meta.margins[4] = meta.margins[4] + (1 - (sum(fixedMargins[c(2, 4)]) + ((pasp * height) / dasp) * ncols + (ncols * sum(panel.wrap.size[c(2,4)])) + (ncols - 1) * between.marginW))
			} else {
				meta.margins[1] = meta.margins[1] + (1 - (sum(fixedMargins[c(1, 3)]) + ((width / pasp) * dasp) * nrows + (nrows * sum(panel.wrap.size[c(1,3)])) + (nrows - 1) * between.marginH))
			}

		}
		
		npages = ceiling(n / (nrows * ncols))	
		
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
			data.table::rbindlist(c(tml$trans_legend, tml$mapping_legend), fill = TRUE)
		}), fill = TRUE)
	})), fill = TRUE)
	
	
	# find out whether there are legends for all facets, per row, per col
	# use them to automatically determine meta.margins (in preprocess_meta)
	o$legend.present = c(all = any(is.na(legs$by1__) & is.na(legs$by2__)), per_row = any(!is.na(legs$by1__) & is.na(legs$by2__)), per_col = any(is.na(legs$by1__) & !is.na(legs$by2__)))
	
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
	print("o$fl")
	print(o$fl)
	print("o$is.wrap")
	print(o$is.wrap)
	
	if (o$panel.type == "xtab") {
		for (k in 1:o$npages) {
			labrows = o$fl[[1]]
			labcols = o$fl[[2]]
			if (length(labrows) == o$nrows) for (i in 1:o$nrows) do.call(FUNxtab, list(label = labrows[i], facet_row = i, facet_page = k)) 
			if (length(labcols) == o$ncols) for (j in 1:o$ncols) do.call(FUNxtab, list(label = labcols[j], facet_col = j, facet_page = k)) 

		}
	}
	


	
	
	
	
	for (i in seq_len(nrow(d))) {
 		bbx = d$bbox[[i]]
		if (is.na(bbx)) next
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
				
				FUN = paste0("tmap", gs, bl$mapping_fun)
				
				#if (FUN == "tmapGridRaster") browser()
				do.call(FUN, list(shpTM = shpTM, dt = mdt, bbx = bbx, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i]))
			}
			
		}
	}

	#print legends
	for (k in seq_len(o$npages)) {
		# whole page legend
		wlegs = legs[by3__ == k | is.na(by3__) & is.na(by1__) & is.na(by2__), ]$legend
		if (length(wlegs)>0) {
			facet_row = if (o$legend.position[2] == "center") 1:o$nrows else if (o$legend.position[2] == "top") -Inf else Inf
			facet_col = if (o$legend.position[1] == "center") 1:o$ncols else if (o$legend.position[1] == "left") -Inf else Inf

			legend.stack = ifelse(o$legend.position[1] == "center", "horizontal", "vertical")
			
			tmapGridLegend(wlegs, o, facet_row = facet_row, facet_col = facet_col, facet_page = k, legend.stack = legend.stack)
		}

		# per row legend
		ldf = legs[by3__ == k | is.na(by3__) & !is.na(by1__) & is.na(by2__), ]
		if (nrow(ldf)>0) for (i in seq_len(o$nrows)) {
			wlegs = ldf[by1__ == i, ]$legend
			facet_col = if (o$legend.position[1] == "left") -Inf else Inf
			tmapGridLegend(wlegs, o, facet_row = i, facet_col = facet_col, facet_page = k, legend.stack = "horizontal")
		}

		# per col legend
		ldf = legs[by3__ == k | is.na(by3__) & is.na(by1__) & !is.na(by2__), ]
		if (nrow(ldf)>0) for (j in seq_len(o$ncols)) {
			wlegs = ldf[by2__ == j, ]$legend
			facet_row = if (o$legend.position[2] == "top") -Inf else Inf
			tmapGridLegend(wlegs, o, facet_row = facet_row, facet_col = j, facet_page = k, legend.stack = "vertical")
		}

		# per facet legend
		ldf = legs[by3__ == k | is.na(by3__) & !is.na(by1__) & !is.na(by2__), ]
		if (nrow(ldf)>0) for (i in seq_len(o$ncols)) {
			for (j in seq_len(o$ncols)) {
				wlegs = ldf[by1__ == i & by2__ == j, ]$legend
				tmapGridLegend(wlegs, o, facet_row = i, facet_col = j, facet_page = k, legend.stack = "vertical")
			}
		}
	}

	
	do.call(FUNrun, list(o = o))
}
