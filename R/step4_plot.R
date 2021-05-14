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

process_margins = function(o) {
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
		between.marginH = between.margins * lineH
		between.marginW = between.margins * lineW
		
		fixedMargins  =  outer.margins + meta.buffers * 2 + meta.margins + xylab.margins + panel.xtab.size + grid.buffers + grid.margins
		
	})
	
}

how_many_rows = function(o) {
	within(o, {
		#if (is.na(asp) && !is.na(sasp)) asp = sasp
		
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
	})
	
	
}


step4_plot = function(tm) {
	tmx = tm$tmo
	o = tm$meta
	
	gs = tmap_graphics_name()
	

	#opts = list(tmf = tmf)
	
	
	# fl = tmf$fl
	# nby = get_nby(fl)
	# is.wrap = tmf$is.wrap
	# nrows = tmf$nrows
	# ncols = tmf$ncols
	o$nby = get_nby(o$fl)
	o$n = prod(o$nby)
	
	# calculate margins (using grid system)
	
	
	if (is.na(o$panel.type)) o$panel.type = ifelse(o$n == 1 || (o$is.wrap && !is.character(o$fl[[1]])) || (!o$is.wrap && !is.character(o$fl[[1]]) && !is.character(o$fl[[2]])), "none", ifelse(o$is.wrap, "wrap", "xtab"))
	
	o = process_margins(o)
	
	
	
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
	
	d[, bbox:=do.call(get_bbox, as.list(.SD)), by = grps, .SDcols = c("by1", "by2", "by3")]

	
	get_asp = function(bbxl) {
		vapply(bbxl, function(bbx) {
			unname((bbx[3] - bbx[1]) / (bbx[4] - bbx[2]))
		}, FUN.VALUE = numeric(1))
	}
	
	d[, asp:=get_asp(bbox)]
	
	#o$asp
	
	
	
	diff_asp = any(d$asp != d$asp[1])
	o$sasp = ifelse(diff_asp, NA, d$asp[1])

	o = how_many_rows(o)
	
	o$npages = ceiling(o$n / (o$nrows * o$ncols))
	
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
 		do.call(FUNshape, list(bbx = bbx, facet_row = d$row[i], facet_col = d$col[i], facet_page = d$page[i]))
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
	do.call(FUNrun, list(o = o))
}
