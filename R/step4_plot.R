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


step4_plot = function(tmx) {
	
	gs = tmap_graphics_name()
	
	o = attr(tmx, "tmf")
	o$mainid = attr(tmx, "main")
	
	
	#opts = list(tmf = tmf)
	
	
	# fl = tmf$fl
	# nby = get_nby(fl)
	# is.wrap = tmf$is.wrap
	# nrows = tmf$nrows
	# ncols = tmf$ncols
	o$nby = get_nby(o$fl)
	o$n = prod(o$nby)
	
	if (!o$is.wrap) {
		o$nrows = o$nby[1]
		o$ncols = o$nby[2]
	} else {
		if (is.na(o$nrows) && !is.na(o$ncols)) {
			o$nrows = ceiling((o$nby[1] / o$ncols))
		} else if (!is.na(o$nrows) && is.na(o$ncols)) {
			o$ncols = ceiling((o$nby[1] / o$nrows))
		} else if (is.na(o$nrows) && is.na(o$ncols)) {
			o$nrows = round(sqrt(o$nby[1]))
			o$ncols = ceiling((o$nby[1] / o$nrows))
		}
	}  
	
	o$npages = ceiling(o$n / (o$nrows * o$ncols))
	
	o$ng = length(tmx)
	
	FUNinit = paste0("tmap", gs, "Init")
	FUNrun = paste0("tmap", gs, "Run")
	FUNshape = paste0("tmap", gs, "Shape")
	
	do.call(FUNinit, list(o = o))

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
	
	tmain = tmx[[o$mainid]][[1]]
	
	
	d = data.table::data.table(do.call(expand.grid, lapply(structure(o$nby, names = c("by1", "by2", "by3")), seq_len)))
	d[, i := seq_len(nrow(d))]
	
	d[, row := as.integer((i - 1) %% o$nrows + 1)]
	d[, col := as.integer((((i - 1) %/% o$nrows + 1) - 1) %% o$ncols + 1)]
	d[, page := as.integer(i - 1) %/% (o$nrows * o$ncols) + 1]
	

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
		list(list(stm_merge_bbox(bbxs)))
	}
	
	d[, bbox:=do.call(get_bbox, as.list(.SD)), by = grps, .SDcols = c("by1", "by2", "by3")]
	
	
	
	# if (!tmf$free.coords) {
	# 	bbxs = lapply(tmain, function(tmi) {
	# 		shpTMs = tmi$shpDT$shpTM
	# 		stm_merge_bbox(lapply(shpTMs, stm_bbox_all))
	# 	})
	# 	bbx = stm_merge_bbox(bbxs)
	# }
	
	
	for (i in seq_len(nrow(d))) {
 		bbx = d$bbox[[i]]
		if (is.na(bbx)) next
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
				do.call(FUN, list(shpTM = shpTM, dt = mdt, facet_col = d$col[i], facet_row = d$row[i], facet_page = d$page[i]))
			}
			
		}
	}
	do.call(FUNrun, list(o = o))
}
