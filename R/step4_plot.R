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
	
	tmf = attr(tmx, "tmf")
	
	fl = tmf$fl
	nby = get_nby(fl)
	is.wrap = tmf$is.wrap
	nrows = tmf$nrows
	ncols = tmf$ncols
	
	if (!is.wrap) {
		nrows = nby[1]
		ncols = nby[2]
	} else {
		if (is.na(nrows) && !is.na(ncols)) {
			nrows = ceiling((nby[1] / ncols))
		} else if (!is.na(nrows) && is.na(ncols)) {
			ncols = ceiling((nby[1] / nrows))
		} else if (is.na(nrows) && is.na(ncols)) {
			nrows = round(sqrt(nby[1]))
			ncols = ceiling((nby[1] / nrows))
		}
	}  
	
	npages = ceiling(prod(nby) / (nrows * ncols))
	

	
	
	bbx = attr(tmx, "bbox")
	
	FUNinit = paste0("tmap", gs, "Init")
	FUNrun = paste0("tmap", gs, "Run")
	FUNshape = paste0("tmap", gs, "Shape")
	
	do.call(FUNinit, list(nrow = nrows, ncol = ncols, npage = npages))
	ng = length(tmx)
	
	get_shpTM = function(shpDT, by1, by2, by3) {
		b = c(by1, by2, by3)
		bynames = intersect(names(shpDT), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))
		
		sel = rep(TRUE, nrow(shpDT))
		if (length(bynames)) {
			for (i in 1:length(bynames)) {
				sel = sel & shpDT[[bynames[i]]] == b[byids[i]]			
			}
		}
		if (sum(sel) != 1L) stop("multiple shpTMs")
		shpDT$shpTM[[which(sel)]]
	}
	
	
	get_dt = function(dt, by1, by2, by3) {
		b = c(by1, by2, by3)
		bynames = intersect(names(dt), paste0("by", 1:3, "__"))
		byids = as.integer(substr(bynames, 3, 3))
		
		sel = rep(TRUE, nrow(dt))
		if (length(bynames)) {
			for (i in 1:length(bynames)) {
				sel = sel & dt[[bynames[i]]] == b[byids[i]]			
			}
		}
		if (!any(sel)) stop("empty dt")
		dt[which(sel),]
	}
	
	mainid = attr(tmx, "main")
	tmain = tmx[[mainid]][[1]]
	
	if (!tmf$free.coords) {
		bbxs = lapply(tmain, function(tmi) {
			shpTMs = tmi$shpDT$shpTM
			stm_merge_bbox(lapply(shpTMs, stm_bbox_all))
		})
		bbx = stm_merge_bbox(bbxs)
	}
	
	opts = list(tmf = tmf)
	
	print(tmf$free.coords)
	
	for (ip in 1L:nby[3]) {
		for (ic in 1L:nby[2]) {
			for (ir in 1L:nby[1]) {
				# ix = data dimensions
				# jx = plot dimensions (not the same with wrap)
				i = get_i(ir, ic, ip, nby)
				jr = get_row(i, nrows)
				jc = get_col(i, nrows, ncols)
				jp = get_page(i, nrows, ncols)
					
				#cat("ir", ir, "ic", ic, "ip", ip, "jr", jr, "jc", jc, "jp", jp, "\n")
						
				if (tmf$free.coords) {
					bbxs = lapply(tmain, function(tmi) {
						shpTM = get_shpTM(tmi$shpDT, ir, ic, ip)
						mdt = get_dt(tmi$mapping_dt, ir, ic, ip)
						
						tmap:::stm_bbox(shpTM, mdt$tmapID__)
					})
					bbx = stm_merge_bbox(bbxs)
				}
				
				
				do.call(FUNshape, list(bbx = bbx, facet_row = jr, facet_col = jc, facet_page = jp))
				for (ig in 1L:ng) {
					
					
					tmxi = tmx[[ig]]
					nl = length(tmxi$layers)
					for (il in 1L:nl) {
						
						#if (ir == 2 && ig == 2 && il == 2) browser()
						
						bl = tmxi$layers[[il]]
						shpTM = get_shpTM(bl$shpDT, ir, ic, ip)
						mdt = get_dt(bl$mapping_dt, ir, ic, ip)
						
						FUN = paste0("tmap", gs, bl$mapping_fun)
						
						#if (FUN == "tmapGridRaster") browser()
						do.call(FUN, list(shpTM = shpTM, dt = mdt, facet_col = jc, facet_row = jr, facet_page = jp))
					}
					
				}
			}
		}
	}
	do.call(FUNrun, list(opts = opts))
}
