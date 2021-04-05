get_nby = function(fl) {
	vapply(fl, function(f) {
		if (is.integer(f)) f else length(f)	
	}, integer(1))
}

get_row = function(i, ncols, nrows) {
	as.integer((((i - 1) %/% ncols + 1) - 1) %% nrows + 1)
}

get_col = function(i, ncols) {
	as.integer((i - 1) %% ncols + 1)
}

get_page = function(i, ncols, nrows) {
	as.integer(i - 1) %/% (nrows * ncols) + 1
}

get_i = function(ir, ic, ip, nby) {
	ir + (ic - 1) * nby[1] + (ip - 1) * prod(nby[1:2])
}


step4_plot = function(tmx) {
	
	gs = tmap_graphics_name()
	
	fl = attr(tmx, "fl")
	nby = get_nby(fl)
	is.wrap = attr(tmx, "is.wrap")
	nrows = attr(tmx, "nrows")
	ncols = attr(tmx, "ncols")
	
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
	
	for (ip in 1L:nby[3]) {
		for (ic in 1L:nby[2]) {
			for (ir in 1L:nby[1]) {
				# ix = data dimensions
				# jx = plot dimensions (not the same with wrap)
				i = get_i(ir, ic, ip, nby)
				jr = get_row(i, ncols, nrows)
				jc = get_col(i, ncols)
				jp = get_page(i, ncols, nrows)
								
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
	do.call(FUNrun, list())
}
