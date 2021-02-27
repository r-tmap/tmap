step4_plot = function(tmx) {
	
	gs = tmap_graphics_name()
	
	fl = attr(tmx, "fl")
	nby = vapply(fl, length, integer(1))
	
	bbx = attr(tmx, "bbox")
	
	FUNinit = paste0("tmap", gs, "Init")
	FUNrun = paste0("tmap", gs, "Run")
	FUNshape = paste0("tmap", gs, "Shape")
	
	do.call(FUNinit, list(nrow = nby[1], ncol = nby[2]))
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
	
	for (ip in 1L:nby[3]) {
		for (ic in 1L:nby[2]) {
			for (ir in 1L:nby[1]) {
				
				do.call(FUNshape, list(bbx = bbx, facet_row = ir, facet_col = ic))
				for (ig in 1L:ng) {
					tmxi = tmx[[ig]]
					nl = length(tmxi$layers)
					for (il in 1L:nl) {
						
						
						bl = tmxi$layers[[il]]
						shpTM = get_shpTM(bl$shpDT, ir, ic, ip)
						mdt = bl$mapping_dt
						
						FUN = paste0("tmap", gs, bl$mapping_fun)
						
						#if (FUN == "tmapGridRaster") browser()
						do.call(FUN, list(shpTM = shpTM, dt = mdt, facet_col = ic, facet_row = ir))
					}
					
				}
			}
		}
	}
	do.call(FUNrun, list())
}
