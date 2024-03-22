tmapGridSymbols = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	args = list(...)
	rc_text = frc(facet_row, facet_col)
	
	g = get("g", .TMAP_GRID)
	
	# COPIED FROM tmapGridText -> function
	# calculate native per line
	wIn = g$colsIn[g$cols_facet_ids[facet_col]]
	hIn = g$rowsIn[g$rows_facet_ids[facet_row]]
	
	wNative = bbx[3] - bbx[1]
	hNative = bbx[4] - bbx[2]
	
	xIn = wNative / wIn
	yIn = hNative / hIn
	
	lineIn = convertHeight(unit(1, "lines"), "inch", valueOnly = TRUE)
	
	
	res = select_sf(shpTM, dt[!is.na(dt$size), ])
	shp = res$shp
	dt = res$dt
	
	gp = impute_gp(gp, dt)
	gp = swap_pch_15_20(gp)	
	
	#gp = get_pch_1000p(gp)
	
	gp = rescale_gp(gp, o$scale_down)
	
	#gp = gp_to_gpar(gp, sel = "all")
	
	diffAlpha = !any(is.na(c(gp$fill_alpha, gp$col_alpha))) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
	
	coords = sf::st_coordinates(shp)
	
	if (diffAlpha) {
		gp1 = gp_to_gpar(gp, sel = "fill", o = o, type = "symbols")
		gp2 = gp_to_gpar(gp, sel = "col", o = o, type = "symbols")
		
		grb1 = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = gp$shape, size = grid::unit(gp$size, "lines"), gp = gp1, name = paste0("symbols_", id))
		grb2 = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = gp$shape, size = grid::unit(gp$size, "lines"), gp = gp2, name = paste0("symbols_borders_", id))
		grb = grid::grobTree(grb1, grb2)
	} else {
		gp = gp_to_gpar(gp, sel = "all", o = o, type = "symbols")
		
		if (any(!is.na(gp$shape) & gp$shape>999)) {
			shapeLib = get("shapeLib", envir = .TMAP)
			justLib = get("justLib", envir = .TMAP)
			
			grobs <- lapply(1:length(gp$shape), function(i) {
				shi = gp$shape[[i]]
				
				just = args$just
				jst = if (!is.na(shi) && shi>999) {
					js = justLib[[shi-999]]
					if (is.na(js[1])) args$just else js
				} else args$just
				
				justs.x = jst[1]
				justs.y = jst[2]
				justx = xIn * lineIn * (justs.x-.5)
				justy = yIn * lineIn * (justs.y-.5)
				
				

				if (!is.na(shi) && shi>999) {
					grbs <- if (gp$lwd[i] == 0) {
						gList(shapeLib[[shi-999]])	
					} else {
						gList(shapeLib[[shi-999]], rectGrob(gp=gpar(fill=NA, col=gp$col[i], lwd=gp$lwd[i])))	
					}
					gTree(children=grbs, vp=viewport(x=grid::unit(coords[i,1] + justx * gp$size[i]*2/3, "native"), 
													 y=grid::unit(coords[i,2] + justy * gp$size[i]*2/3, "native"),
													 width=unit(gp$size[i]*2/3, "lines"),
													 height=unit(gp$size[i]*2/3, "lines")))
				} else {
					pointsGrob(x=grid::unit(coords[i,1] + justx * gp$size[i], "native"), 
							   y=grid::unit(coords[i,2] + justx * gp$size[i], "native"),
							   size=unit(gp$size[i], "lines"),
							   pch=shi,
							   gp=gp)
				}
			})
			grb = gTree(children=do.call(gList, grobs), name=paste0("symbols_", id))
		} else {
			grb = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = gp$shape, size = grid::unit(gp$size, "lines"), gp = gp, name = paste0("symbols_", id))
		}
		
		
	}
	
	
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))
	
	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
	
}
