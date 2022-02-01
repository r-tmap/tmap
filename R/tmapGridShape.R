tmapGridShape = function(bbx, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	

	fbbx = bb_asp(bbx, g$fasp)
	
	rc_text = frc(facet_row, facet_col)
	
	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]
	
	bg = if (is.na(o$frame) || is.na(o$bg.color)) {
		NULL
	} else {
		rndrectGrob(gp=gpar(fill=o$bg.color, lwd=0, lineend="square"), r = o$frame.r, name = "inner_rect")
	}

	gtmap = grid::grobTree(bg,
						   vp = grid::vpStack(grid::viewport(layout.pos.col = colid, layout.pos.row = rowid, name = paste0("vp_facet_", rc_text)),
						   				   grid::viewport(xscale = fbbx[c(1,3)], yscale = fbbx[c(2,4)], name = paste0("vp_map_", rc_text), clip = TRUE)), name = paste0("gt_facet_", rc_text))
	
	gts[[facet_page]] = grid::addGrob(gts[[facet_page]], gtmap, gPath = grid::gPath("gt_main"))
	
	assign("gts", gts, envir = .TMAP_GRID)
	NULL
}

tmapGridOverlay = function(facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	rc_text = frc(facet_row, facet_col)
	
	frame.lwd = if (identical(o$frame, FALSE)) 0 else o$frame.lwd
	frame.col = if (identical(o$frame, FALSE)) NA else if (identical(o$frame, TRUE)) o$attr.color else o$frame
	
	frame = if (!(is.na(frame.col))) {
		pH = convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*frame.lwd
		pW = convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*frame.lwd
		if (o$frame.double.line) {
			bgcol = substr(o$bg.color, 1, 7) # remove transparency
			grid::grobTree(
				rndrectGrob(width = 1-4*pW, height=1-4*pH, gp=gpar(col=bgcol, fill=NA, lwd=5*frame.lwd, lineend="square"), r = o$frame.r, name = "outer_frame"),
				rndrectGrob(gp=gpar(col=frame.col, fill=NA, lwd=3*frame.lwd, lineend="square"), r = o$frame.r, name = "between_frame"),
				rndrectGrob(width = 1-8*pW, height=1-8*pH, gp=gpar(col=frame.col, fill=NA, lwd=frame.lwd, lineend="square"), r = o$frame.r, name = "inner_frame"))
		} else {
			rndrectGrob(gp=gpar(col=frame.col, fill=NA, lwd=frame.lwd, lineend="square"), r = o$frame.r, name = "outer_frame")
		}
	} else NULL
	
	
	#gt_name = paste0("gt_facet_", rc_text)
	
	#gts[[facet_page]] = grid::addGrob(gts[[facet_page]], frame, gPath = grid::gPath(gt_name))

	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]
	
	gt = add_to_gt(gt, frame, row = rowid, col = colid)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
	
}
