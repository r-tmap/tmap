tmapGridShape = function(bbx, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	
	basp = (bbx[3] - bbx[1]) / (bbx[4] - bbx[2])
	
	fbbx = bb_asp(bbx, g$fasp)
	
	rc_text = frc(facet_row, facet_col)
	
	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]
	
	bgcol = if (!is.na(o$bg.color)) o$bg.color else NA
	frame.lwd = if (identical(o$frame, FALSE)) 0 else o$frame.lwd
	frame.col = if (identical(o$frame, FALSE)) NA else if (identical(o$frame, TRUE)) o$attr.color else o$frame
	
	frame = if (!(is.na(frame.col))) {
		pH = convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*frame.lwd
		pW = convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*frame.lwd
		if (o$frame.double.line) {
			gList(
				rectGrob(width = 1-4*pW, height=1-4*pH, gp=gpar(col=bgcol, fill=bgcol, lwd=5*frame.lwd, lineend="square"), name = "outer_frame"),
				rectGrob(gp=gpar(col=frame.col, fill=NA, lwd=3*frame.lwd, lineend="square"), name = "between_frame"),
				rectGrob(width = 1-8*pW, height=1-8*pH, gp=gpar(col=frame.col, fill=NA, lwd=frame.lwd, lineend="square"), name = "inner_frame"))
		} else {
			rectGrob(gp=gpar(col=frame.col, fill=bgcol, lwd=frame.lwd, lineend="square"), name = "outer_frame")
		}
	} else NULL

	# innerRect = if (is.na(bgcol) && frame.lwd == 0) {
	# 	NULL
	# } else grid::rectGrob(gp=grid::gpar(col=frame.col, lwd = frame.lwd, fill = bgcol), name = "outer_frame")
	
	gtmap = grid::grobTree(frame,
						   vp = grid::vpStack(grid::viewport(layout.pos.col = colid, layout.pos.row = rowid, name = paste0("vp_facet_", rc_text)),
						   				   grid::viewport(xscale = fbbx[c(1,3)], yscale = fbbx[c(2,4)], name = paste0("vp_map_", rc_text), clip = TRUE)), name = paste0("gt_facet_", rc_text))
	
	gts[[facet_page]] = grid::addGrob(gts[[facet_page]], gtmap, gPath = grid::gPath("gt_main"))
	
	assign("gts", gts, envir = .TMAP_GRID)
}
