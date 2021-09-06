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
	frame.col = if (identical(o$frame, FALSE)) NA else if (identical(o$frame, TRUE)) "gray30" else o$frame.col
	
	innerRect = if (is.na(bgcol) && frame.lwd == 0) {
		NULL
	} else grid::rectGrob(gp=grid::gpar(col=frame.col, lwd = frame.lwd, fill = o$bg.color), name = "outer_frame")
	
	gtmap = grid::grobTree(innerRect,
						   vp = grid::vpStack(grid::viewport(layout.pos.col = colid, layout.pos.row = rowid, name = paste0("vp_facet_", rc_text)),
						   				   grid::viewport(xscale = fbbx[c(1,3)], yscale = fbbx[c(2,4)], name = paste0("vp_map_", rc_text), clip = TRUE)), name = paste0("gt_facet_", rc_text))
	
	gts[[facet_page]] = grid::addGrob(gts[[facet_page]], gtmap, gPath = grid::gPath("gt_main"))
	
	assign("gts", gts, envir = .TMAP_GRID)
}
