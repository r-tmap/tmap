tmapGridWrap = function(label, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	rot = g$panel_rot
	row = g$rows_panel_ids[facet_row]
	col = g$cols_panel_ids[facet_col]
	
	frame.col = if (identical(o$frame, FALSE)) o$attr.color else if (identical(o$frame, TRUE)) o$attr.color else o$frame
	
	#scale = o$scale * o$scale_down
	
	gpar_rect = grid::gpar(fill = o$panel.label.bg.color, lwd=o$frame.lwd, col = o$frame)
	gpar_text = rescale_gp(grid::gpar(cex = o$panel.label.size, col = o$panel.label.color, fontfamily = o$panel.label.fontfamily, fontface = o$panel.label.fontface), o$scale_down)
	
	# resize due to not fitting
	gpar_text$cex = determine_scale(label = label, rot = rot, row = row, col = col, g = g, scale = gpar_text$cex)
	grb = grid::grobTree(
		rndrectGrob(gp = gpar_rect, r = o$frame.r),
		grid::textGrob(label = label, rot = rot, gp = gpar_text)
	)
	
	
	gt = add_to_gt(gt, grb, row = row, col = col)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}

tmapGridXtab = function(label, facet_row = NULL, facet_col = NULL, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	if (is.null(facet_row)) {
		rot = g$panel_col_rot
		row = g$cols_panel_row_id
		col = g$cols_panel_col_ids[facet_col]
	} else {
		rot = g$panel_row_rot
		row = g$rows_panel_row_ids[facet_row]
		col = g$rows_panel_col_id
	}
	
	gpar_rect = grid::gpar(fill = o$panel.label.bg.color, lwd=o$frame.lwd)
	gpar_text = rescale_gp(grid::gpar(cex = o$panel.label.size, col = o$panel.label.color, fontfamily = o$panel.label.fontfamily, fontface = o$panel.label.fontface), o$scale_down)
	
	# resize due to not fitting
	gpar_text$cex = determine_scale(label = label, rot = rot, row = row, col = col, g = g, scale = gpar_text$cex)
	
	grb = grid::grobTree(
		grid::rectGrob(gp = gpar_rect),
		grid::textGrob(label = label, rot = rot, gp = gpar_text)
	)
	
	gt = add_to_gt(gt, grb, row = row, col = col)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}
