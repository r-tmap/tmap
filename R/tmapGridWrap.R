get_panel_grob = function(label, o, g, rot, row, col) {
	panel.show = o$panel.label.frame || o$panel.label.bg
	frame.col = if (o$panel.label.frame) o$panel.label.frame.color else NA
	bg.col = if (o$panel.label.bg) o$panel.label.bg.color else NA
	if (panel.show) {
		if (o$panel.label.bg.alpha != o$panel.label.frame.alpha) {
			gpar_panel = grid::gpar(fill = bg.col, lwd=0, col = NA, alpha = o$panel.label.bg.alpha)
			gpar_panel_frame = grid::gpar(fill = NA, lwd=o$panel.label.frame.lwd * o$scale, col = frame.col, alpha = o$panel.label.frame.alpha)
		} else {
			gpar_panel = grid::gpar(fill = bg.col, lwd=o$panel.label.frame.lwd * o$scale, col = frame.col, alpha = o$panel.label.bg.alpha)
			gpar_panel_frame = NULL
		}
	}
	gpar_text = rescale_gp(grid::gpar(cex = o$panel.label.size * o$scale, col = o$panel.label.color, fontfamily = o$panel.label.fontfamily, fontface = o$panel.label.fontface, alpha = o$panel.label.alpha), o$scale_down)

	# resize due to not fitting
	gpar_text$cex = determine_scale(label = label, rot = rot, row = row, col = col, g = g, scale = gpar_text$cex)

	if (!is.na(label)) label = decode_expr(label)


	grid::grobTree(
		if (panel.show) {
			rndrectGrob(gp = gpar_panel, r = o$panel.label.frame.r * o$scale)
		} else NULL,
		if (panel.show && !is.null(gpar_panel_frame)) {
			rndrectGrob(gp = gpar_panel_frame, r = o$panel.label.frame.r * o$scale)
		} else NULL,
		grid::textGrob(label = label, rot = rot, gp = gpar_text)
	)
}

tmapGridWrap = function(label, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)

	gt = gts[[facet_page]]

	rot = g$panel_rot
	row = g$rows_panel_ids[facet_row]
	col = g$cols_panel_ids[facet_col]

	grb = get_panel_grob(label, o, g, rot, row, col)

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

	grb = get_panel_grob(label, o, g, rot, row, col)

	gt = add_to_gt(gt, grb, row = row, col = col)

	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)


}
