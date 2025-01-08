tmapGridXlab = function(facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)

	gt = gts[[facet_page]]

	row = g$xlab_row_id
	col = g$xlab_col_ids

	gpar_text = rescale_gp(grid::gpar(cex = o$xlab.size, col = o$xlab.color, fontfamily = o$xlab.fontfamily, fontface = o$xlab.fontface, alpha = o$xlab.alpha), o$scale_down)

	grb = grid::grobTree(
		grid::textGrob(label = o$xlab.text, rot = o$xlab.rotation, gp = gpar_text)
	)

	gt = add_to_gt(gt, grb, row = row, col = col)
	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)

}


tmapGridYlab = function(facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)

	gt = gts[[facet_page]]

	row = g$ylab_row_ids
	col = g$ylab_col_id

	gpar_text = rescale_gp(grid::gpar(cex = o$ylab.size, col = o$ylab.color, fontfamily = o$ylab.fontfamily, fontface = o$ylab.fontface), o$scale_down)

	grb = grid::grobTree(
		grid::textGrob(label = o$ylab.text, rot = o$ylab.rotation, gp = gpar_text)
	)

	gt = add_to_gt(gt, grb, row = row, col = col)
	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)

}
