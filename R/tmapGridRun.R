tmapGridRun = function(o) {
	gts = get("gts", .TMAP_GRID)
	lapply(gts, function(gt) {
		grid::grid.newpage()
		grid::grid.draw(gt)
	})
}
