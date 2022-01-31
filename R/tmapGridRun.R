tmapGridRun = function(o) {
	gts = get("gts", .TMAP_GRID)
	lapply(gts, function(gt) {
		if (is.null(o$vp)) grid::grid.newpage()
		grid::grid.draw(gt)
	})
	if (!is.null(o$vp)) upViewport(1)
}
