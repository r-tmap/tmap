tmapGridRun = function(o, show) {
	gts = get("gts", .TMAP_GRID)
	if (show) {
		lapply(gts, function(gt) {
			if (is.null(o$vp)) grid::grid.newpage()
			grid::grid.draw(gt)
		})
		if (!is.null(o$vp)) upViewport(1)
	}
	gts
}
