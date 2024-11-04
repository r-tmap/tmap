tmapGridRun = function(o, q, show, knit, args) {
	gts = get("gts", .TMAP_GRID)
	if (show) {
		mapply(function(gt,i) {
			if (is.null(o$vp) && i != 1L) grid::grid.newpage()
			tryCatch({
				grid::grid.draw(gt)
			}, error = function(e) {
				stop("Plot error. Try adding + tm_check_fix()", call. = FALSE)
			})

		}, gts, seq_along(gts), SIMPLIFY = FALSE)
		if (!is.null(o$vp)) upViewport(1)
	}
	if (length(gts) == 1) gts = gts[[1]]
	gts
}
