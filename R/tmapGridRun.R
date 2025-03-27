tmapGridRun = function(o, q, show, knit, args) {
	gts = get("gts", .TMAP_GRID)
	if (show) {
		ani = is.null(o$animate_disable) && (o$animate || o$trans_animate) # animate is multiple variables/facets, trans_animate for transition animation only
		if (ani) {
			d <- paste(tempdir(), "/tmap_plots", sep="/")
			dir.create(d, showWarnings = FALSE)
			gasp2 = get("gasp2", .TMAP_GRID)
			devsize = c(grid::convertWidth(grid::unit(1, "npc"), unitTo = "inch", valueOnly = TRUE) * 72,
						grid::convertHeight(grid::unit(1, "npc"), unitTo = "inch", valueOnly = TRUE) * 72)
			dasp = devsize[1] / devsize[2]
			if (gasp2 > dasp) {
				#
			}
		}

		mapply(function(gt,i) {
			if (ani) {
				png(paste0(d, "/plot", sprintf("%03d", i), ".png"), width = devsize[1], height = devsize[2], res = 72, type = "cairo")
				grid::grid.newpage()
			} else {
				if (is.null(o$vp) && i != 1L) grid::grid.newpage()
			}

			tryCatch({
				grid::grid.draw(gt)
			}, error = function(e) {
				stop("Plot error. Try adding + tm_check_fix()", call. = FALSE)
			})
			if (ani) {
				dev.off()
			}

		}, gts, seq_along(gts), SIMPLIFY = FALSE)
		if (!is.null(o$vp)) upViewport(1)
	}
	if (length(gts) == 1) gts = gts[[1]]

	if (show && ani) {
		files = list.files(path = d, pattern = "^plot[0-9]{3}\\.png$", full.names = TRUE)
		if (o$play == "pingpong") files = c(files, rev(files))
		filename = tempfile(fileext = ".gif")
		create_animation(filename = filename, files = files, width = devsize[1], height = devsize[2], delay = NA, fps = o$fps, loop = o$play != "once", progress = FALSE, gif = TRUE, showAni = TRUE)
	}

	gts
}
