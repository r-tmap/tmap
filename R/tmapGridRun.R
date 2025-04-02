tmapGridRun = function(o, q, show, knit, args) {
	gts = get("gts", .TMAP_GRID)
	if (show) {
		if (o$show_gif_ani) {
			d <- paste(tempdir(), "/tmap_plots", sep="/")
			dir.create(d, showWarnings = FALSE)
			devsize = c(grid::convertWidth(grid::unit(1, "npc"), unitTo = "inch", valueOnly = TRUE) * 72 * o$dpr,
						grid::convertHeight(grid::unit(1, "npc"), unitTo = "inch", valueOnly = TRUE) * 72 * o$dpr)
		}

		mapply(function(gt,i) {
			if (o$show_gif_ani) {
				png(paste0(d, "/plot", sprintf("%03d", i), ".png"), width = devsize[1], height = devsize[2], res = 72*o$dpr, type = "cairo-png")
				grid::grid.newpage()
			} else {
				if (is.null(o$vp) && i != 1L) grid::grid.newpage()
			}

			tryCatch({
				grid::grid.draw(gt)
			}, error = function(e) {
				stop("Plot error. Try adding + tm_check_fix()", call. = FALSE)
			})
			if (o$show_gif_ani) {
				dev.off()
			}

		}, gts, seq_along(gts), SIMPLIFY = FALSE)
		if (!is.null(o$vp)) upViewport(1)
	}
	if (length(gts) == 1) gts = gts[[1]]

	if (show && o$show_gif_ani) {
		files = list.files(path = d, pattern = "^plot[0-9]{3}\\.png$", full.names = TRUE)
		if (o$play == "pingpong") files = c(files, rev(files))
		filename = tempfile(fileext = ".gif")
		create_animation(filename = filename, files = files, width = devsize[1], height = devsize[2], delay = NA, fps = o$fps, loop = o$play != "once", progress = FALSE, gif = TRUE, showAni = TRUE, dpr = o$dpr)
	}

	gts
}
