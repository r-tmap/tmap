tmapLeafletRun = function(o, show, knit, args) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	
	lfs2 = lapply(lfs, function(lfsi) {
		x = if (o$nrows == 1 && o$ncols == 1) {
			lfsi[[1]]
		} else {
			fc = o$free.coords
			sync = if (identical(o$sync, TRUE) || all(!fc)) {
				"all"
			} else if (all(fc)) {
				"none"
			} else if (fc[1]) {
				asplit(matrix(1:(o$nrows*o$ncols), ncol = o$ncols, byrow = TRUE), 1)
			} else {
				asplit(matrix(1:(o$nrows*0$ncols), ncol = 0$ncols, byrow = TRUE), 2)
			}
			marg = paste0(o$between.margin, "em")
			
			#print(do.call(leafsync::latticeView, c(lfsi, list(ncol = o$ncols, sync = sync, sync.cursor = all(!fc), no.initial.sync = FALSE, between = list(x = marg, y = marg)))))
			do.call(leafsync::latticeView, c(lfsi, list(ncol = o$ncols, sync = sync, sync.cursor = all(!fc), no.initial.sync = FALSE)))
		}
		if (o$pc$sepia.intensity != 0) {
			col = process_color("#ffffff", sepia.intensity = o$pc$sepia.intensity)
			htmlwidgets::prependContent(x, htmltools::tags$style(paste0(
				".leaflet-control-layers {background: ", col, ";}
				.leaflet-control-zoom-in {background: ", col, " !important;}
				.leaflet-control-zoom-out {background: ", col, " !important;}"
			)))
		} else {
			x
		}
	})
	
	if (length(lfs2) == 1) lfs2 = lfs2[[1]]
	if (show && !knit) {
		print(lfs2)
	}
	lfs2
}
