tmapLeafletRun = function(o) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	
	lapply(lfs, function(lfsi) {
		if (o$nrows == 1 && o$ncols == 1) {
			print(lfsi[[1]])
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
			print(do.call(leafsync::latticeView, c(lfsi, list(ncol = o$ncols, sync = sync, sync.cursor = all(!fc), no.initial.sync = FALSE))))
		}
	})
}
