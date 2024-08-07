tmapLeafletRun = function(o, q, show, knit, args) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	
	lfs2 = lapply(lfs, function(lfsi) {
		x = if (o$nrows == 1 && o$ncols == 1) {
			lf = lfsi[[1]]
			# proxy remove
			if (!is.null(.TMAP_LEAFLET$layerIds2)) {
				L = .TMAP_LEAFLET$layerIds
				for (L2 in .TMAP_LEAFLET$layerIds2) {
					if (L2$type == "raster") {
						lf = leaflet::removeImage(lf, L2$Lid)
					} else if (L2$type %in% c("symbols", "text")) {
						lf = leaflet::removeMarker(lf, L2$Lid)
					} else {
						lf = leaflet::removeShape(lf, L2$Lid)
					}
					L = lapply(L, function(x) {
						if (any(L2$Lid %in% x$Lid)) NULL else x
					})
					L = L[!vapply(L, is.null, FUN.VALUE = logical(1))]
				}
				# to do: update group and type arguments 
				.TMAP_LEAFLET$layerIds = L
				.TMAP_LEAFLET$layerIds2 = NULL
			}
			lf
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
		if (o$pc$sepia.intensity != 0 && !.TMAP$in.shiny) {
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
	if (show && !knit && !.TMAP$in.shiny) {
		print(lfs2)
	}
	lfs2
}
