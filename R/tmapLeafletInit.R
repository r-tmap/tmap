tmapLeafletInit = function(o) {
	if (!requireNamespace("leaflet")) stop("grid package required but not installed yet.")
	#bbx = unname(bbx)
	
	per_page = rep(o$ncols * o$nrows, o$npages)
	k = o$ncols * o$nrows * o$npages
	if (o$n < k) {
		per_page[o$npages] = per_page[o$npages] - (k - o$n)
	}
	
	
	lfs = lapply(per_page, function(p) {
		lapply(seq_len(p), function(i) {
			leaflet::leaflet() |> leaflet::addTiles()# |> leaflet::fitBounds(bbx[1], bbx[2], bbx[3], bbx[4])
		})
	})
	
	assign("lfs", lfs, envir = .TMAP_LEAFLET)
	assign("nrow", o$nrows, envir = .TMAP_LEAFLET)
	assign("ncol", o$ncols, envir = .TMAP_LEAFLET)
	NULL
}
