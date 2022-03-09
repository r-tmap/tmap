get_facet_id = function(row, col, nrow, ncol) {
	col + (row - 1L) * ncol
}

get_lf = function(facet_row, facet_col, facet_page) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)
	
	lfsi = lfs[[facet_page]]
	
	lfid = get_facet_id(facet_row, facet_col, nrow, ncol)
	
	lfsi[[lfid]]
}

assign_lf = function(lf, facet_row, facet_col, facet_page) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)

	lfid = get_facet_id(facet_row, facet_col, nrow, ncol)
	
	lfs[[facet_page]][[lfid]] = lf
	assign("lfs", lfs, envir = .TMAP_LEAFLET)
	NULL
}

tmapLeafletWrap = function(label, facet_row, facet_col, facet_page, o) {
	NULL
}

tmapLeafletXtab = function(label, facet_row, facet_col, facet_page, o) {
}