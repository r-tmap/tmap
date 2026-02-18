get_facet_id = function(row, col, nrow, ncol) {
	col + (row - 1L) * ncol
}

get_lf = function(facet_row, facet_col, facet_page) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)

	lfsi = lfs[[facet_page]]

	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2

	lfid = get_facet_id(fr, fc, nrow, ncol)

	lfsi[[lfid]]
}

assign_lf = function(lf, facet_row, facet_col, facet_page) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)


	fr = max(1, facet_row) # facet_row can be -1 or -2
	fc = max(1, facet_col) # facet_row can be -1 or -2



	lfid = get_facet_id(fr, fc, nrow, ncol)

	lfs[[facet_page]][[lfid]] = lf
	assign("lfs", lfs, envir = .TMAP_LEAFLET)
	NULL
}

getClusterOpts = function(clustering) {
	if (identical(clustering, TRUE)) {
		clusterOpts = leaflet::markerClusterOptions()
	} else if (identical(clustering, FALSE)) {
		clusterOpts = NULL
	} else {
		clusterArgs = setdiff(names(formals(leaflet::markerClusterOptions)), "...")
		if (!all(clusterArgs %in% names(clustering))) cli::cli_abort("{.field clustering} Cluster options should have the same options as {.fun leaflet::markerClusterOptions}")
		clusterOpts = clustering
	}
}


tmapLeafletWrap = function(label, facet_row, facet_col, facet_page, o) {

	NULL
}

tmapLeafletXtab = function(label, facet_row, facet_col, facet_page, o) {
}
