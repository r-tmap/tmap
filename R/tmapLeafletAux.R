tmapLeafletTilesPrep = function(a, bs, id, o) {
	tiles = lapply(1L:length(bs), function(i) a)
	.TMAP_LEAFLET$tiles[[id]] = tiles
	paste0(a$server, collapse = "__")
}

tmapLeafletTiles = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {

	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)


	tiles = .TMAP_LEAFLET$tiles[[id]][[bi]]

	if (o$credits.defined) {
		opt = leaflet::tileOptions(attribution = "", maxNativeZoom = tiles$max.native.zoom, pane = pane)
	} else {
		opt = leaflet::tileOptions(maxNativeZoom = tiles$max.native.zoom, pane = pane)
	}
	if (!is.na(o$set_zoom_limits[2])) opt$maxZoom = o$set_zoom_limits[2]

	for (s in tiles$server) if (s != "") lf = leaflet::addProviderTiles(lf, provider = s, group = s, options = opt)

	assign_lf(lf, facet_row, facet_col, facet_page)
	NULL
}

tmapLeafletGridPrep = function(a, bs, id, o) {

	grid_intervals = vapply(bs, function(b) {
		# implementation similarish as plot mode but needs polishing
		dx = b[3] - b[1]
		dy = b[4] - b[2]
		# if not specified, aim for 15 lines in total
		n.x = if (!is.na(a$n.x)) a$n.x else if (is.na(a$x)) 7.5 else length(a$x)
		n.y = if (!is.na(a$n.y)) a$n.y else if (is.na(a$y)) 7.5 else length(a$x)

		x = pretty30(b[c(1,3)], n=n.x, longlat = !is.na(a$crs) && sf::st_is_longlat(a$crs))
		y = pretty30(b[c(2,4)], n=n.y, longlat = !is.na(a$crs) && sf::st_is_longlat(a$crs))

		max((x[2] - x[1]), (y[2]-y[1]))
	}, FUN.VALUE = numeric(1))
	assign("grid_intervals", grid_intervals, envir = .TMAP_LEAFLET)

	return("grid")
}

tmapLeafletGrid = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	lf = get_lf(facet_row, facet_col, facet_page)
	rc_text = frc(facet_row, facet_col)

	grid_intervals = get("grid_intervals", envir = .TMAP_LEAFLET)

	lf = leaflet::addGraticule(lf, interval = grid_intervals[bi], options = pathOptions(pane = pane))


	assign_lf(lf, facet_row, facet_col, facet_page)
	NULL
}

tmapLeafletGridXLab = function(bi, bbx, facet_row, facet_col, facet_page, o) {
	NULL
}

tmapLeafletGridYLab = function(bi, bbx, facet_row, facet_col, facet_page, o) {
	NULL
}
