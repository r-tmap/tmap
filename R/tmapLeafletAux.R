tmapLeafletTilesPrep = function(a, bs, o) {
	tiles = lapply(1L:length(bs), function(i) a)
	.TMAP_LEAFLET$tiles = tiles
	paste0(a$server, collapse = "__")
}

tmapLeafletTiles = function(bi, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	
	
	tiles = .TMAP_LEAFLET$tiles[[bi]]
	
	for (s in tiles$server) lf = leaflet::addProviderTiles(lf, provider = s, group = s)
	assign_lf(lf, facet_row, facet_col, facet_page)
	NULL	
}
