tmapLeafletShape = function(bbx, facet_row, facet_col, facet_page, o) {
	bbx = unname(bbx)
	if (!.TMAP$proxy) {
		get_lf(facet_row, facet_col, facet_page) %>%
			leaflet::fitBounds(bbx[1], bbx[2], bbx[3], bbx[4]) %>%
			view_set_bounds(bbx, o) %>%
			assign_lf(facet_row, facet_col, facet_page)
	}
	NULL
}

tmapLeafletOverlay = function(bbx, facet_row, facet_col, facet_page, o) {
	NULL
}
