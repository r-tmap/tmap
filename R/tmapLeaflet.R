tmapLeafletSetup = function() {
	list(crs = 4326) #3857
}

tmapLeafletInit = function(bbx) {
	if (!requireNamespace("leaflet")) stop("grid package required but not installed yet.")
	bbx = unname(bbx)
	lf = leaflet::leaflet() %>% leaflet::addTiles() %>% leaflet::fitBounds(bbx[1], bbx[2], bbx[3], bbx[4])
	assign("lf", lf, envir = .TMAP_LEAFLET)
	NULL
}

tmapLeafletPolygons = function(x, fill, color) {
	lf = get("lf", envir = .TMAP_LEAFLET)
	lf = lf %>% leaflet::addPolygons(data = x, color = unname(color), fillColor = unname(fill), opacity = 1, fillOpacity = 1)
	assign("lf", lf, envir = .TMAP_LEAFLET)
	NULL
}

tmapLeafletRun = function() {
	lf = get("lf", envir = .TMAP_LEAFLET)
	print(lf)
}
