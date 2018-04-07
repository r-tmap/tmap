process_tiles <- function(g, gt) {
	server <- if (is.null(g$server)) {
		NA
	} else if (is.na(g$server)) {
		gt$basemaps 
	} else g$server
	alpha <- if (is.na(g$alpha)) gt$basemaps.alpha else g$alpha
	list(tile.server = server, tile.alpha = alpha, tile.group = g$group, tile.grouptype = g$grouptype)
}
