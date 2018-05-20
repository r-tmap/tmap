process_tiles <- function(g, gt) {
	if (is.null(g$server)) {
		server <- NA
		alpha <- if (is.na(g$alpha)) 1 else g$alpha	
	} else {
		if (!is.vector(g$server)) {
			stop("The first argument of tm_tiles and tm_basemap, called \"server\", should contain a character value/vector of server URLs or names.", call. = FALSE)
		}
		
		if (is.na(g$server[1])) {
			if (g$grouptype == "base") {
				server <- gt$basemaps 
				alpha <- if (is.na(g$alpha)) gt$basemaps.alpha else g$alpha		
			} else {
				server <- gt$overlays 
				alpha <- if (is.na(g$alpha)) gt$overlays.alpha else g$alpha		
			}
		} else {
			server <- g$server
			alpha <- rep(if (is.na(g$alpha)) 1 else g$alpha, length.out = length(server))
		}
	} 
	
	group <- rep(if (is.na(g$group) && !is.null(names(server))) names(server) else g$group, length.out = length(server))
	list(tile.server = server, tile.alpha = alpha, tile.group = group, tile.grouptype = g$grouptype)
}
