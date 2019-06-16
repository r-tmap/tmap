process_tiles <- function(g, gt) {
	if (is.null(g$server)) {
		server <- NA
		alpha <- if (is.na(g$alpha)) 1 else g$alpha	
		group <- NULL
		ns <- 1
	} else {
		if (!is.vector(g$server)) {
			stop("The first argument of tm_tiles and tm_basemap, called \"server\", should contain a character value/vector of server URLs or names.", call. = FALSE)
		}
		
		
		if (is.na(g$server[1])) {
			if (g$gtype == "base") {
				server <- gt$basemaps 
				ns <- length(server)
				alpha <- rep(if (is.na(g$alpha)) gt$basemaps.alpha else g$alpha, length.out = ns)	
			} else {
				server <- gt$overlays 
				ns <- length(server)
				alpha <- rep(if (is.na(g$alpha)) gt$overlays.alpha else g$alpha, length.out = ns)	
			}
		} else {
			server <- g$server
			ns <- length(server)
			alpha <- rep(if (is.na(g$alpha)) 1 else g$alpha, length.out = ns)
		}
		group <- g$group
	} 
	
	group <- if (is.null(group)) {
		NULL
	} else rep(if (is.na(group) && !is.null(names(server))) names(server) else group, length.out = ns)
	
	tms <- rep(g$tms, ns)
	
	list(tile.server = server, tile.alpha = alpha, tile.group = group, tile.gtype = g$gtype, tile.zindex = g$zindex, tile.tms = tms)
}
