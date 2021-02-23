shapeTM = function(shp, tmapID = NULL) {
	x = structure(list(shp = shp, tmapID = tmapID), class = "shapeTM")
}

stm_bbox = function(shpTM) {
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	if (inherits(shp, "stars")) {
		shp$values[tmapID] = TRUE
		sf::st_bbox(trim_stars(shp))
	} else if (inherits(shp, c("sf", "sfc"))) {
		sf::st_bbox(shp[tmapID])
	} else {
		stop("unknown shape class")
	}
}

stm_bbox_all = function(shpTM) {
	sf::st_bbox(shpTM$shp)
}