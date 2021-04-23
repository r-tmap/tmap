shapeTM = function(shp, tmapID = NULL) {
	x = structure(list(shp = shp, tmapID = tmapID), class = c("shapeTM", "list"))
}

# stm_bbox = function(shpTM) {
# 	shp = shpTM$shp
# 	tmapID = shpTM$tmapID
# 	if (inherits(shp, "stars")) {
# 		shp$values[tmapID] = TRUE
# 		sf::st_bbox(trim_stars(shp))
# 	} else if (inherits(shp, c("sf", "sfc"))) {
# 		sf::st_bbox(shp[tmapID])
# 	} else {
# 		stop("unknown shape class")
# 	}
# }

stm_bbox = function(shpTM, tmapID) {
	shp = shpTM$shp
	shpID = shpTM$tmapID
	
	ids = match(tmapID, shpID)
	
	if (inherits(shp, "stars")) {
		shp$values[ids] = TRUE
		sf::st_bbox(trim_stars(shp))
	} else if (inherits(shp, c("sf", "sfc"))) {
		sf::st_bbox(shp[ids])
	} else {
		stop("unknown shape class")
	}
}


stm_merge_bbox = function(blist) {
	m = do.call(rbind, blist)
	st_bbox(c(apply(m[, 1:2, drop = FALSE], 2, min), apply(m[, 3:4, drop = FALSE], 2, max)))
}


stm_bbox_all = function(shpTM) {
	sf::st_bbox(shpTM$shp)
}