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
	
	# filter the shape?
	do_filter = (length(tmapID) != length(shpID)) || (!all(tmapID == shpID))

	if (do_filter) {
		ids = match(tmapID, shpID)
		
		if (inherits(shp, "dimensions")) {
			m = matrix(NA, nrow = nrow(shp), ncol = ncol(shp))
			m[ids] = TRUE
			
			s = stars::st_as_stars(list(values = m), dimensions = shp)
			shp = trim_stars(s)
		} else if (inherits(shp, c("sf", "sfc"))) {
			shp = shp[ids]
		} else {
			stop("unknown shape class")
		}
	}
	sf::st_bbox(shp)

}


stm_merge_bbox = function(blist) {
	m = do.call(rbind, blist)
	sf::st_bbox(c(apply(m[, 1:2, drop = FALSE], 2, min), apply(m[, 3:4, drop = FALSE], 2, max)))
}


stm_bbox_all = function(shpTM) {
	sf::st_bbox(shpTM$shp)
}

bb_ext = function(bbx, ext = c(0, 0, 0, 0)) {
	dx = bbx[3] - bbx[1]
	dy = bbx[4] - bbx[2]
	
	bbx[2] = bbx[2] - ext[1] * dy 
	bbx[1] = bbx[1] - ext[2] * dx
	bbx[4] = bbx[4] + ext[3] * dy 
	bbx[3] = bbx[3] + ext[4] * dx
	bbx
}

bb_asp = function(bbx, asp) {
	dx = bbx[3] - bbx[1]
	dy = bbx[4] - bbx[2]
	
	cx = mean(bbx[c(1,3)])
	cy = mean(bbx[c(2,4)])
	
	basp = dx/dy
	if (basp > asp) {
		dy2 = dx / asp
		bbx[2] = cy - (dy2 / 2)
		bbx[4] = cy + (dy2 / 2)
	} else {
		dx2 = dy * asp
		bbx[1] = cx - (dx2 / 2)
		bbx[3] = cx + (dx2 / 2)
	}	
	bbx
}
