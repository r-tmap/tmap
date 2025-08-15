#' Internal tmap function to create a tmap shape
#'
#' Internal tmap function to create a tmap shape
#'
#' @param shp Shape file
#' @param tmapID tmap Identifier
#' @param bbox bounding box
#' @export
#' @keywords internal
shapeTM = function(shp, tmapID = NULL, bbox = NULL, ...) {
	# filter empty geometries
	if (inherits(shp, "sfc")) {
		is_empty = sf::st_is_empty(shp)
		shp = shp[!is_empty]
		tmapID = tmapID[!is_empty]
	}
	x = structure(list(shp = shp, tmapID = tmapID, bbox = bbox, ...), class = c("shapeTM", "list"))
}


stm_bbox = function(shpTM, tmapID, crs) {
	bbox = shpTM$bbox

	if (!is.null(bbox$x)) {
		if (identical(bbox$x, "FULL")) {
			bbox$x = sf::st_bbox(c(xmin = -180, xmax = 180, ymax = 90, ymin = -90), crs = sf::st_crs(4326))
		}
		# to make sure the supplied bounding box will be converted in the correct crs
		bbox$projection = crs
	} else {
		shp = shpTM$shp
		shpID = shpTM$tmapID

		if (length(tmapID) == 0) return(sf::st_bbox(NA_real_))
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
		bbox$x = sf::st_bbox(shp)
	}
	if (inherits(bbox$x, "SpatExtent")) bbox$x = sf::st_bbox(bbox$x)
	do.call(tmaptools::bb, bbox)
}


stm_merge_bbox = function(blist) {
	crs = sf::st_crs(blist[[1]])
	m = do.call(rbind, blist)
	sf::st_bbox(c(apply(m[, 1:2, drop = FALSE], 2, min), apply(m[, 3:4, drop = FALSE], 2, max)), crs = crs)
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
	sasp = get_asp_ratio(bbx)

	fact = sasp / asp

	if (fact > 1) {
		cy = mean(bbx[c(2,4)])
		dy = bbx[4] - bbx[2]
		dy2 = dy * fact
		bbx[2] = cy - (dy2 / 2)
		bbx[4] = cy + (dy2 / 2)
	} else if (fact < 1) {
		cx = mean(bbx[c(1,3)])
		dx = bbx[3] - bbx[1]
		dx2 = dx / fact
		bbx[1] = cx - (dx2 / 2)
		bbx[3] = cx + (dx2 / 2)
	}
	bbx
}

bb_ll_valid = function(bbx) {
	bbx[2] = max(bbx[2], -90)
	bbx[4] = min(bbx[4], 90)
	bbx
}
