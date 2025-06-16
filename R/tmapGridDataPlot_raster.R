#' @export
#' @rdname tmapGridLeaflet
tmapGridDataPlot.tm_data_raster = function(a, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	gts = get("gts", .TMAP_GRID)
	#bbx = get("bbx", .TMAP_GRID)

	g = get("g", .TMAP_GRID)

	g$fasp


	rc_text = frc(facet_row, facet_col)


	bb_target <- bb_asp(bbx, g$fasp)
	bb_real <-  stm_bbox_all(shpTM)

	shp = shpTM$shp
	tmapID = shpTM$tmapID

	if (is_regular_grid(shp)) {

		if (nrow(dt) == length(tmapID)) {
			# no matching needed

			if (all(dt$col_alpha == 1)) {
				color = dt$col
			} else {
				dt = merge_alpha(dt, name = "col")
				color = dt$ca
			}
		} else {
			# matching: to be improved
			tid = intersect(tmapID, dt$tmapID__)

			color = rep(NA, length(tmapID)) #"#FFFFFF"

			sel = which(tmapID %in% tid)
			tid2 = tmapID[sel]

			if (all(dt$col_alpha == 1)) {
				color[sel] = dt$col[match(tid2, dt$tmapID__)]
			} else {
				dt = merge_alpha(dt, name = "col")
				color[sel] = dt$ca[match(tid2, dt$tmapID__)]
			}
		}


		if (all(abs(bb_real-bb_target)< 1e-3)) {
			width <- 1
			height <- 1
			cent <- c(mean.default(c(bb_target[1], bb_target[3])), mean.default(c(bb_target[2], bb_target[4])))
		} else {
			width <- (bb_real[3] - bb_real[1]) / (bb_target[3] - bb_target[1])
			height <- (bb_real[4] - bb_real[2]) / (bb_target[4] - bb_target[2])
			cent <- c(mean.default(c(bb_real[1], bb_real[3])), mean.default(c(bb_real[2], bb_real[4])))
		}

		cx <- (cent[1] - bb_target[1]) / (bb_target[3] - bb_target[1])
		cy <- (cent[2] - bb_target[2]) / (bb_target[4] - bb_target[2])

		m <- matrix(color, ncol=nrow(shp), nrow=ncol(shp), byrow = TRUE)

		y_is_pos <- local({
			vals = stars::st_get_dimension_values(shp, "y")
			if (!is.null(vals)) {
				!all(diff(vals) < 0)
			} else {
				rst = attr(shp, "raster")
				name_y = names(get_xy_dim(shp))[2]
				delta = shp[[name_y]]$delta
				delta > 0
			}
		})

		if (y_is_pos) {
			m <- m[nrow(m):1L, ]
		}
		m[is.na(m)] = NA #"#0000FF"

		grb = grid::rasterGrob(m, x=cx, y=cy, width=width, height=height, interpolate = a$interpolate, name = paste0("raster_", id)) #gpl$raster.misc$interpolate
		gt = grid::addGrob(gts[[facet_page]], grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
		gts[[facet_page]] = gt
		assign("gts", gts, envir = .TMAP_GRID)
	} else {
		m = matrix(tmapID, nrow = nrow(shp), ncol = ncol(shp))
		shp2 = structure(list(tmapID = m), class = "stars", dimensions = shp)
		shpTM = shapeTM(sf::st_geometry(sf::st_as_sf(shp2)), tmapID)

		#dt[, ":="(ord__ = 1, fill = col, fill_alpha = col_alpha, lty = "solid")]
		#dt[, ":="(col_alpha = 0)]

		dt[, ":="(ord__ = 1, lty = "solid")]

		a2 = structure(list(), class = "tm_data_polygons")
		tmapGridDataPlot(a2, shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
		#grid.shape(s, gp=grid::gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
}
