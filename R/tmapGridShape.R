

tmapGridShape = function(bbx, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)



	fbbx = bb_asp(bbx, g$fasp)

	rc_text = frc(facet_row, facet_col)

	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]

	bg = if (is.na(o$frame) || is.na(o$bg.color)) {
		if (o$earth_boundary) {
			eotw = end_of_the_world(o$crs_step4, earth_datum = o$earth_datum)
			sf::st_as_grob(eotw, gp = grid::gpar(fill = o$bg.color, lwd = 0), name = "inner_world")
		} else {
			NULL
		}
	} else {
		if (o$earth_boundary) {
			eotw = end_of_the_world(o$crs_step4, earth_datum = o$earth_datum)
			grid::gList(rndrectGrob(gp=gpar(fill=o$space.color, lwd=0, lineend="square"), r = o$frame.r, name = "inner_rect"),
						sf::st_as_grob(eotw, gp = grid::gpar(fill = o$bg.color, lwd = 0), name = "inner_world"))
		} else {
			rndrectGrob(gp=gpar(fill=o$bg.color, lwd=0, lineend="square"), r = o$frame.r, name = "inner_rect")
		}


	}

	clip = if ((is.na(o$frame) || is.na(o$bg.color)) && o$earth_boundary) {
		grid::as.path(bg)
	} else TRUE

	gtmap = grid::grobTree(bg,
						   vp = grid::vpStack(grid::viewport(layout.pos.col = colid, layout.pos.row = rowid, name = paste0("vp_facet_", rc_text)),
						   				   grid::viewport(xscale = fbbx[c(1,3)], yscale = fbbx[c(2,4)], name = paste0("vp_map_", rc_text), clip = clip)), name = paste0("gt_facet_", rc_text))

	gts[[facet_page]] = grid::addGrob(gts[[facet_page]], gtmap, gPath = grid::gPath("gt_main"))

	assign("gts", gts, envir = .TMAP_GRID)
	NULL
}

tmapGridOverlay = function(bbx, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)

	gt = gts[[facet_page]]

	rc_text = frc(facet_row, facet_col)

	frame.lwd = if (identical(o$frame, FALSE)) 0 else o$frame.lwd * o$scale
	frame.col = if (identical(o$frame, FALSE)) NA else if (identical(o$frame, TRUE)) o$attr.color else o$frame
	r = o$frame.r * o$scale

	boundary = if (o$earth_boundary) {
		fbbx = bb_asp(bbx, g$fasp)

		eotw = end_of_the_world(o$crs_step4, earth_datum = o$earth_datum)

		sf::st_as_grob(eotw, gp = grid::gpar(fill = NA, col = o$earth_boundary.color, lwd = o$earth_boundary.lwd), name = "outer_world", vp = grid::viewport(xscale = fbbx[c(1,3)], yscale = fbbx[c(2,4)]))
	} else NULL

	frame = if (!(is.na(frame.col))) {
		pH = convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*frame.lwd
		pW = convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*frame.lwd

		if (o$frame.double_line) {
			bgcol = substr(o$bg.color, 1, 7) # remove transparency
			grid::grobTree(
				rndrectGrob(width = 1-4*pW, height=1-4*pH, gp=gpar(col=bgcol, fill=NA, lwd=4*frame.lwd, lineend="square"), r = r, name = "outer_frame"),
				rndrectGrob(gp=gpar(col=frame.col, fill=NA, lwd=1.5*frame.lwd, lineend="square"), r = r, name = "between_frame"),
				rndrectGrob(width = 1-8*pW, height=1-8*pH, gp=gpar(col=frame.col, fill=NA, lwd=frame.lwd, lineend="square"), r = r, name = "inner_frame"))
		} else {
			rndrectGrob(width = 1-0.5*pW, height = 1-0.5*pH, gp=gpar(col=frame.col, fill=NA, lwd=frame.lwd, lineend="square"), r = r, name = "outer_frame")
			#rectGrob(width = 1-0.5*pW, height = 1-0.5*pH, gp=gpar(col=frame.col, fill=NA, lwd=frame.lwd, lineend="square"), name = "outer_frame")
		}

	} else NULL


	#gt_name = paste0("gt_facet_", rc_text)

	#gts[[facet_page]] = grid::addGrob(gts[[facet_page]], frame, gPath = grid::gPath(gt_name))

	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]

	if (o$earth_boundary) {
		gt = gt |>
			#add_to_gt(boundary, row = rowid, col = colid)
			grid::addGrob(boundary, paste0("gt_facet_", rc_text))
	}

	gt = gt |>
		add_to_gt(frame, row = rowid, col = colid)
	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)
	NULL

}
