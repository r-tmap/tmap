

tmapGridShape = function(bbx, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)

	design = getOption("tmap.design.mode")


	fbbx = bb_asp(bbx, g$fasp)

	rc_text = frc(facet_row, facet_col)

	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]

	bg = if (o$earth_boundary) {
		if (!o$space || design) o$space.color = NA
		eotw = end_of_the_world(o$crs_step4, earth_datum = o$earth_datum)
		grid::gList(rndrectGrob(gp=gpar(fill=o$space.color, lwd=NA, lineend="square"), r = o$frame.r * o$scale, name = "inner_rect"),
					sf::st_as_grob(eotw, gp = grid::gpar(fill = o$bg.color, lwd = NA), name = "inner_world"))

	} else if (design) {
		NULL
	} else {
		rndrectGrob(gp=gpar(fill=o$bg.color, lwd=NA, lineend="square"), r = o$frame.r * o$scale, name = "inner_rect")
	}

	clip = if (is.na(o$bg.color) && o$earth_boundary && !o$space_overlay) {
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
	frame.lwd = if (!o$frame) 0 else o$frame.lwd * o$scale
	r = o$frame.r * o$scale

	boundary = if (o$earth_boundary) {
		fbbx = bb_asp(bbx, g$fasp)

		eotw = end_of_the_world(o$crs_step4, earth_datum = o$earth_datum)

		sf::st_as_grob(eotw, gp = grid::gpar(fill = NA, col = o$earth_boundary.color, lwd = o$earth_boundary.lwd), name = "outer_world", vp = grid::viewport(xscale = fbbx[c(1,3)], yscale = fbbx[c(2,4)]))
	} else NULL

	gframe = if (o$frame) {
		pH = convertHeight(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*frame.lwd
		pW = convertWidth(unit(1, "points"), unitTo = "npc", valueOnly = TRUE)*frame.lwd

		if (o$frame.double_line) {
			bgcol = substr(o$bg.color, 1, 7) # remove transparency
			grid::grobTree(
				rndrectGrob(width = 1-4*pW, height=1-4*pH, gp=gpar(col=bgcol, fill=NA, lwd=4*frame.lwd, lineend="square"), r = r, name = "outer_frame"),
				rndrectGrob(gp=gpar(col=o$frame.color, fill=NA, lwd=1.5*frame.lwd, lineend="square", alpha = o$frame.alpha), r = r, name = "between_frame"),
				rndrectGrob(width = 1-8*pW, height=1-8*pH, gp=gpar(col=o$frame.color, fill=NA, lwd=frame.lwd, lineend="square", alpha = o$frame.alpha), r = r, name = "inner_frame"))
		} else {
			rndrectGrob(width = 1, height = 1, gp=gpar(col=o$frame.color, fill=NA, lwd=frame.lwd, lineend="square", alpha = o$frame.alpha), r = r, name = "outer_frame")
			#rectGrob(width = 1-0.5*pW, height = 1-0.5*pH, gp=gpar(col=frame.col, fill=NA, lwd=frame.lwd, lineend="square"), name = "outer_frame")
		}

	} else NULL



	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]

	if (o$earth_boundary && o$space_overlay) {
		space_fill = local({
			if (o$frame) {
				gframe_path = roundrect_to_pathGrob(gframe)
			} else {
				gframe_path = viewport_frame_path()
			}
			boundary_npc = local({
				x = as.vector(boundary$x)
				x2 = (x - boundary$vp$xscale[1]) / diff(boundary$vp$xscale)
				y = as.vector(boundary$y)
				y2 = (y - boundary$vp$yscale[1]) / diff(boundary$vp$yscale)
				boundary$vp = NULL
				boundary$x = unit(x2, "npc")
				boundary$y = unit(y2, "npc")
				boundary
			})
			combine_paths(gframe_path, boundary_npc, fill = o$space.color)
		})
		gt = gt |>
			grid::addGrob(space_fill, paste0("gt_facet_", rc_text))
	}

	if (o$earth_boundary) {
		gt = gt |>
			grid::addGrob(boundary, paste0("gt_facet_", rc_text))
	}

	gt = gt |>
		add_to_gt(gframe, row = rowid, col = colid)
	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)
	NULL

}



## functions to create a grob that draws space 'space'
## needed because clipping by earth boundary does not always work (e.g. not for rasterGrobs #1170)

combine_paths = function(pg1, pg2, rule = "winding", fill = "black") {
	# Combine coordinates and assign IDs
	combined_x = unit.c(pg1$x, pg2$x)
	combined_y = unit.c(pg1$y, pg2$y)
	combined_id = c(rep(1, length(pg1$x)), rep(2, length(pg2$x)))

	# Return compound path
	pathGrob(
		x = combined_x,
		y = combined_y,
		id = combined_id,
		rule = rule,
		gp = gpar(fill = fill, col = NA)
	)
}

roundrect_to_pathGrob = function(roundrect, n_corner = 10, fill = "black", col = NA, rule = "winding") {
	stopifnot(inherits(roundrect, "roundrect"))

	# Extract and convert parameters to NPC coordinates
	x0 = convertX(roundrect$x - roundrect$width / 2, "npc", valueOnly = TRUE)
	x1 = convertX(roundrect$x + roundrect$width / 2, "npc", valueOnly = TRUE)
	y0 = convertY(roundrect$y - roundrect$height / 2, "npc", valueOnly = TRUE)
	y1 = convertY(roundrect$y + roundrect$height / 2, "npc", valueOnly = TRUE)
	rx = convertWidth(roundrect$r, "npc", valueOnly = TRUE)
	ry = convertHeight(roundrect$r, "npc", valueOnly = TRUE)

	# Clamp corner radius to avoid overlaps
	rx = min(rx, (x1 - x0) / 2)
	ry = min(ry, (y1 - y0) / 2)

	# Helper: ellipse arc between angles
	arc = function(cx, cy, rx, ry, start, end, n = 10) {
		theta = seq(start, end, length.out = n)
		list(
			x = cx + rx * cos(theta),
			y = cy + ry * sin(theta)
		)
	}

	# Define corner arcs (clockwise from upper-left)
	ul = arc(x0 + rx, y1 - ry, rx, ry, pi / 2, pi, n_corner)
	ur = arc(x1 - rx, y1 - ry, rx, ry, 0, pi / 2, n_corner)
	lr = arc(x1 - rx, y0 + ry, rx, ry, -pi / 2, 0, n_corner)
	ll = arc(x0 + rx, y0 + ry, rx, ry, -pi, -pi / 2, n_corner)

	# Assemble path: arcs and straight edges between them
	x_coords = c(
		rev(ul$x),
		ur$x[n_corner],
		rev(ur$x),
		lr$x[n_corner],
		rev(lr$x),
		ll$x[n_corner],
		rev(ll$x),
		ul$x[n_corner]
	)

	y_coords = c(
		rev(ul$y),
		ur$y[n_corner],
		rev(ur$y),
		lr$y[n_corner],
		rev(lr$y),
		ll$y[n_corner],
		rev(ll$y),
		ul$y[n_corner]
	)

	# Return as filled path grob
	pathGrob(
		x = unit(x_coords, "npc"),
		y = unit(y_coords, "npc"),
		id = rep(1, length(x_coords)),
		rule = rule,
		gp = gpar(fill = fill, col = col)
	)
}


viewport_frame_path = function(fill = "black", col = NA, rule = "winding") {
	# Clockwise path around the viewport (0,0) to (1,1)
	x = unit(rev(c(0, 1, 1, 0, 0)), "npc")
	y = unit(rev(c(0, 0, 1, 1, 0)), "npc")

	pathGrob(
		x = x,
		y = y,
		id = rep(1, length(x)),
		rule = rule,
		gp = gpar(fill = fill, col = col)
	)
}
