add_to_gt = function(gt, grb, row, col) {
	vp = grid::viewport(layout.pos.col = col, layout.pos.row = row)
	gtr = grid::grobTree(grb, vp = vp)
	
	grid::addGrob(gt, gtr, gPath = grid::gPath("gt_main"))
}



tmapGridInit = function(o) {
	if (!requireNamespace("grid")) stop("grid package required but not installed yet.")

	
	# 
	# nr2=o$nrows
	# nc2=o$ncols
	# width = ((1 - sum(fixedMargins[c(2, 4)])) - (nc2 * sum(panel.wrap.size[c(2,4)])) - (nc2 - 1) * between.marginW) / nc2
	# height = ((1 - sum(fixedMargins[c(1, 3)])) - (nr2 * sum(panel.wrap.size[c(1,3)])) - (nr2 - 1) * between.marginH) / nr2
	# 
	# nasp = (width / height) * dasp
	# 
	# print("nasp")
	# print(nasp)
	# 
	
	rows = with(o, {
		x = c(outer.margins.top = outer.margins[3],
			  meta.buffers.top.out = meta.buffers[3],
			  meta.margins.top = meta.margins[3],
			  meta.buffers.top.in = meta.buffers[3],
			  xylab.margins.top = xylab.margins[3],
			  
			  panel.xtab.top = panel.xtab.size[3],
			  grid.buffers.top = grid.buffers[3],
			  grid.margins.top = grid.margins[3],
			  
			  {if (o$nrows > 1) rep(c(panel.wrap.size[3], 0, panel.wrap.size[1], between.marginH), o$nrows -1) else NULL},
			  panel.wrap.size[3], 0, panel.wrap.size[1],
			  
			  grid.margins.top = grid.margins[1],
			  grid.buffers.top = grid.buffers[1],
			  panel.xtab.bottom = panel.xtab.size[1],

			  xylab.margins.bottom = xylab.margins[1],
			  meta.buffers.bottom.in = meta.buffers[1],
			  meta.margins.bottom = meta.margins[1],
			  meta.buffers.bottom.out = meta.buffers[1],
			  outer.margins.bottom = outer.margins[1])

		u = grid::unit(x, "npc")
		names(u) = names(x)
		u
	})
	
	cols = with(o, {
		x = c(outer.margins.left = outer.margins[2],
			  meta.buffers.left.out = meta.buffers[2],
			  meta.margins.left = meta.margins[2],
			  meta.buffers.left.in = meta.buffers[2],
			  xylab.margins.left = xylab.margins[2],
			  
			  panel.xtab.left = panel.xtab.size[2],
			  grid.buffers.left = grid.buffers[2],
			  grid.margins.left = grid.margins[2],
			  
			  {if (o$ncols > 1) rep(c(panel.wrap.size[2], 0, panel.wrap.size[4], between.marginW), o$ncols -1) else NULL},
			  panel.wrap.size[2], 0, panel.wrap.size[4],
			  
			  grid.margins.left = grid.margins[4],
			  grid.buffers.left = grid.buffers[4],
			  panel.xtab.right = panel.xtab.size[4],

			  xylab.margins.right = xylab.margins[4],
			  meta.buffers.right.in = meta.buffers[4],
			  meta.margins.right = meta.margins[4],
			  meta.buffers.right.out = meta.buffers[4],
			  outer.margins.right = outer.margins[4])

		u = grid::unit(x, "npc")
		names(u) = names(x)
		u
	})
	
	nr = length(rows)
	nc = length(cols)
	
	cols_facet_ids = 1:o$ncols * 4 + 6
	rows_facet_ids = 1:o$nrows * 4 + 6
	
	#if (o$panel.type == "xtab") {
		cols_panel_col_ids = cols_facet_ids
		cols_panel_row_id = ifelse(o$panel.xtab.pos[2] == "top", 6, nc - 5)
		
		rows_panel_row_ids = rows_facet_ids
		rows_panel_col_id = ifelse(o$panel.xtab.pos[1] == "left", 6, nc - 5)
	#} else if (o$panel.type == "wrap") {
		cols_panel_ids = cols_facet_ids + ifelse(o$panel.wrap.pos  == "left", -1, ifelse(o$panel.wrap.pos  == "right", 1, 0))
		rows_panel_ids = rows_facet_ids + ifelse(o$panel.wrap.pos  == "top", -1, ifelse(o$panel.wrap.pos  == "bottom", 1, 0))
		
		panel_col_rot = 0
		panel_row_rot = ifelse(o$panel.xtab.pos[1] == "left", 90, 270)
		panel_rot = ifelse(o$panel.wrap.pos  == "left", 90, ifelse(o$panel.wrap.pos  == "right", 270, 0))

	#}
	
	sum(rows[rows_facet_ids])
	
	#cols = convertWidth(cols, "inch")
	#rows = convertHeight(rows, "inch")
	
	sum(cols)
	sum(rows)
	
	names(rows)
	
	prows = as.numeric(rows)
	pcols = as.numeric(cols)
	
	fasp = ((1-sum(pcols)) / (1-sum(prows))) * o$dasp / o$ncols * o$nrows # asp per facet (with original outer margins)
	gasp = ((1-sum(pcols)) / (1-sum(prows))) * o$dasp # asp total facets (with original outer margins)
	
	
	if (!is.na(o$asp) && o$asp != 0) {
		# follow device
		fasp = o$asp
	} else if (is.na(o$asp) && !is.na(o$sasp)) {
		fasp = o$sasp
	}
	# print("fasp")
	# print(fasp)
	
	gasp2 = fasp * o$ncols / o$nrows # target gasp

	
	# print("gasp")
	# print(gasp)
	# print("gasp2")
	# print(gasp2)
	


	if (gasp2 > gasp) {
		extra.height =   (1 - ((1 - sum(pcols))/(gasp2/o$dasp))) - sum(prows)
		rows[c(1, length(rows))] = rows[c(1, length(rows))] + grid::unit(extra.height / 2, "npc")
	} else if (gasp2 < gasp) {
		extra.width =   (1 - ((1 - sum(prows)) * (gasp2/o$dasp))) - sum(pcols)
		cols[c(1, length(cols))] = cols[c(1, length(cols))] + grid::unit(extra.width / 2, "npc")
	}
	cols[cols_facet_ids] = (grid::unit(1, "npc") - sum(cols)) / o$ncols
	rows[rows_facet_ids] = (grid::unit(1, "npc") - sum(rows)) / o$nrows
	
	
	#rows[rows_facet_ids] = unit()
	
	
	
	vp_tree = grid::vpStack(grid::viewport(width = grid::unit(o$cw, "snpc"), height = grid::unit(o$ch, "snpc"), name = "vp_asp"),
							#grid::viewport(layout = grid::grid.layout(nrow = length(rows), ncol = length(cols)), name = "vp_main")
							grid::viewport(layout = grid::grid.layout(nrow = length(rows), ncol = length(cols), widths = cols, heights = rows), name = "vp_main")
	)
	
	#gt = grobTree(grid::rectGrob(gp=grid::gpar(fill="red")), vp = vp_tree)
	
	outerRect = if (!is.null(o$outer.bg.color)) grid::rectGrob(gp=grid::gpar(col=NA, lwd = 0, fill = o$outer.bg.color), name = "outer_rect") else NULL
	
	gts = lapply(1L:o$npages, function(ip) {
		grid::grobTree(
							outerRect,
							grid::grobTree(name = "gt_main"), 
							vp = vp_tree, name = "tmap_grob_tree")
	})
	
	
	g = list(
		rows_facet_ids = rows_facet_ids,
		cols_facet_ids = cols_facet_ids,
		
		rows_panel_ids = rows_panel_ids,
		cols_panel_ids = cols_panel_ids,
		
		rows_panel_row_ids = rows_panel_row_ids,
		rows_panel_col_id = rows_panel_col_id,
		cols_panel_row_id = cols_panel_row_id,
		cols_panel_col_ids = cols_panel_col_ids,
		
		panel_col_rot = panel_col_rot,
		panel_row_rot = panel_row_rot,
		panel_rot = panel_rot,
		
		meta_rows = c(3, (nr-2)),
		meta_cols = c(3, (nc-2)),
		
		fasp = fasp
	)
	
	
	if (getOption("tmap.design.mode")) {
		gts = lapply(gts, function(gt) {
			
			# length(rows)
			# length(cols)
			# 
			# prows = as.numeric(rows)
			# pcols = as.numeric(cols)
			# 
			# names(prows) = names(rows)
			# names(pcols) = names(cols)
			# 
			# cat("rows:\n")
			# print(prows)
			# cat("cols:\n")
			# print(pcols)
			
			#scales::show_col(pals::brewer.paired(12))
			p = rep(pals::brewer.paired(12), 3)
			
			gt = gt %>% 
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[1])), row = 1:(nr), col = 1:(nc)) %>%  # outer
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[2])), row = 2:(nr-1), col = 2:(nc-1)) %>%   # meta buffer out
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[3])), row = 3:(nr-2), col = 3:(nc-2)) %>%   # meta margins
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[2])), row = 4:(nr-3), col = 4:(nc-3)) %>%   # meta buffer in
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[4])), row = 5:(nr-4), col = 5:(nc-4))  # xylab
			if (o$panel.type == "xtab") {
				#add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[5])), row = 6:(nr-5), col = 6:(nc-5)) # panel buffer
				gt = add_to_gt(gt, grid::rectGrob(gp=grid::gpar(fill = p[5])), row = 6:(nr-5), col = 6:(nc-5)) # panel
			}
			
			gt = gt %>% 
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[6])), row = 7:(nr-6), col = 7:(nc-6)) %>%  # grid buffer
				add_to_gt(grid::rectGrob(gp=grid::gpar(fill = p[7])), row = 8:(nr-7), col = 8:(nc-7))  # grid
			
			
			for (i in 1:o$nrows) {
				for (j in 1:o$ncols) {
					gt = add_to_gt(gt, grid::rectGrob(gp=grid::gpar(fill = p[11])), row = g$rows_facet_ids[i], col = g$cols_facet_ids[j])
					if (o$panel.type == "wrap") {
						gt = add_to_gt(gt, grid::rectGrob(gp=grid::gpar(fill = p[5])), row = g$rows_panel_ids[i], col = g$cols_panel_ids[j])
					}
					
				}
			}
			
			# if (o$panel.type == "xtab") {
			# 	for (i in 1:o$nrows) {
			# 		gt = add_to_gt(gt, grid::textGrob(label = paste("Row", i), rot = ifelse(o$panel.xtab.pos[1] == "left", 90, 270)), row = g$rows_panel_row_ids[i], col = g$rows_panel_col_id)
			# 	}
			# 	for (i in 1:o$ncols) {
			# 		gt = add_to_gt(gt, grid::textGrob(label = paste("Col", i)), row = g$cols_panel_row_id, col = g$cols_panel_col_ids[i])
			# 	}
			# } else if (o$panel.type == "wrap") {
			# 	for (i in 1:o$nrows) {
			# 		for (j in 1:o$ncols) {
			# 			gt = add_to_gt(gt, grid::textGrob(label = paste("Wrap", i, j)), row = g$rows_panel_ids[i], col = g$cols_panel_ids[j])
			# 		}
			# 	}
			# }
			gt
		})
		
		
	}
	
	
	
	assign("gts", gts, envir = .TMAP_GRID)
	assign("g", g, envir = .TMAP_GRID)

}

tmapGridShape = function(bbx, facet_row, facet_col, facet_page, o) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	
	basp = (bbx[3] - bbx[1]) / (bbx[4] - bbx[2])
	
	fbbx = bb_asp(bbx, g$fasp)
	
	rc_text = frc(facet_row, facet_col)
	
	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]
	
	bgcol = if (!is.na(o$bg.color)) o$bg.color else NA
	frame.lwd = if (identical(o$frame, FALSE)) 0 else o$frame.lwd
	frame.col = if (identical(o$frame, FALSE)) NA else if (identical(o$frame, TRUE)) "gray30" else o$frame.col
	
	
	innerRect = if (is.na(bgcol) && frame.lwd == 0) {
		NULL
	} else grid::rectGrob(gp=grid::gpar(col=frame.col, lwd = frame.lwd, fill = o$bg.color), name = "outer_frame")
	
	gtmap = grid::grobTree(innerRect,
						   vp = grid::vpStack(grid::viewport(layout.pos.col = colid, layout.pos.row = rowid, name = paste0("vp_facet_", rc_text)),
						   				   grid::viewport(xscale = fbbx[c(1,3)], yscale = fbbx[c(2,4)], name = paste0("vp_map_", rc_text), clip = TRUE)), name = paste0("gt_facet_", rc_text))
	
	gts[[facet_page]] = grid::addGrob(gts[[facet_page]], gtmap, gPath = grid::gPath("gt_main"))
	
	#assign("devsize", devsize, envir = .TMAP_GRID)
	#assign("dasp", dasp, envir = .TMAP_GRID)
	assign("gts", gts, envir = .TMAP_GRID)
	#assign("bbx", bbx, envir = .TMAP_GRID)
}

tmapGridLegend = function(legs, o, facet_row = NULL, facet_col = NULL, facet_page) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	legs = list(list(brks = NA, pals = rainbow(5), levs = letters[1:5]))
	
	rows = if (facet_row[1] == -Inf) g$meta_rows[1] else if (facet_row[1] == Inf) g$meta_rows[2] else g$rows_facet_ids[facet_row]
	cols = if (facet_col[1] == -Inf) g$meta_cols[1] else if (facet_col[1] == Inf) g$meta_cols[2] else g$cols_facet_ids[facet_col]
	
	legH = vapply(legs, function(leg) {
		length(leg$levs) + 3
	}, FUN.VALUE = numeric(1))
	
	legHcs = cumsum(c(0, head(legH, -1)))
	
	#grbs = do.call(grid::gList, mapply(function(leg, startH) {
	#	grid::grid.text(c(leg$title, leg$levs), x = grid::unit(1, "lines"), y = grid::unit(c(0.5, seq(2,by=1, length.out = length(leg$levs))) + startH, "lines"), just = "left")
	#}, legs, legHcs, SIMPLIFY = FALSE))
	
	print("----")
	print(facet_row)
	print(facet_col)
	print("-")
	print(rows)
	print(cols)
	print("----")
	grbs = grid::rectGrob(gp=grid::gpar(fill="gold"))
	
	gt = add_to_gt(gt, grbs, row = rows, col = cols)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}


tmapGridWrap = function(label, facet_row, facet_col, facet_page) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	
	
	rot = g$panel_rot
	
	gt = add_to_gt(gt, grid::textGrob(label = label, rot = rot), row = g$rows_panel_ids[facet_row], col = g$cols_panel_ids[facet_col])

	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}

tmapGridXtab = function(label, facet_row = NULL, facet_col = NULL, facet_page) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	if (is.null(facet_row)) {
		rot = g$panel_col_rot
		row = g$cols_panel_row_id
		col = g$cols_panel_col_ids[facet_col]
	} else {
		rot = g$panel_row_rot
		row = g$rows_panel_row_ids[facet_row]
		col = g$rows_panel_col_id
	}
	print(rot)
	
		# 		gt = add_to_gt(gt, grid::textGrob(label = paste("Row", i), rot = ifelse(o$panel.xtab.pos[1] == "left", 90, 270)), row = g$rows_panel_row_ids[i], col = g$rows_panel_col_id)
		# 	}
		# 	for (i in 1:o$ncols) {
		# 		gt = add_to_gt(gt, grid::textGrob(label = paste("Col", i)), row = g$cols_panel_row_id, col = g$cols_panel_col_ids[i])
		# 	}
	

	gt = add_to_gt(gt, grid::textGrob(label = label, rot = rot), row = row, col = col)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}




# 
# tmapGridInit = function(o) {
# 	if (!requireNamespace("grid")) stop("grid package required but not installed yet.")
# 	#grid.newpage()
# 	
# 	devsize = dev.size()
# 	dasp = devsize[1] / devsize[2]
# 	
# 	
# 	fasp = dasp * o$nrows / o$ncols
# 	
# 	if (dasp > 1) {
# 		cw <- dasp
# 		ch <- 1
# 	} else {
# 		ch <- 1/dasp
# 		cw <- 1
# 	}
# 	
# 	gts = lapply(1L:o$npages, function(ip) {
# 		vp_tree = grid::vpStack(grid::viewport(width = grid::unit(cw, "snpc"), height = grid::unit(ch, "snpc"), name = "vp_container"),
# 								grid::viewport(layout = grid::grid.layout(nrow = o$nrows, ncol = o$ncols), name = "vp_facets"))
# 		
# 		#gt = grobTree(grid::rectGrob(gp=grid::gpar(fill="red")), vp = vp_tree)
# 		
# 		grid::grobTree(grid::grobTree(name = "gt_facets"), 
# 							grid::rectGrob(gp=grid::gpar(col="red", lwd = 4, fill = NA), name = "red_frame"),
# 							vp = vp_tree, name = "tmap_grob_tree")
# 	})
# 	
# 	# gt = grid::grobTree(grid::grobTree(name = "gt_map"), 
# 	# 					grid::rectGrob(gp=grid::gpar(col="red", lwd = 4, fill = NA), name = "red_frame"),
# 	# 					vp = vp_tree, name = "tmap_grob_tree")
# 	
# 	assign("devsize", devsize, envir = .TMAP_GRID)
# 	assign("dasp", dasp, envir = .TMAP_GRID)
# 	assign("fasp", fasp, envir = .TMAP_GRID)
# 	assign("gts", gts, envir = .TMAP_GRID)
# 	#assign("bbx", bbx, envir = .TMAP_GRID)
# 
# 	NULL
# }

frc = function(row, col) paste0(sprintf("%02d", row), "_", sprintf("%02d", col))


# tmapGridShape = function(bbx, facet_row, facet_col, facet_page) {
# 	gts = get("gts", .TMAP_GRID)
# 	fasp = get("fasp", .TMAP_GRID)
# 	
# 	sasp = (bbx[3] - bbx[1]) / (bbx[4] - bbx[2])
# 
# 	if (sasp > fasp) {
# 		width = 1
# 		height = fasp / sasp
# 	} else {
# 		height = 1
# 		width = sasp / fasp
# 	}
# 	rc_text = frc(facet_row, facet_col)
# 	
# 	gtmap = grid::grobTree(grid::rectGrob(gp=grid::gpar(col="blue", lwd = 4, fill = NA), name = paste0("blue_rect_", rc_text)),
# 						   vp = grid::vpStack(grid::viewport(layout.pos.col = facet_col, layout.pos.row = facet_row, name = paste0("vp_facet_", rc_text)),
# 						   				   grid::viewport(width = width, height = height, xscale = bbx[c(1,3)], yscale = bbx[c(2,4)], name = paste0("vp_map_", rc_text), clip = TRUE)), name = paste0("gt_facet_", rc_text))
# 	
# 	gts[[facet_page]] = grid::addGrob(gts[[facet_page]], gtmap, gPath = grid::gPath("gt_facets"))
# 	
# 	#assign("devsize", devsize, envir = .TMAP_GRID)
# 	#assign("dasp", dasp, envir = .TMAP_GRID)
# 	assign("gts", gts, envir = .TMAP_GRID)
# 	assign("bbx", bbx, envir = .TMAP_GRID)
# 	NULL
# }


select_sf = function(shpTM, dt) {
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	
	tmapIDdt = dt$tmapID__
	
	tid = intersect(tmapID, tmapIDdt)
	
	shpSel = shp[match(tid, tmapID)] #st_cast(shp[match(tid, tmapID)], "MULTIPOLYGON")
	
	
	dt = dt[match(tid, tmapIDdt), ]
	list(shp = shpSel, dt = dt)
}

tmapGridPolygons = function(shpTM, dt, bbx, facet_row, facet_col, facet_page) {
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	fill = if ("fill" %in% names(dt)) dt$fill else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	gp = grid::gpar(fill = fill, col = color) # lwd=border.lwd, lty=border.lty)
	#grb = gTree(sf::st_as_grob(shpSel, gp = gp), name = "polygons")
	
	grb = sf::st_as_grob(shp, gp = gp, name = "polygons")
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))
	
	#gt$children$gt_main$children[[gt_name]]$children = appendGlist(gt$children$gt_main$children[[gt_name]]$children, grb)
	#gt$children$gt_main$children[[gt_name]]$childrenOrder = names(gt$children$gt_main$children[[gt_name]]$children)

	gts[[facet_page]] = gt
		
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
}

appendGlist = function(glist, x) {
	glist = grid::gList(glist, x)
	names(glist)[length(glist)] = x$name
	glist
}


tmapGridSymbols = function(shpTM, dt, bbx, facet_row, facet_col, facet_page) {
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	size = if ("size" %in% names(dt)) dt$size else rep(NA, nrow(dt))
	shape = if ("shape" %in% names(dt)) dt$shape else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	coords = sf::st_coordinates(shp)
	
	gp = grid::gpar(fill = color, col = "green")
	grb = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = shape, size = grid::unit(size, "lines"), gp = gp, name = "symbols")
	
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))

	# gt$children$gt_main$children[[gt_name]]$children = appendGlist(gt$children$gt_main$children[[gt_name]]$children, grb)
	# gt$children$gt_main$children[[gt_name]]$childrenOrder = names(gt$children$gt_main$children[[gt_name]]$children)
	
	
	#gt = grid::addGrob(gt, grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
	
}


tmapGridRaster <- function(shpTM, dt, bbx, facet_row, facet_col, facet_page) {
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
		
		tid = intersect(tmapID, dt$tmapID__)
		
		color = rep(NA, length(tmapID)) #"#FFFFFF"
		
		sel = which(tmapID %in% tid)
		tid2 = tmapID[sel]
		
		color[sel] = dt$color[match(tid2, dt$tmapID__)]
		
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
		
		y_is_neg <- all(diff(st_get_dimension_values(shp, "y")) < 0)
		if (!y_is_neg) {
			m <- m[nrow(m):1L, ]
		}
		m[is.na(m)] = NA #"#0000FF"
		grb = grid::rasterGrob(m, x=cx, y=cy, width=width, height=height, interpolate = FALSE) #gpl$raster.misc$interpolate
		gt = grid::addGrob(gts[[facet_page]], grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
		gts[[facet_page]] = gt
		assign("gts", gts, envir = .TMAP_GRID)
	} else {
		shp[[1]][tmapID] = tmapID
		shpTM <- shapeTM(sf::st_geometry(sf::st_as_sf(shp)), tmapID)
		tmapGridPolygons(shpTM, dt, facet_row, facet_col, facet_page)
		#grid.shape(s, gp=grid::gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
} 




tmapGridRun = function(o) {
	gts = get("gts", .TMAP_GRID)
	lapply(gts, function(gt) {
		grid::grid.newpage()
		grid::grid.draw(gt)
	})
}
