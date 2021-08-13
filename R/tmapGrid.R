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
	
	colsIn = as.numeric(cols) * o$devsize[1]
	rowsIn = as.numeric(rows) * o$devsize[2]
	
	sum(grid::convertWidth(cols, "inches", valueOnly = TRUE))
	
	
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
		
		fasp = fasp,
		
		colsIn = colsIn,
		rowsIn = rowsIn
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

leg_standard_p_lines = function(leg) {
	if (is.na(leg$gp$shape[1])) {
		lwd = leg$gp$lwd
		if (is.na(lwd)) lwd = 1
		rep(1 + lwd / 5, length.out = leg$nitems)
		#rep(1, length.out = leg$nitems)
	} else {
		rep(pmax(1, leg$gp$size + 0.3), length.out = leg$nitems)
	}
	
	# if (leg$type == "fill") {
	# 	rep(1, length(leg$labels))
	# } else if (leg$type == "symbols") {
	# 	pmax(1, leg$sizes + 0.3)
	# }
}

tmapGridLegend = function(legs, o, facet_row = NULL, facet_col = NULL, facet_page, legend.stack = "vertical") {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	#legs = list(list(brks = NA, pals = rainbow(5), levs = letters[1:5]))
	
	rows = if (facet_row[1] == -2) g$meta_rows[1] else if (facet_row[1] == -1) g$meta_rows[2] else g$rows_facet_ids[facet_row]
	cols = if (facet_col[1] == -2) g$meta_cols[1] else if (facet_col[1] == -1) g$meta_cols[2] else g$cols_facet_ids[facet_col]
	
	maxH = sum(g$rowsIn[rows])
	maxW = sum(g$colsIn[cols])
	
	
	# legH = vapply(legs, function(leg) {
	# 	length(leg$levs) + 3
	# }, FUN.VALUE = numeric(1))
	# 
	# legHcs = cumsum(c(0, head(legH, -1)))
	
	gridCell = function(rows, cols, e) {
		vp = grid::viewport(layout.pos.row = rows, layout.pos.col = cols)
		#grid::gTree(children=grid::gList(e), vp=vp)
		#browser()
		grid::grobTree(e, vp = vp)
	}
	
	
	
	
	leg_standard = list(
		fun_height = function(leg) {
			inch = grid::convertHeight(grid::unit(1, "lines"), "inches", valueOnly = TRUE)
			
			tH = ifelse(leg$title == "", 0, inch * o$legend.title.size * 1.375)
			
			nlines = sum(leg_standard_p_lines(leg))
			
			iH = inch * (nlines + 0.8) * o$legend.text.size
			tH + iH
		},
		fun_width = function(leg) {
			inch = grid::convertHeight(grid::unit(1, "lines"), "inches", valueOnly = TRUE)
			
			tW = ifelse(leg$title == "", 0, inch * o$legend.title.size * grid::convertWidth(grid::stringWidth(leg$title), unitTo = "lines", valueOnly = TRUE))
			
			
			iW = inch * o$legend.text.size * grid::unit(grid::convertWidth(grid::stringWidth(leg$labels), unitTo = "lines", valueOnly = TRUE) + 1.65, "lines")
			max(c(tW, iW)) + (inch * o$legend.text.size * 0.75)
		},
		fun_plot = function(leg) {
			o$legend.title.size = o$legend.title.size * leg$scale
			o$legend.text.size = o$legend.text.size * leg$scale
			
			
			nlev = leg$nitems
			lH = grid::convertHeight(grid::unit(1, "lines"), "inches", valueOnly = TRUE)
			
			if (leg$title == "") o$legend.title.size = 0
			
			nlines = leg_standard_p_lines(leg) * o$legend.text.size

			
			vp = grid::viewport(layout = grid::grid.layout(ncol = 4, nrow = nlev + 4, 
														   widths = grid::unit(c(lH * o$legend.text.size * 0.4, lH * o$legend.text.size, lH * o$legend.text.size * 0.25, 1), units = c("inches", "inches", "inches", "null")),
														   heights = grid::unit(
														   	c(lH * o$legend.title.size * c(0.25, 1),
														   	  lH * o$legend.title.size * .125 + lH * o$legend.text.size * .4,
														   	  lH * nlines, 1), units = c(rep("inches", nlev + 3), "null"))))
			
			grTitle = gridCell(1:3, 2, grid::textGrob(leg$title, x = 0, just = "left", gp = grid::gpar(cex = o$legend.title.size)))
			grText = lapply(1:nlev, function(i) gridCell(i+3, 4, grid::textGrob(leg$labels[i], x = 0, just = "left", gp = grid::gpar(cex = o$legend.text.size))))
			
			gp = leg$gp
			
			legtype = if (!is.na(gp$fill[1]) && nchar(gp$fill[1]) > 50) {
				"color_cont"
			} else if (is.na(gp$shape)) {
				"color_cls"
			} else {
				"symbols"
			}
			
			
			if (legtype == "color_cont") {
				fill_list = strsplit(fill, split = "-", fixed=TRUE)
				fill_list = lapply(fill_list, function(i) {
					i[i=="NA"] <- NA
					i
				})
				grItems = mapply(function(i, f) {
					h = 1 / length(f)
					ys = seq(1-.5*h, by = -h, length.out = length(f))
					gridCell(i+3, 2, grid::rectGrob(y = ys, height = h, gp = grid::gpar(fill = f, col = NA)))
				}, 1:nlev, fill_list, SIMPLIFY = FALSE)
			} else if (legtype == "color_cls") {
				gps = split_gp(gp, n = nlev)
				
				diffAlpha = !any(is.na(c(gp$fill_alpha, gp$col_alpha))) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
				
				
				if (diffAlpha) {
					gpars1 = lapply(gps, gp_to_gpar_fill)
					gpars2 = lapply(gps, gp_to_gpar_borders)

					#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
					grItems = mapply(function(i, gpar1i, gpar2i) gridCell(i+3, 2, {
						grid::grobTree(
							grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpar1i),
							grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpar2i))
					}), 1:nlev, gpars1, gpars2, SIMPLIFY = FALSE)
					
				} else {
					gpars = lapply(gps, gp_to_gpar)
					#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
					grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
				}
				
					
				
				
			} else if (legtype == "symbols") {
				gps = split_gp(gp, n = nlev)
				gpars = lapply(gps, gp_to_gpar)
				
				grItems = mapply(function(i, gpi, gpari) gridCell(i+3, 2, grid::pointsGrob(x=0.5, y=0.5, pch = gpi$shape, size = grid::unit(gpi$size, "lines"), gp = gpari)), 1:nlev, gps, gpars, SIMPLIFY = FALSE)
			}
			
			
			g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, list(vp = vp)))
			
			#g = grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="red")))
			
			g
			
		}
	)
	
	legWin = vapply(legs, leg_standard$fun_width, FUN.VALUE = numeric(1))
	legHin = vapply(legs, leg_standard$fun_height, FUN.VALUE = numeric(1))
	
	clipW = pmax(1, legWin / maxW) 
	clipH = pmax(1, legHin / maxH) 
	if (o$legend.resize.as.group) {
		clipT = rep(max(clipW, clipH), length(legs))
	} else {
		clipT = pmax(clipW, clipH)
	}
	
	
	legWin = legWin / clipT
	legHin = legHin / clipT
	
	if (o$legend.justified) {
		if (legend.stack == "vertical") {
			legWin = rep(max(legWin), length(legs))		
		} else {
			legHin = rep(max(legHin), length(legs))
		}
	} 
	
	
	legW = grid::unit(legWin, "inches")
	legH = grid::unit(legHin, "inches")
	
	legs = mapply(function(leg, scale) {
		leg$scale = scale
		leg
	}, legs, 1/clipT, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	
	
	legGrobs = lapply(legs, leg_standard$fun_plot)
	
	
	
	if (length(legs) == 1) {
		legY = list(grid::unit(1, "npc"))
		legX = list(grid::unit(0, "npc"))
	} else {
		legY = c(list(grid::unit(1, "npc")), lapply(1:(length(legs)-1), function(i) {
			u = grid::unit(1, "npc")
			for (j in 1:i) {
				u = u - legH[[j]]
			}
			u
		}))
		legX = c(list(grid::unit(0, "npc")), lapply(1:(length(legs)-1), function(i) {
			u = legW[[1]]
			if (i>1) for (j in 2:i) {
				u = u + legW[[j]]
			}
			u
		}))
	}
	
	grbs = do.call(grid::gList, mapply(function(lG, lX, lY, lH, lW) {
		if (!is.na(o$legend.frame)) {
			frame = grid::rectGrob(gp=grid::gpar(fill = o$legend.bg.color, col = o$legend.frame, lwd = o$legend.frame.lwd))
		} else {
			frame = NULL
		}
		
		if (legend.stack == "vertical") {
			grid::grobTree(frame, lG, vp = grid::viewport(x = lW/2, width = lW, y = lY - lH/2, height = lH))
		} else {
			grid::grobTree(frame, lG, vp = grid::viewport(x = lX + lW/2, width = lW, y = legY[[1]] - lH/2, height = lH))
		}
		
		
	}, legGrobs, legX, legY, legH, legW, SIMPLIFY = FALSE))
	


	gt = add_to_gt(gt, grbs, row = rows, col = cols)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}

split_gp = function(gp, n) {
	lapply(1L:n, function(i) {
		lapply(gp, function(gpi) {
			if (length(gpi) == n) gpi[i] else gpi[1]
		})
	})
}

gp_to_gpar = function(gp) {
	grid::gpar(fill = gp$fill,
			   col = gp$col,
			   alpha = if (!is.na(gp$fill_alpha[1])) gp$fill_alpha else 1,
			   lty = if (!is.na(gp$lty[1])) gp$lty else "solid",
			   lwd = if (!is.na(gp$lwd[1])) gp$lwd else 0,
			   lineend = if (!is.na(gp$lineend[1])) gp$lineend else "round",
			   linejoin = if (!is.na(gp$linejoin[1])) gp$linejoin else "round")
}


gp_to_gpar_borders = function(gp) {
	grid::gpar(fill = NA,
			   col = gp$col,
			   alpha = gp$col_alpha,
			   lty = gp$lty,
			   lwd = gp$lwd,
			   lineend = gp$lineend,
			   linejoin = gp$linejoin)
}

gp_to_gpar_fill = function(gp) {
	grid::gpar(fill = gp$fill,
			   col = NA,
			   alpha = gp$fill_alpha,
			   lty = "blank",
			   lwd = 0,
			   lineend = gp$lineend,
			   linejoin = gp$linejoin)
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
	#print(rot)
	
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

tmapGridPolygons = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page) {
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	
	
	#fill = if ("fill" %in% names(dt)) dt$fill else rep(NA, nrow(dt))
	#color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	dtn = setdiff(names(dt), c("tmapID__", paste0("by", 1L:3L, "__")))
	
	cols = paste0("__", dtn)
	gpids = match(cols, sapply(gp, "[[", 1))
	gp[gpids] = as.list(dt[, dtn, with = FALSE])
	
	
	diffAlpha = !any(is.na(c(gp$fill_alpha, gp$col_alpha))) && !(length(gp$fill_alpha) != length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
	
	
	if (diffAlpha) {
		gp1 = gp_to_gpar_fill(gp)
		gp2 = gp_to_gpar_borders(gp)
		grb1 = sf::st_as_grob(shp, gp = gp1, name = "polygons")
		grb2 = sf::st_as_grob(shp, gp = gp2, name = "polygon_borders")
		grb = grid::grobTree(grb1, grb2)
	} else {
		gp = gp_to_gpar(gp)
		grb = sf::st_as_grob(shp, gp = gp, name = "polygons")
	}
	
	
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


tmapGridSymbols = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page) {
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	size = if ("size" %in% names(dt)) dt$size else rep(NA, nrow(dt))
	shape = if ("shape" %in% names(dt)) dt$shape else rep(NA, nrow(dt))
	#color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	gpf = sapply(gp, "[[", 1)
	gpw = which(substr(gpf, 1, 2) == "__")
	cols = names(gpw)
	
	
	cols__ = paste0("__", cols)
	gp[gpw] = as.list(dt[, cols, with = FALSE])
	
	
	gp = gp_to_gpar(gp)
	
	
	coords = sf::st_coordinates(shp)
	
	#gp = grid::gpar(fill = color, col = "gray30")
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


tmapGridRaster <- function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page) {
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
			# shortcut for else case
			color = dt$col
		} else {
			# to be improved
			tid = intersect(tmapID, dt$tmapID__)
			
			color = rep(NA, length(tmapID)) #"#FFFFFF"
			
			sel = which(tmapID %in% tid)
			tid2 = tmapID[sel]
			
			color[sel] = dt$col[match(tid2, dt$tmapID__)]
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
