gridCell = function(rows, cols, e) {
	vp = grid::viewport(layout.pos.row = rows, layout.pos.col = cols)
	grid::grobTree(e, vp = vp)
}

legapply = function(cdt, fun, ...) {
	lapply(cdt, function(leg) {
		d = leg$design
		f = get(paste0("leg_", d))[[fun]]
		do.call(f, c(list(leg = leg), list(...)))
	})
}



process_comp_box = function(comp, sc, o) {
	comp = within(comp, {
		frame.lwd = if (identical(frame, FALSE)) 0 else frame.lwd * sc
		frame.col = if (identical(frame, FALSE)) NA else if (identical(frame, TRUE)) o$attr.color else frame
		frame.r = frame.r * sc
	})
	comp
}

tmapGridCompCorner = function(comp, o, stack, pos.h, pos.v, maxH, maxW, offsetIn.h, offsetIn.v, marginIn, are_nums, fH, fW) {
	#return(grid::rectGrob(gp=gpar(fill = "gold")))


	n = length(comp)
	# if (stack == "vertical") {
	# 	maxH = totH - marginInTot
	# 	maxW = totW
	# } else {
	# 	maxW = totW - marginInTot
	# 	maxH = totH
	# }
	legWin = vapply(comp, "[[", FUN.VALUE = numeric(1), "Win")   #  vapply(comp, leg_standard$fun_width, FUN.VALUE = numeric(1), o = o)
	legHin = vapply(comp, "[[", FUN.VALUE = numeric(1), "Hin")#vapply(comp, leg_standard$fun_height, FUN.VALUE = numeric(1), o = o)

	#legWin = 10

	group.just = c(pos.h, pos.v)
	group.frame = comp[[1]]$group.frame

	legWin[is.infinite(legWin)] = maxW
	legHin[is.infinite(legHin)] = maxH

	scaleW = legWin / maxW
	scaleH = legHin / maxH

	# because of legend frames (for which margins are added in tmapGridComp), the scale may be a bit above 1, even though automatic layout is applied and there is enough space

	if (o$component.autoscale) {
		if (any(scaleW > 1.1) || any(scaleH > 1.1)) message_comp_scale()
	} else {
		scaleW[scaleW > 1.1] = 1
		scaleH[scaleH > 1.1] = 1
	}


	clipW = pmax(1, scaleW)
	clipH = pmax(1, scaleH)
	if (comp[[1]]$resize_as_group) {
		clipT = rep(max(clipW, clipH), n)
	} else {
		clipT = pmax(clipW, clipH)
	}

	legWin = legWin / clipT
	legHin = legHin / clipT

	if (group.frame) {
		if (stack == "vertical") {
			legWin = rep(max(legWin), n)
		} else {
			legHin = rep(max(legHin), n)
		}
	}

	# rescale due to stacking
	if (stack == "vertical") {
		scaleS = sum(legHin) / maxH
	} else {
		scaleS = sum(legWin) / maxW
	}

	if (o$component.autoscale) {
		if (scaleS > 1.01) {
			if (scaleS > 1.05) {
				message_comp_high_wide(stack)

			}
			legWin = legWin / scaleS
			legHin = legHin / scaleS
			clipT = clipT * scaleS
		}
	}

	legW = grid::unit(legWin, "inches")
	legH = grid::unit(legHin, "inches")

	if (stack == "vertical") {
		W = max(legWin)
		H = sum(legHin)

		Hs = unit_add_between(legHin, marginIn)
		Ws = W
	} else {
		W = sum(legWin)
		H = max(legHin)

		Ws = unit_add_between(legWin, marginIn)
		Hs = H
	}





	if (are_nums) {
		group.just = as.numeric(group.just)
		just.h = comp[[1]]$position$just.h
		just.v = comp[[1]]$position$just.v

		# add dummy offsets
		Hs = unit_add_sides(Hs, 0)
		Ws = unit_add_sides(Ws, 0)

		ancher_x = maxW * group.just[1]
		if (just.h == "left") {
			Ws = unit_add_sides(Ws, c(ancher_x, maxW - ancher_x- sum(Ws)))
		} else if (just.h == "right") {
			Ws = unit_add_sides(Ws, c(ancher_x - sum(Ws), maxW - ancher_x))
		} else {
			Ws = unit_add_sides(Ws, c(ancher_x - (sum(Ws) / 2), maxW - ancher_x- (sum(Ws) / 2)))
		}

		ancher_y = maxH * (1 - group.just[2])
		if (just.v == "top") {
			Hs = unit_add_sides(Hs, c(ancher_y, maxH - ancher_y - sum(Hs)))
		} else if (just.v == "bottom") {
			Hs = unit_add_sides(Hs, c(ancher_y - sum(Hs), maxH - ancher_y))
		} else {
			Hs = unit_add_sides(Hs, c(ancher_y - (sum(Hs) / 2), maxH - ancher_y - (sum(Hs) / 2)))
		}

	} else {
		nullsH = switch(group.just[1], left = c(0, 1), center = c(0.5, 0.5), right = c(1, 0))
		nullsV = switch(group.just[2], top = c(0, 1), center = c(0.5, 0.5), bottom = c(1, 0))

		Hs = unit_add_sides(Hs, offsetIn.v)
		Hs = unit_add_sides(Hs, nullsV)

		Ws = unit_add_sides(Ws, offsetIn.h)
		Ws = unit_add_sides(Ws, nullsH)

	}

	Hn = length(Hs)
	Wn = length(Ws)

	Hid = seq(3, Hn - 2, by = 2)
	Wid = seq(3, Wn - 2, by = 2)

	if (are_nums) {
		Wu = rep("inch", Wn)
		Hu = rep("inch", Hn)
	} else {
		Wu = c("null", rep("inch", Wn-2), "null")
		Hu = c("null", rep("inch", Hn-2), "null")
	}

	# po(Ws, Hs)
	# Ws = distr_space_over_nulls(Ws, maxW)
	# Hs = distr_space_over_nulls(Hs, maxH)
	# po(Ws, Hs)

	vp = grid::viewport(layout = grid::grid.layout(ncol = Wn,
												   nrow = Hn,
												   widths = grid::unit(Ws, Wu),
												   heights = grid::unit(Hs, Hu)))


	comp = mapply(function(leg, scale, W, H) {
		leg$scale = scale

		if ("flexCol" %in% names(leg)) leg$wsu = distr_space_over_nulls(u = leg$wsu * scale, tot = W, stretchID = leg$flexCol)
		if ("flexRow" %in% names(leg)) leg$hsu = distr_space_over_nulls(u = leg$hsu * scale, tot = H, stretchID = leg$flexRow)
		leg
	}, comp, 1/clipT, legW, legH, SIMPLIFY = FALSE, USE.NAMES = FALSE)


	legGrobs = lapply(comp, tmapGridLegPlot, o = o, fH = fH, fW = fW)


	#sq = function(x) do.call(seq, as.list(unname(range(x))))
	sc = min(1/clipT) * o$scale

	comp = lapply(comp, process_comp_box, sc = sc, o = o)

	groupframe = if ((comp[[1]]$frame.lwd!=0) && group.frame) {
		gridCell(range(Hid), range(Wid), rndrectGrob(gp=grid::gpar(fill = comp[[1]]$bg.color, alpha = comp[[1]]$bg.alpha, col = comp[[1]]$frame, lwd = comp[[1]]$frame.lwd), r = comp[[1]]$frame.r))
	} else NULL


	grbs = do.call(grid::gList, mapply(function(leg, lG, lH, lW, iW, iH) {
		frame = if (!is.na(leg$frame) && !group.frame) {
			rndrectGrob(gp=grid::gpar(fill = leg$bg.color, alpha = leg$bg.alpha, col = leg$frame, lwd = leg$frame.lwd), r = leg$frame.r)
		} else NULL
		if (stack == "vertical") {
			x = switch(leg$position$align.h, "left" = lW/2, "right" = grid::unit(W, "inch") -lW/2, grid::unit(0.5, "npc"))
			y = grid::unit(0.5, "npc")
		} else {
			x = grid::unit(0.5, "npc")
			y = switch(leg$position$align.v, "top" = grid::unit(H, "inch")-lH/2, "bottom" = lH/2, grid::unit(0.5, "npc"))
		}
		gridCell(iH, iW, grid::grobTree(frame, lG, vp = grid::viewport(x = x, width = lW, y = y, height = lH)))
	}, comp, legGrobs, legH, legW, Wid, Hid, SIMPLIFY = FALSE))


	if (getOption("tmap.design.mode")) {
		df = expand.grid(col = 1:ncol(vp$layout),
						 row = 1:nrow(vp$layout))

		grDesign = lapply(1:nrow(df), function(i) gridCell(df$row[i], df$col[i], grid::rectGrob(gp=gpar(fill=NA,col="orange", lwd=2))))
	} else {
		grDesign = NULL
	}

	do.call(grid::grobTree, c(list(groupframe), grbs, grDesign, list(vp=vp)))
}

tmapGridComp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)

	gt = gts[[facet_page]]
	n = length(comp)
	# rows and cols in main grid
	rows = if (facet_row[1] == -2) g$meta_rows[1] else if (facet_row[1] == -1) g$meta_rows[2] else {
		panel_rows = if (class == "in") {
			NULL
		} else if (o$panel.type == "xtab") {
		    g$rows_panel_row_ids[facet_row]
		} else if (o$panel.type == "wrap") {
			g$rows_panel_ids[facet_row]
		} else {
			NULL
		}
		c(g$rows_facet_ids[facet_row], panel_rows)
	}
	cols = if (facet_col[1] == -2) g$meta_cols[1] else if (facet_col[1] == -1) g$meta_cols[2] else {
		panel_cols = if (class == "in") {
			NULL
		} else if (o$panel.type == "xtab") {
			g$cols_panel_col_ids[facet_col]
		} else if (o$panel.type == "wrap") {
			g$cols_panel_ids[facet_col]
		} else {
			NULL
		}
		c(g$cols_facet_ids[facet_col], panel_cols)
	}
#po(rows,cols)
# 	# add the panel viewports to the rows and cols (scrape them, next step we get the whole ranges)
# 	if (o$panel.type == "xtab") {
# 		cols = c(cols, g$cols_panel_col_ids, g$rows_panel_col_id)
# 		rows = c(rows, g$cols_panel_row_id, g$rows_panel_row_ids)
# 	} else if (o$panel.type == "wrap") {
# 		cols = c(cols, g$cols_panel_ids)
# 		rows = c(rows, g$rows_panel_ids)
# 	}
# po(rows,cols)

	# make rows and cols a range
	rows = seq(min(rows), max(rows))
	cols = seq(min(cols), max(cols))


	rowsIn = g$rowsIn[rows]
	colsIn = g$colsIn[cols]
	if (any(pos.h %in% c("LEFT", "RIGHT"))) {
		pos.h = tolower(pos.h)
		CASE.h = toupper
	} else CASE.h = function(x)x
	if (any(pos.v %in% c("TOP", "BOTTOM"))) {
		pos.v = tolower(pos.v)
		CASE.v = toupper
	} else CASE.v = function(x)x

	are_nums = !(any(is.na(suppressWarnings(as.numeric(c(pos.h, pos.v))))))

	if (are_nums) {
		component.offset.h = 0
		component.offset.v = 0
	} else {
		component.offset.h = get_option_class(o$component.offset, class = CASE.h(paste0(class, "side")), spatial_class = FALSE)
		component.offset.v = get_option_class(o$component.offset, class = CASE.v(paste0(class, "side")), spatial_class = FALSE)
	}

	offsetIn.h = component.offset.h * o$lin + (o$frame.lwd * o$scale / 144) # 1 line = 1/72 inch, frame lines are centered (so /2)
	offsetIn.v = component.offset.v * o$lin + (o$frame.lwd * o$scale / 144)
	marginIn = o$component.stack_margin * o$lin

	marginInTot = (n - 1L) * marginIn
	offsetInTot.h  = 2 * offsetIn.h
	offsetInTot.v  = 2 * offsetIn.v

	totH = sum(rowsIn) - offsetInTot.v
	totW = sum(colsIn) - offsetInTot.h
	w1 = which(pos.v=="bottom" & pos.h=="left")
	w2 = which(pos.v=="top" & pos.h=="left")
	w3 = which(pos.v=="top" & pos.h=="right")
	w4 = which(pos.v=="bottom" & pos.h=="right")
	w5 = setdiff(seq_len(n), c(w1, w2, w3, w4))
	#########
	#2     3#
	#       #
	#1     4#
	#########
	# 5 is rect category consisting of "center"s or numbers

	## update scale_bar width: identified by the WnativeID = 3 item (null for other components)
	comp = mapply(function(cmp, bb) {
		bbw = bb[3] - bb[1]

		# TRUE if component is scalebar
		if (!is.null(cmp$WnativeID)) {
			# find out whether the bounding of borrowed from the map (yes if scale bar is drawn in another facet)
			bbox_nb = attr(bbox, "borrow")
			if (is.null(bbox_nb)) {
				bb_facet = sum(colsIn)
			} else {
				bb_facet = sum(g$colsIn[g$cols_facet_ids[bbox_nb$col]])
			}

			oldIn = as.numeric(cmp$wsu[cmp$WnativeID])

			if (is.null(cmp$WnativeRange) || ("width" %in% cmp$call)) {
				# in case breaks not defined: allow the width to be maximal fraction of facet width
				#newIn = min(totW, bb_facet * cmp$width)
				newIn = min(totW, cmp$Win)
			} else {
				# in case breaks are defined: compute width
				Wextra = (text_width_inch(paste0("   ", cmp$units$unit)) + text_width_inch(paste0(tail(cmp$breaks, 1), cmp$breaks[1])) / 2 ) * cmp$text.size

				# bbw are number of (CRS) units of map width, totalCoords translated to scalebar units
				totalCoords =  bbw * cmp$units$to

				Wcomp = bb_facet * (cmp$WnativeRange / totalCoords) + Wextra

				newIn = min(totW, Wcomp)
			}

			margins =  sum(as.numeric(cmp$wsu[c(2,4)]))

			newIn_without_margins = newIn - margins

			cmp$wsu[cmp$WnativeID] = unit(newIn_without_margins, "inch")
			cmp$Win = newIn_without_margins + margins

			# get cpi: coordinates per inch

			cmp$cpi = unname(bbw / bb_facet)
		}
		cmp
	}, comp, bbox, SIMPLIFY = FALSE)


	qH = rep(totH, 5)
	qW = rep(totW, 5)

	legWin = vapply(comp, "[[", FUN.VALUE = numeric(1), "Win")   #  vapply(comp, leg_standard$fun_width, FUN.VALUE = numeric(1), o = o)
	legHin = vapply(comp, "[[", FUN.VALUE = numeric(1), "Hin")#vapply(comp, leg_standard$fun_height, FUN.VALUE = numeric(1), o = o)


	# get total value (width or height)
	getH = function(s, lH) {
		if (!length(s)) return(NULL)
		if (s[1] == "vertical") {
			sum(unit_add_between(lH, marginIn))
		} else {
			max(lH)
		}
	}
	getW = function(s, lW) {
		if (!length(s)) return(NULL)
		if (s[1] == "vertical") {
			max(lW)
		} else {
			sum(unit_add_between(lW, marginIn))
		}
	}


	align_values = function(id, dir) {
		w1 = get(paste0("w", id[1]))
		w2 = get(paste0("w", id[2]))
		s1 = stack[w1[1]]
		s2 = stack[w2[1]]
		x1 = switch(dir, v = legHin[w1], h = legWin[w1])
		x2 = switch(dir, v = legHin[w2], h = legWin[w2])
		t1 = if (dir == "v") {
			getH(s1, x1)
		} else {
			getW(s1, x1)
		}
		t2 = if (dir == "v") {
			getH(s2, x2)
		} else {
			getW(s2, x2)
		}
		tot = switch(dir, v = totH, h = totW)
		if (is.infinite(t1) || is.infinite(t2)) {
			c(tot/2,tot/2)
		} else {
			scale = sum(t1, t2) / tot
			if (scale > 1) {
				t1 = t1 / scale
				t2 = t2 / scale
			}
			c(t1, t2)
		}
	}

	if (length(w1) && length(w2)) qH[1:2] = align_values(1:2, "v") # left edge
	if (length(w3) && length(w4)) qH[3:4] = align_values(3:4, "v") # right edge
	if (length(w2) && length(w3)) qW[2:3] = align_values(2:3, "h") # top edge
	if (length(w1) && length(w4)) qW[c(1,4)] = align_values(c(1,4), "h") # bottom edge


	grbs = do.call(grid::gList, lapply(1:5, function(i) {
		id = get(paste0("w", i))
		if (length(id)) {
			if (!all(stack_auto[id])) {
				# get first specified 'stack' argument
				stck = stack[id][which(!stack_auto[id])[1]]
			} else {
				# get first stack argument
				stck = stack[id[1]]
			}
			tmapGridCompCorner(comp = comp[id], o = o, stack = stck, pos.h = pos.h[id[1]], pos.v = pos.v[id[1]], maxH = qH[i], maxW = qW[i], offsetIn.h = offsetIn.h, offsetIn.v = offsetIn.v, marginIn = marginIn, are_nums = are_nums, fH = totH, fW = totW)#sum(rowsIn), fW = sum(colsIn))
		}
	}))

	#grbs = grid::rectGrob(gp=gpar(fill = "gold"))

	gt = add_to_gt(gt, grbs, row = rows, col = cols)

	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)

}
