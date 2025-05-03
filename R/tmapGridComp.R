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
		frame.lwd = if (!frame) 0 else frame.lwd * sc
		frame.color = if (!frame) NA else if (is.null(frame.color)) o$attr.color else frame.color
		frame.r = frame.r * sc
	})
	comp
}

tmapGridComp2 = function(grp, comp, o, stack, pos.h, pos.v, maxH, maxW, offsetIn.h, offsetIn.v, marginInH, marginInV, are_nums, fH, fW) {
	n = length(comp)
	legWin = vapply(comp, "[[", FUN.VALUE = numeric(1), "Win")   #  vapply(comp, leg_standard$fun_width, FUN.VALUE = numeric(1), o = o)
	legHin = vapply(comp, "[[", FUN.VALUE = numeric(1), "Hin")#vapply(comp, leg_standard$fun_height, FUN.VALUE = numeric(1), o = o)

	group.just = c(pos.h, pos.v)
	frame_combine = grp$frame_combine

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
	if (grp$resize_as_group) {
		clipT = rep(max(clipW, clipH), n)
	} else {
		clipT = pmax(clipW, clipH)
	}

	legWin = legWin / clipT
	legHin = legHin / clipT

	if (frame_combine) {
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

		Hs = unit_add_between(legHin, marginInV)
		Ws = W
	} else {
		W = sum(legWin)
		H = max(legHin)

		Ws = unit_add_between(legWin, marginInH)
		Hs = H
	}





	if (are_nums) {
		group.just = as.numeric(group.just)
		just.h = grp$position$just.h
		just.v = grp$position$just.v

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

	draw_rect = grp$frame || grp$bg
	if (draw_rect && frame_combine) {
		if (grp$frame) {
			groupframe = gridCell(range(Hid), range(Wid), rndrectGrob(gp=grid::gpar(fill = NA, col = grp$frame.color, lwd = grp$frame.lwd), r = grp$frame.r))
		} else {
			groupframe = NULL
		}

		if (grp$bg) {
			groupbg = gridCell(range(Hid), range(Wid), rndrectGrob(gp=grid::gpar(fill = grp$bg.color, alpha = grp$bg.alpha, col = NA, lwd = 0), r = grp$frame.r))
		} else {
			groupbg = NULL
		}
	} else {
		groupbg = NULL
		groupframe = NULL
	}

	equalize = grp$equalize


	if (equalize) {
		if (stack == "horizontal") {
			legFH = grid::unit(1, "npc")
			legFW = legW
		} else {
			legFH = legH
			legFW = grid::unit(1, "npc")
		}
	} else {
		legFH = legH
		legFW = legW
	}


	grbs = do.call(grid::gList, mapply(function(leg, lG, lH, lW, fH, fW, iW, iH) {
		frame = if (draw_rect && !frame_combine) {
			bg.color = if (leg$bg) leg$bg.color else NA
			frame.color = if (leg$frame) leg$frame.color else NA
			rndrectGrob(gp=grid::gpar(fill = bg.color, alpha = leg$bg.alpha, col = frame.color, lwd = leg$frame.lwd), r = leg$frame.r)
		} else NULL
		if (stack == "vertical") {
			x = switch(leg$position$align.h, "left" = lW/2, "right" = grid::unit(W, "inch") -lW/2, grid::unit(0.5, "npc"))
			y = grid::unit(0.5, "npc")
		} else {
			x = grid::unit(0.5, "npc")
			y = switch(leg$position$align.v, "top" = grid::unit(H, "inch")-lH/2, "bottom" = lH/2, grid::unit(0.5, "npc"))
		}
		gridCell(iH, iW, {
			grid::gList(
				if (equalize) {
					grid::grobTree(frame, vp = grid::viewport(width = fW, height = fH))
				} else {
					grid::grobTree(frame, vp = grid::viewport(x = x, y = y, width = fW, height = fH))
				},
				grid::grobTree(lG, vp = grid::viewport(x = x, width = lW, y = y, height = lH)))
		})
	}, comp, legGrobs, legH, legW, legFH, legFW, Wid, Hid, SIMPLIFY = FALSE))


	if (getOption("tmap.design.mode")) {
		df = expand.grid(col = 1:ncol(vp$layout),
						 row = 1:nrow(vp$layout))

		grDesign = lapply(1:nrow(df), function(i) gridCell(df$row[i], df$col[i], grid::rectGrob(gp=gpar(fill=NA,col="orange", lwd=2))))
	} else {
		grDesign = NULL
	}

	do.call(grid::grobTree, c(list(groupbg), grbs, list(groupframe), grDesign, list(vp=vp)))
}

tmapGridComp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	# get component group settings
	grp = comp[[1]][c("position",
					  "stack",
					  "frame_combine",
					  "equalize",
					  "resize_as_group",
					  "stack_margin",
					  "offset",
					  "frame" ,
					  "frame.color",
					  "frame.lwd",
					  "frame.r",
					  "bg",
					  "bg.color",
					  "bg.alpha")]

	any_legend_chart_inset = any(vapply(comp, inherits, FUN.VALUE = logical(1), c("tm_legend", "tm_chart", "tm_inset")))
	grp_called = setdiff(unique(do.call(c, lapply(comp, FUN = "[[", "called_via_comp_group"))), "group_id")

	if (!("frame" %in% grp_called)) grp$frame = any_legend_chart_inset
	if (!("bg" %in%grp_called)) grp$bg = any_legend_chart_inset


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


	# make rows and cols a range
	rows = seq(min(rows), max(rows))
	cols = seq(min(cols), max(cols))


	# multiple not needed anymore
	stack = stack[1] # check what happens when stack_auto is T, e.g. facet_stack
	pos.h = pos.h[1]
	pos.v = pos.v[1]



	rowsIn = g$rowsIn[rows]
	colsIn = g$colsIn[cols]
	if (pos.h %in% c("LEFT", "RIGHT")) {
		pos.h = tolower(pos.h)
		CASE.h = toupper
	} else CASE.h = function(x)x
	if (pos.v %in% c("TOP", "BOTTOM")) {
		pos.v = tolower(pos.v)
		CASE.v = toupper
	} else CASE.v = function(x)x

	are_nums = !(any(is.na(suppressWarnings(as.numeric(c(pos.h, pos.v))))))

	if (are_nums) {
		component.offset.h = 0
		component.offset.v = 0
	} else {
		offset = grp$offset
		if (!is.null(names(offset)) && all(c("inside", "INSIDE", "outside", "OUTSIDE") %in% names(offset))) {
			component.offset.h = get_option_class(offset, class = CASE.h(paste0(class, "side")), spatial_class = FALSE)
			component.offset.v = get_option_class(offset, class = CASE.v(paste0(class, "side")), spatial_class = FALSE)
		} else {
			offset = rep_len(offset, 2L)
			component.offset.h = offset[1]
			component.offset.v = offset[2]

		}
	}

	offsetIn.h = component.offset.h * o$lin + (o$frame.lwd * o$scale / 144) # 1 line = 1/72 inch, frame lines are centered (so /2)
	offsetIn.v = component.offset.v * o$lin + (o$frame.lwd * o$scale / 144)

	stack_margin = grp$stack_margin
	if (!is.null(names(stack_margin)) && all(c("combined", "apart") %in% names(stack_margin))) {
		stack_margin = rep_len(get_option_class(stack_margin, class = ifelse(comp[[1]]$frame_combine, "combined", "apart"), spatial_class = FALSE), 2L)
	} else {
		stack_margin = rep_len(stack_margin, 2L)
	}

	marginInH = stack_margin[1] * o$lin
	marginInV = stack_margin[2] * o$lin

	marginInTotH = (n - 1L) * marginInH
	marginInTotV = (n - 1L) * marginInV
	offsetInTot.h  = 2 * offsetIn.h
	offsetInTot.v  = 2 * offsetIn.v

	totH = sum(rowsIn) - offsetInTot.v - marginInTotH
	totW = sum(colsIn) - offsetInTot.h - marginInTotV

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

	legWin = vapply(comp, "[[", FUN.VALUE = numeric(1), "Win")   #  vapply(comp, leg_standard$fun_width, FUN.VALUE = numeric(1), o = o)
	legHin = vapply(comp, "[[", FUN.VALUE = numeric(1), "Hin")#vapply(comp, leg_standard$fun_height, FUN.VALUE = numeric(1), o = o)


	# get total value (width or height)
	getH = function(s, lH) {
		if (!length(s)) return(NULL)
		if (s[1] == "vertical") {
			sum(unit_add_between(lH, marginInV))
		} else {
			max(lH)
		}
	}
	getW = function(s, lW) {
		if (!length(s)) return(NULL)
		if (s[1] == "vertical") {
			max(lW)
		} else {
			sum(unit_add_between(lW, marginInH))
		}
	}

	grbs = tmapGridComp2(grp = grp, comp = comp, o = o, stack = stack, pos.h = pos.h, pos.v = pos.v, maxH = totH, maxW = totW, offsetIn.h = offsetIn.h, offsetIn.v = offsetIn.v, marginInH = marginInH, marginInV = marginInV, are_nums = are_nums, fH = totH, fW = totW)#sum(rowsIn), fW = sum(colsIn))

	gt = add_to_gt(gt, grbs, row = rows, col = cols)

	gts[[facet_page]] = gt

	assign("gts", gts, envir = .TMAP_GRID)

}
