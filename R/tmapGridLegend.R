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




tmapGridCompCorner = function(comp, o, stack, pos.h, pos.v, maxH, maxW, offsetIn.h, offsetIn.v, marginIn) {
	
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
	
	
	group.just = c(pos.h, pos.v)
	group.frame = comp[[1]]$group.frame
	
	#comp = lapply(comp, leg_standard$fun_add_leg_type)
	
	
	
	legWin[is.infinite(legWin)] =maxW
	legHin[is.infinite(legHin)] =maxH
	
	scaleW = legWin / maxW
	scaleH = legHin / maxH
	
	if (any(scaleW > 1) || any(scaleH > 1)) warning("Some legend items do not fit with the specified font size, and are therfore rescaled.", call. = FALSE)
	
	clipW = pmax(1, scaleW) 
	clipH = pmax(1, scaleH) 
	if (comp[[1]]$resize.as.group) {
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
	
	if (scaleS > 1) {
		warning("(Set of) legends is too ", ifelse(stack == "vertical", "high", "wide"), " and are therefore rescaled.", call. = FALSE)
		legWin = legWin / scaleS
		legHin = legHin / scaleS
		clipT = clipT * scaleS
	}
	
	legW = grid::unit(legWin, "inches")
	legH = grid::unit(legHin, "inches")
	
	nullsH = switch(group.just[1], left = c(0, 1), center = c(0.5, 0.5), right = c(1, 0)) 
	nullsV = switch(group.just[2], top = c(0, 1), center = c(0.5, 0.5), bottom = c(1, 0)) 
	
	
	#Ws = as.vector(rbind(legW, rep(grid::unit(marginIn, "inch"), n)))
	#Ws[length(Ws)] = grid::unit(offsetIn, "inch")
	#Ws = c(grid::unit(offsetIn, "inch"), Ws)
	
	
	if (stack == "vertical") {
		W = max(legW)
		H = sum(legH)
		
		Hs = unit_add_between(legH, grid::unit(marginIn, "inch"))
		Hs = unit_add_sides(Hs, grid::unit(offsetIn.v, "inch"))
		Hs = unit_add_sides(Hs, grid::unit(nullsV, "null"))
		
		Ws = unit_add_sides(W, grid::unit(offsetIn.h, "inch"))
		Ws = unit_add_sides(Ws, grid::unit(nullsH, "null"))
		
	} else {
		W = sum(legW)
		H = max(legH)
		
		Ws = unit_add_between(legW, grid::unit(marginIn, "inch"))
		Ws = unit_add_sides(Ws, grid::unit(offsetIn.v, "inch"))
		Ws = unit_add_sides(Ws, grid::unit(nullsH, "null"))
		
		Hs = unit_add_sides(H, grid::unit(offsetIn.h, "inch"))
		Hs = unit_add_sides(Hs, grid::unit(nullsV, "null"))
	}
	
	Hn = length(Hs)
	Wn = length(Ws)
	
	Hid = seq(3, Hn - 2, by = 2)
	Wid = seq(3, Wn - 2, by = 2)
	
	Wu = c("null", rep("inch", Wn-2), "null")
	Hu = c("null", rep("inch", Hn-2), "null")
	
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
	
	
	legGrobs = lapply(comp, tmapGridLegPlot, o = o)
	
	
	#sq = function(x) do.call(seq, as.list(unname(range(x))))
	
	groupframe = if (!is.na(comp[[1]]$frame) && group.frame) {
		#x = switch(group.just[1], "left" = W/2, "right" = grid::unit(1,"npc") - W/2, grid::unit(0.5, "npc"))
		#y = switch(group.just[2], "top" = grid::unit(1,"npc") - H/2, "bottom" = H/2, grid::unit(0.5, "npc"))
		gridCell(range(Hid), range(Wid), rndrectGrob(gp=grid::gpar(fill = comp[[1]]$bg.color, col = comp[[1]]$frame, lwd = comp[[1]]$frame.lwd), r = comp[[1]]$frame.r))
	} else NULL
	
	
	grbs = do.call(grid::gList, mapply(function(leg, lG, lH, lW, iW, iH) {
		frame = if (!is.na(leg$frame) && !group.frame) {
			rndrectGrob(gp=grid::gpar(fill = leg$bg.color, col = leg$frame, lwd = leg$frame.lwd), r = leg$frame.r)
		} else NULL
		if (stack == "vertical") {
			x = switch(leg$position$just.h, "left" = lW/2, "right" = W-lW/2, grid::unit(0.5, "npc"))
			y = grid::unit(0.5, "npc")
		} else {
			x = grid::unit(0.5, "npc")
			y = switch(leg$position$just.v, "top" = H-lH/2, "bottom" = lH/2, grid::unit(0.5, "npc"))
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

tmapGridLegend = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, pos.h, pos.v) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	n = length(comp)
	# rows and cols in main grid
	rows = if (facet_row[1] == -2) g$meta_rows[1] else if (facet_row[1] == -1) g$meta_rows[2] else g$rows_facet_ids[facet_row]
	cols = if (facet_col[1] == -2) g$meta_cols[1] else if (facet_col[1] == -1) g$meta_cols[2] else g$cols_facet_ids[facet_col]
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
	
	component.offset.h = get_option_class(o$component.offset, class = CASE.h(paste0(class, "side")), spatial_class = FALSE)
	component.offset.v = get_option_class(o$component.offset, class = CASE.v(paste0(class, "side")), spatial_class = FALSE)
	
	offsetIn.h = component.offset.h * o$lin
	offsetIn.v = component.offset.v * o$lin
	marginIn = o$component.stack.margin * o$lin
	
	marginInTot = (n - 1L) * marginIn
	offsetInTot.h  = 2 * offsetIn.h
	offsetInTot.v  = 2 * offsetIn.v
	
	totH = sum(rowsIn) - offsetInTot.v
	totW = sum(colsIn) - offsetInTot.h
	

	
	w1 = which(pos.v=="bottom" & pos.h=="left")
	w2 = which(pos.v=="top" & pos.h=="left")
	w3 = which(pos.v=="top" & pos.h=="right")
	w4 = which(pos.v=="bottom" & pos.h=="right")
	w5 = which(pos.v=="center" | pos.h=="center")
	#########
	#2     3#
	#       #
	#1     4#
	#########
	# 5 is rect category consisting of "center"s
	
	
	
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
	

	
	grbsQ = do.call(grid::gList, lapply(1:5, function(i) {
		id = get(paste0("w", i))
		if (length(id)) {
			tmapGridCompCorner(comp = comp[id], o = o, stack = stack[id[1]], pos.h = pos.h[id[1]], pos.v = pos.v[id[1]], maxH = qH[i], maxW = qW[i], offsetIn.h = offsetIn.h, offsetIn.v = offsetIn.v, marginIn = marginIn)
		}
	}))

	
	
	gt = add_to_gt(gt, grbsQ, row = rows, col = cols)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
}
