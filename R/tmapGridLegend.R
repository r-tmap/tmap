gridCell = function(rows, cols, e) {
	vp = grid::viewport(layout.pos.row = rows, layout.pos.col = cols)
	grid::grobTree(e, vp = vp)
}

legapply = function(legs, fun, ...) {
	lapply(legs, function(leg) {
		d = leg$design
		f = get(paste0("leg_", d))[[fun]]
		do.call(f, c(list(leg = leg), list(...)))
	})
}






tmapGridLegend = function(legs, o, facet_row = NULL, facet_col = NULL, facet_page, class, legend.stack = "vertical") {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	n = length(legs)
	# rows and cols in main grid
	rows = if (facet_row[1] == -2) g$meta_rows[1] else if (facet_row[1] == -1) g$meta_rows[2] else g$rows_facet_ids[facet_row]
	cols = if (facet_col[1] == -2) g$meta_cols[1] else if (facet_col[1] == -1) g$meta_cols[2] else g$cols_facet_ids[facet_col]
	rowsIn = g$rowsIn[rows]
	colsIn = g$colsIn[cols]
	
	component.offset = get_option_class(o$component.offset, class = paste0(class, "side"), spatial_class = FALSE)
	
	offsetIn = component.offset * o$lin
	marginIn = o$component.stack.margin
	
	marginInTot = (n - 1L) * marginIn
	offsetInTot  = 2 * offsetIn

	if (legend.stack == "vertical") {
		maxH = sum(rowsIn) - offsetInTot - marginInTot 
		maxW = sum(colsIn) - offsetInTot
	} else {
		maxW = sum(colsIn) - offsetInTot - marginInTot 
		maxH = sum(rowsIn) - offsetInTot
	}

	group.just = unlist(legs[[1]]$position[c("pos.h", "pos.v")])
	group.frame = legs[[1]]$group.frame
	
	#legs = lapply(legs, leg_standard$fun_add_leg_type)
	
	legWin = vapply(legs, "[[", FUN.VALUE = numeric(1), "Win")   #  vapply(legs, leg_standard$fun_width, FUN.VALUE = numeric(1), o = o)
	legHin = vapply(legs, "[[", FUN.VALUE = numeric(1), "Hin")#vapply(legs, leg_standard$fun_height, FUN.VALUE = numeric(1), o = o)
	
	
	legWin[is.infinite(legWin)] =maxW
	legHin[is.infinite(legHin)] =maxH

	scaleW = legWin / maxW
	scaleH = legHin / maxH
	
	if (any(scaleW > 1) || any(scaleH > 1)) warning("Some legend items do not fit with the specified font size, and are therfore rescaled.", call. = FALSE)
	
	clipW = pmax(1, scaleW) 
	clipH = pmax(1, scaleH) 
	if (legs[[1]]$resize.as.group) {
		clipT = rep(max(clipW, clipH), n)
	} else {
		clipT = pmax(clipW, clipH)
	}

	legWin = legWin / clipT
	legHin = legHin / clipT
	
	if (group.frame) {
		if (legend.stack == "vertical") {
			legWin = rep(max(legWin), n)		
		} else {
			legHin = rep(max(legHin), n)
		}
	} 
	
	# rescale due to stacking
	if (legend.stack == "vertical") {
		scaleS = sum(legHin) / maxH
	} else {
		scaleS = sum(legWin) / maxW
	}
	
	if (scaleS > 1) {
		warning("(Set of) legends is too ", ifelse(legend.stack == "vertical", "high", "wide"), " and are therefore rescaled.", call. = FALSE)
		legWin = legWin / scaleS
		legHin = legHin / scaleS
		clipT = clipT * scaleS
	}

	legW = grid::unit(legWin, "inches")
	legH = grid::unit(legHin, "inches")

	nullsH = switch(group.just[1], left = c(0, 1), center = c(0.5, 0.5), right = c(1, 0)) 
	nullsV = switch(group.just[2], top = c(0, 1), center = c(0.5, 0.5), bottom = c(1, 0)) 
	
	
	Ws = as.vector(rbind(legW, rep(grid::unit(marginIn, "inch"), n)))
	Ws[length(Ws)] = grid::unit(offsetIn, "inch")
	Ws = c(grid::unit(offsetIn, "inch"), Ws)
	
		
	if (legend.stack == "vertical") {
		W = max(legW)
		H = sum(legH)
		
		Hs = unit_add_between(legH, grid::unit(marginIn, "inch"))
		Hs = unit_add_sides(Hs, grid::unit(offsetIn, "inch"))
		Hs = unit_add_sides(Hs, grid::unit(nullsV, "null"))
		
		Ws = unit_add_sides(W, grid::unit(offsetIn, "inch"))
		Ws = unit_add_sides(Ws, grid::unit(nullsH, "null"))
		
	} else {
		W = sum(legW)
		H = max(legH)
		
		Ws = unit_add_between(legW, grid::unit(marginIn, "inch"))
		Ws = unit_add_sides(Ws, grid::unit(offsetIn, "inch"))
		Ws = unit_add_sides(Ws, grid::unit(nullsH, "null"))
		
		Hs = unit_add_sides(H, grid::unit(offsetIn, "inch"))
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
	
	po(vp$layout$widths)
	po(vp$layout$heights)
	
	legs = mapply(function(leg, scale, W, H) {
		leg$scale = scale
		if (is.na(leg$title.just)) leg$title.just = leg$position$just.h
		
		leg$wsu = distr_space_over_nulls(u = leg$wsu * scale, tot = W, stretchID = leg$flexCol)
		leg$hsu = distr_space_over_nulls(u = leg$hsu * scale, tot = H, stretchID = leg$flexRow)
		leg
	}, legs, 1/clipT, legW, legH, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	
	
	legGrobs = lapply(legs, tmapGridLegPlot, o = o)
	
	
	
	if (n == 1) {
		legX = switch(group.just[1], "left" = grid::unit(0, "npc"), "right" = grid::unit(1,"npc") - W, grid::unit(0.5, "npc") - W/2)
		legY = switch(group.just[2], "top" = grid::unit(1, "npc"), "bottom" = grid::unit(0,"npc") + H, grid::unit(0.5, "npc") + H/2)
	} else {
		Y1 = switch(group.just[2], "top" = grid::unit(1, "npc"), "bottom" = grid::unit(0,"npc") + H, grid::unit(0.5, "npc") + H/2)
		if (legend.stack == "horizontal") {
			legY = rep(Y1, n)
		} else {
			legY = lapply(0L:(n-1), function(i) {
				legY = Y1
				if (i>0L) for (j in 1:i) legY = legY - legH[[j]]
				legY
			})
		}
		X1 = switch(group.just[1], "left" = grid::unit(0, "npc"), "right" = grid::unit(1,"npc") - W, grid::unit(0.5, "npc") - W/2)
		if (legend.stack == "vertical") {
			legX = rep(X1, n)
		} else {
			legX = lapply(0L:(n-1), function(i) {
				legX = X1
				if (i>0L) for (j in 1:i) legX = legX + legW[[j]]
				legX
			})
		}
	}
	
	#sq = function(x) do.call(seq, as.list(unname(range(x))))
	
	groupframe = if (!is.na(legs[[1]]$frame) && group.frame) {
		#x = switch(group.just[1], "left" = W/2, "right" = grid::unit(1,"npc") - W/2, grid::unit(0.5, "npc"))
		#y = switch(group.just[2], "top" = grid::unit(1,"npc") - H/2, "bottom" = H/2, grid::unit(0.5, "npc"))
		gridCell(range(Hid), range(Wid), rndrectGrob(gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd), r = legs[[1]]$frame.r))
	} else NULL
	
	grbs = do.call(grid::gList, mapply(function(leg, lG, lX, lY, lH, lW) {
		frame = if (!is.na(leg$frame) && !group.frame) {
			rndrectGrob(gp=grid::gpar(fill = leg$bg.color, col = leg$frame, lwd = leg$frame.lwd), r = leg$frame.r)
		} else NULL
		x = lX + switch(leg$position$just.h, "left" = lW/2, "right" = lW/2, lW/2)
		y = lY - switch(leg$position$just.v, "top" = lH/2, "bottom" = lH/2, lH/2)
		grid::grobTree(frame, lG, vp = grid::viewport(x = x, width = lW, y = y, height = lH))
	}, legs, legGrobs, legX, legY, legH, legW, SIMPLIFY = FALSE))
	
	grbs = do.call(grid::gList, mapply(function(leg, lG, lH, lW, iW, iH) {
		frame = if (!is.na(leg$frame) && !group.frame) {
			rndrectGrob(gp=grid::gpar(fill = leg$bg.color, col = leg$frame, lwd = leg$frame.lwd), r = leg$frame.r)
		} else NULL
		if (legend.stack == "vertical") {
			x = switch(leg$position$just.h, "left" = lW/2, "right" = W-lW/2, grid::unit(0.5, "npc"))
			y = grid::unit(0.5, "npc")
		} else {
			x = grid::unit(0.5, "npc")
			y = switch(leg$position$just.v, "top" = H-lH/2, "bottom" = lH/2, grid::unit(0.5, "npc"))
		}
		gridCell(iH, iW, grid::grobTree(frame, lG, vp = grid::viewport(x = x, width = lW, y = y, height = lH)))
	}, legs, legGrobs, legH, legW, Wid, Hid, SIMPLIFY = FALSE))
	
	
	if (getOption("tmap.design.mode")) {
		df = expand.grid(col = 1:ncol(vp$layout),
						 row = 1:nrow(vp$layout))
		
		grDesign = lapply(1:nrow(df), function(i) gridCell(df$row[i], df$col[i], grid::rectGrob(gp=gpar(fill=NA,col="orange", lwd=2))))
	} else {
		grDesign = NULL
	}
	
	all_grbs = do.call(grid::grobTree, c(list(groupframe), grbs, grDesign, list(vp=vp)))
	
	
	gt = add_to_gt(gt, all_grbs, row = rows, col = cols)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}
