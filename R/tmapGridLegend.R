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


fun_add_leg_type = function(leg) {
	within(leg, {
		type = if (!is.na(gp$fill[1]) && any(nchar(gp$fill) > 50) || !is.na(gp$fill_alpha[1]) && any(nchar(gp$fill_alpha) > 50)) {
			"gradient"
		} else if (is.na(gp$shape[1])) {
			"rect"
		} else {
			"symbols"
		}
		gpar = gp_to_gpar(gp)
		
	})
}




tmapGridLegend = function(legs, o, facet_row = NULL, facet_col = NULL, facet_page, legend.stack = "vertical") {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	rows = if (facet_row[1] == -2) g$meta_rows[1] else if (facet_row[1] == -1) g$meta_rows[2] else g$rows_facet_ids[facet_row]
	cols = if (facet_col[1] == -2) g$meta_cols[1] else if (facet_col[1] == -1) g$meta_cols[2] else g$cols_facet_ids[facet_col]
	

	maxH = sum(g$rowsIn[rows])
	maxW = sum(g$colsIn[cols])
	
	print("test1235")
	
	group.just = unlist(legs[[1]]$position[c("pos.h", "pos.v")])

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
		clipT = rep(max(clipW, clipH), length(legs))
	} else {
		clipT = pmax(clipW, clipH)
	}
	
	
	legWin = legWin / clipT
	legHin = legHin / clipT
	
	if (legs[[1]]$group.frame) {
		if (legend.stack == "vertical") {
			legWin = rep(max(legWin), length(legs))		
		} else {
			legHin = rep(max(legHin), length(legs))
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

	if (legend.stack == "vertical") {
		W = max(legW)
		H = sum(legH)
	} else {
		W = sum(legW)
		H = max(legH)
	}
	
		
	legs = mapply(function(leg, scale) {
		leg$scale = scale
		if (is.na(leg$title.just)) leg$title.just = leg$position$just.h
		leg
	}, legs, 1/clipT, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	
	
	legGrobs = lapply(legs, tmapGridLegPlot, o = o)
	
	
	
	if (length(legs) == 1) {
		legX = switch(group.just[1], "left" = grid::unit(0, "npc"), "right" = grid::unit(1,"npc") - W, grid::unit(0.5, "npc") - W/2)
		legY = switch(group.just[2], "top" = grid::unit(1, "npc"), "bottom" = grid::unit(0,"npc") + H, grid::unit(0.5, "npc") + H/2)
	} else {
		Y1 = switch(group.just[2], "top" = grid::unit(1, "npc"), "bottom" = grid::unit(0,"npc") + H, grid::unit(0.5, "npc") + H/2)
		if (legend.stack == "horizontal") {
			legY = rep(Y1, length(legs))
		} else {
			legY = lapply(0L:(length(legs)-1), function(i) {
				legY = Y1
				if (i>0L) for (j in 1:i) legY = legY - legH[[j]]
				legY
			})
		}
		X1 = switch(group.just[1], "left" = grid::unit(0, "npc"), "right" = grid::unit(1,"npc") - W, grid::unit(0.5, "npc") - W/2)
		if (legend.stack == "vertical") {
			legX = rep(X1, length(legs))
		} else {
			legX = lapply(0L:(length(legs)-1), function(i) {
				legX = X1
				if (i>0L) for (j in 1:i) legX = legX + legW[[j]]
				legX
			})
		}
	}
	

	
	groupframe = if (!is.na(legs[[1]]$frame) && legs[[1]]$group.frame) {
		x = switch(group.just[1], "left" = W/2, "right" = grid::unit(1,"npc") - W/2, grid::unit(0.5, "npc"))
		y = switch(group.just[2], "top" = grid::unit(1,"npc") - H/2, "bottom" = H/2, grid::unit(0.5, "npc"))
		grid::rectGrob(x = x, width = W, y = y, height = H, gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd))
	} else NULL
	
	po(legX, legY, legW, legH, W, H, group.just)
	grbs = do.call(grid::gList, mapply(function(leg, lG, lX, lY, lH, lW) {
		frame = if (!is.na(leg$frame) && !legs[[1]]$group.frame) {
			grid::rectGrob(gp=grid::gpar(fill = leg$bg.color, col = leg$frame, lwd = leg$frame.lwd))
		} else NULL
		x = lX + switch(leg$position$just.h, "left" = lW/2, "right" = lW/2, lW/2)
		y = lY - switch(leg$position$just.v, "top" = lH/2, "bottom" = lH/2, lH/2)
		grid::grobTree(frame, lG, vp = grid::viewport(x = x, width = lW, y = y, height = lH))
	}, legs, legGrobs, legX, legY, legH, legW, SIMPLIFY = FALSE))
	
	frame_grbs = grid::gList(groupframe, grbs)
	
	gt = add_to_gt(gt, frame_grbs, row = rows, col = cols)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}
