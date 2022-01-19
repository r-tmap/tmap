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
	
	group.just = legs[[1]]$group.just

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
		if (length(legs) > 1) leg$title.just = leg$block.just[1]
		leg
	}, legs, 1/clipT, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	
	
	legGrobs = lapply(legs, tmapGridLegPlot, o = o)
	
	
	
	if (length(legs) == 1) {
		legY = list(grid::unit(1, "npc"))
		
		if (group.just[1] == "left") {
			legX = list(grid::unit(0, "npc"))
		} else if (group.just[1] == "right") {
			legX = list(grid::unit(1, "npc"))
		} else {
			legX = list(grid::unit(0.5, "npc"))
		}
		
	} else {
		sY = if (group.just[2] == "top") {
			grid::unit(1, "npc")
		} else if (group.just[2] == "bottom") {
			H
		} else {
			grid::unit(0.5, "npc") + H/2
		}
		legY = lapply(0L:(length(legs)-1), function(i) {
			if (i>0L) for (j in 1:i) sY = sY - legH[[j]]
			sY
		})
		
		
		sX = if (group.just[1] == "left") {
			grid::unit(0, "npc")
		} else if (group.just[1] == "right") {
			grid::unit(1, "npc") - W
		} else {
			grid::unit(0.5, "npc") - W/2
		}
		legX = lapply(0L:(length(legs)-1), function(i) {
			if (i>0L) for (j in 1:i) sX = sX + legW[[j]]
			sX
		})
		
	}
	
	groupframe = if (!is.na(legs[[1]]$frame) && legs[[1]]$group.frame) {
		#grid::rectGrob(gp=gpar(col = "blue", lwd= 4))
		
		if (legend.stack == "horizontal") {

			if (group.just[1] == "left") {
				grid::rectGrob(x = W/2, width = W, y = grid::unit(1,"npc") -H/2, height = H, gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd))
			} else if (group.just[1] == "right") {
				grid::rectGrob(x = grid::unit(1,"npc") - W/2, width = W, y = grid::unit(1,"npc") -H/2, height = H, gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd))
			} else {
				grid::rectGrob(x = 0.5, width = W, y = grid::unit(1,"npc") -H/2, height = H, gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd))
			}
		} else {
			if (group.just[2] == "top") {
				grid::rectGrob(x = W/2, width = W, y = grid::unit(1,"npc") -H/2, height = H, gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd))
			} else if (group.just[2] == "bottom") {
				grid::rectGrob(x = W/2, width = W, y = H/2, height = H, gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd))
			} else {
				grid::rectGrob(x = W/2, width = W, y = 0.5, height = H, gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd))
			}
		}
	} else {
		NULL
	}
	
	
	grbs = do.call(grid::gList, mapply(function(leg, lG, lX, lY, lH, lW) {
		if (!is.na(leg$frame) && !legs[[1]]$group.frame) {
			frame = grid::rectGrob(gp=grid::gpar(fill = leg$bg.color, col = leg$frame, lwd = leg$frame.lwd))
		} else {
			frame = NULL
		}

		#grid::grobTree(frame, lG, vp = grid::viewport(x = lX + lW/2, width = lW, y = lY - lH/2, height = lH))	
		po(lY, lH)
		
		if (legend.stack == "vertical") {
			x = switch(leg$block.just[1], "left" = lW/2, "right" = unit(1, "npc") - lW/2, 0.5)
			grid::grobTree(frame, lG, vp = grid::viewport(x = x, width = lW, y = lY - lH/2, height = lH))
		} else {
			y = switch(leg$block.just[2], "top" = legY[[1]] - lH/2, "bottom" = lH/2, 0.5)
			grid::grobTree(frame, lG, vp = grid::viewport(x = lX + lW/2, width = lW, y = y, height = lH))
		}
	}, legs, legGrobs, legX, legY, legH, legW, SIMPLIFY = FALSE))
	
	frame_grbs = grid::gList(groupframe, grbs)
	
	gt = add_to_gt(gt, frame_grbs, row = rows, col = cols)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}
