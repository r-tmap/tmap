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
	
	if (legs[[1]]$justified) {
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
	
	legs = mapply(function(leg, scale) {
		leg$scale = scale
		leg
	}, legs, 1/clipT, SIMPLIFY = FALSE, USE.NAMES = FALSE)
	
	
	legGrobs = lapply(legs, tmapGridLegPlot, o = o)
	
	
	
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
	
	groupframe = if (!is.na(legs[[1]]$frame) && legs[[1]]$justified) {
		if (legend.stack == "vertical") {
			W = legW[1]
			H = sum(legH)
		} else {
			W = sum(legW)
			H = legH[1]
		}
		grid::rectGrob(x = W/2, width = W, y = grid::unit(1,"npc") -H/2, height = H, gp=grid::gpar(fill = legs[[1]]$bg.color, col = legs[[1]]$frame, lwd = legs[[1]]$frame.lwd))
	} else {
		NULL
	}
	
	
	grbs = do.call(grid::gList, mapply(function(leg, lG, lX, lY, lH, lW) {
		if (!is.na(leg$frame) && !legs[[1]]$justified) {
			frame = grid::rectGrob(gp=grid::gpar(fill = leg$bg.color, col = leg$frame, lwd = leg$frame.lwd))
		} else {
			frame = NULL
		}

		if (legend.stack == "vertical") {
			grid::grobTree(frame, lG, vp = grid::viewport(x = lW/2, width = lW, y = lY - lH/2, height = lH))
		} else {
			grid::grobTree(frame, lG, vp = grid::viewport(x = lX + lW/2, width = lW, y = legY[[1]] - lH/2, height = lH))
		}
	}, legs, legGrobs, legX, legY, legH, legW, SIMPLIFY = FALSE))
	
	frame_grbs = grid::gList(groupframe, grbs)
	
	gt = add_to_gt(gt, frame_grbs, row = rows, col = cols)
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	
	
}
