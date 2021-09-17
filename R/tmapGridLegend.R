tmapGridLegend = function(legs, o, facet_row = NULL, facet_col = NULL, facet_page, legend.stack = "vertical") {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	rows = if (facet_row[1] == -2) g$meta_rows[1] else if (facet_row[1] == -1) g$meta_rows[2] else g$rows_facet_ids[facet_row]
	cols = if (facet_col[1] == -2) g$meta_cols[1] else if (facet_col[1] == -1) g$meta_cols[2] else g$cols_facet_ids[facet_col]
	
	maxH = sum(g$rowsIn[rows])
	maxW = sum(g$colsIn[cols])
	
	
	
	gridCell = function(rows, cols, e) {
		vp = grid::viewport(layout.pos.row = rows, layout.pos.col = cols)
		grid::grobTree(e, vp = vp)
	}
	
	
	
	
	leg_standard = list(
		fun_add_leg_type = function(leg) {
			gp = leg$gp
			
			leg$type = if (!is.na(gp$fill[1]) && any(nchar(gp$fill) > 50) || !is.na(gp$fill_alpha[1]) && any(nchar(gp$fill_alpha) > 50)) {
				"gradient"
			} else if (is.na(gp$shape[1])) {
				"rect"
			} else {
				"symbols"
			}
			leg
		},
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
			
			#print(nlines)
			
			vp = grid::viewport(layout = grid::grid.layout(ncol = 4, nrow = nlev + 4, 
														   widths = grid::unit(c(lH * o$legend.text.size * 0.4, lH * o$legend.text.size, lH * o$legend.text.size * 0.25, 1), units = c("inches", "inches", "inches", "null")),
														   heights = grid::unit(
														   	c(lH * o$legend.title.size * c(0.25, 1),
														   	  lH * o$legend.title.size * .125 + lH * o$legend.text.size * .4,
														   	  lH * nlines, 1), units = c(rep("inches", nlev + 3), "null"))))
			
			grTitle = gridCell(1:3, 2, grid::textGrob(leg$title, x = 0, just = "left", gp = grid::gpar(cex = o$legend.title.size)))
			grText = lapply(1:nlev, function(i) gridCell(i+3, 4, grid::textGrob(leg$labels[i], x = 0, just = "left", gp = grid::gpar(cex = o$legend.text.size))))
			
			gp = leg$gp
			
			
			
			if (leg$type == "gradient") {
				# for borders
				gpars = gp_to_gpar(gp, id = 1L, sel = "col")
				
				# for gradient fill
				nlev2 = (nlev-leg$na.show) # nlev without na
				lvs = 1:nlev2
				
				vary_fill = (length(gp$fill) > 1)
				vary_alpha = (length(gp$fill_alpha) > 1)
				
				
				# vary fill color
				if (vary_fill) {
					fill_list = strsplit(gp$fill[lvs], split = "-", fixed=TRUE)
					fill_list = lapply(fill_list, function(i) {
						i[i=="NA"] <- NA
						i
					})
				} else {
					fill_list = rep(gp$fill[1], nlev2)
				}
				
				
				# vary fill alpha
				if (vary_alpha) {
					alpha_list = strsplit(gp$fill_alpha[lvs], split = "-", fixed=TRUE)
					alpha_list = lapply(alpha_list, function(i) {
						i[i=="NA"] <- 0
						as.numeric(i)
					})
					
					if (!vary_fill) {
						fill_list = mapply(rep.int, fill_list, vapply(alpha_list, length, FUN.VALUE = integer(1)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
					}
					
				} else {
					alpha_list = rep(gp$fill_alpha[1], nlev2)
				}
				
				
				# fill
				grItems1 = mapply(function(i, f, a) {
					h = 1 / length(f)
					ys = seq(1-.5*h, by = -h, length.out = length(f))
					#f[!is.na(f)] = "red"
					gpi = grid::gpar(fill = f, alpha = a, col = NA)
					gridCell(i+3, 2, grid::rectGrob(y = ys, height = h, gp = gpi))
				}, lvs, fill_list, alpha_list, SIMPLIFY = FALSE)
				
				# border (for fill part)
				nlines_filled = nlines[lvs]
				
				if (vary_fill) {
					y1 = (sum(is.na(fill_list[[1]])) * .1) / nlev2
					y2 = (sum(is.na(fill_list[[nlev2]])) * .1) / nlev2
				} else {
					y1 = (sum(is.na(alpha_list[[1]])) * .1) / nlev2
					y2 = (sum(is.na(alpha_list[[nlev2]])) * .1) / nlev2
				}
				
				
				#h = (1 - y1 - y2) * sum(nlines_filled) * lH
				h = (1L - y1 - y2) * sum(nlines_filled) * lH
				ym = (y2 + (1L - y1 - y2)/2) * sum(nlines_filled) * lH
				
				grItems2 = list(gridCell(lvs+3, 2, grid::rectGrob(y = grid::unit(ym, "inches"), width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(h, "inches"), gp = gpars)))
				if (leg$na.show) {
					gpars$fill = gp$fill[ifelse(length(gp$fill), nlev, 1)]
					gpars$fill_alpha = gp$fill[ifelse(length(gp$fill_alpha), nlev, 1)]
					# fill and border for NA
					grItems1 = c(grItems1, list(gridCell(nlev+3, 2, grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpars))))
				}
				
				# borders
				grItems = c(grItems1, grItems2)
				
				
			} else if (leg$type == "rect") {
				#gps = split_gp(gp, n = nlev)
				
				diffAlpha = !any(is.na(c(gp$fill_alpha, gp$col_alpha))) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
				
				
				if (diffAlpha) {
					gpars1 = gp_to_gpar(gp, sel = "fill", split_to_n = nlev) #lapply(gps, gp_to_gpar_fill)
					gpars2 = gp_to_gpar(gp, sel = "col", split_to_n = nlev) #lapply(gps, gp_to_gpar_borders)
					
					#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
					grItems = mapply(function(i, gpar1i, gpar2i) gridCell(i+3, 2, {
						grid::grobTree(
							grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpar1i),
							grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpar2i))
					}), 1:nlev, gpars1, gpars2, SIMPLIFY = FALSE)
					
				} else {
					gpars = gp_to_gpar(gp, sel = "all", split_to_n = nlev)#lapply(gps, gp_to_gpar)
					#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
					grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
				}
				
				
				
				
			} else if (leg$type == "symbols") {
				gpars = gp_to_gpar(gp, split_to_n = nlev)
				
				# scale down (due to facet use)
				gpars = lapply(gpars, rescale_gp, scale = o$scale_down)
				
				grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::pointsGrob(x=0.5, y=0.5, pch = gpari$shape, size = grid::unit(gpari$size, "lines"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			}
			
			
			g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, list(vp = vp)))
			
			g
			
		}
	)
	legs = lapply(legs, leg_standard$fun_add_leg_type)
	
	legWin = vapply(legs, leg_standard$fun_width, FUN.VALUE = numeric(1))
	legHin = vapply(legs, leg_standard$fun_height, FUN.VALUE = numeric(1))
	
	scaleW = legWin / maxW
	scaleH = legHin / maxH
	
	if (any(scaleW > 1) || any(scaleH > 1)) warning("Some legend items do not fit with the specified font size, and are therfore rescaled.", call. = FALSE)
	
	clipW = pmax(1, scaleW) 
	clipH = pmax(1, scaleH) 
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
