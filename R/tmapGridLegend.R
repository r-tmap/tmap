gridCell = function(rows, cols, e) {
	vp = grid::viewport(layout.pos.row = rows, layout.pos.col = cols)
	grid::grobTree(e, vp = vp)
}

legapply = function(legs, fun, ...) {
	lapply(legs, function(leg) {
		d = leg$setup$design
		f = get(paste0("leg_", d))[[fun]]
		do.call(f, c(list(leg = leg), list(...)))
	})
}


leg_portrait = list(
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
	},
	fun_height = function(leg, o) {
		s = leg$setup$settings
		
		nlev = leg$nitems
		textS = leg$setup$text.size
		titleS = if (leg$title == "") 0 else leg$setup$title.size

		padding = get_legend_option(s$item.padding, leg$type)
		paddingNA = get_legend_option(s$item.na.padding, leg$type)
		heights = get_legend_option(s$item.height, leg$type)
		
		item_heights = if (leg$type == "symbols") rep(leg$gpar$size / textS, length.out = nlev) else rep(heights, nlev)
		

		if (is.na(leg$setup$height) && (padding == -1 || any(item_heights == -1))) leg$setup$height = Inf
		if (is.infinite(leg$setup$height)) {
			if (padding != -1 && all(item_heights != -1)) padding = -1
			if (padding == -1 && any(item_heights == -1)) item_heights = rep(1, nlev)
			if (padding == -1 && s$item.na.padding != -1) s$item.na.padding == -1
			leg$setup$stretch = if (padding == -1) "padding" else "items"
		} else if (is.na(leg$setup$height)) {
			leg$setup$stretch = "none"
		} else {
			if (padding == -1 && any(item_heights == -1)) item_heights = rep(1, nlev)
			if (padding != -1 || all(item_heights != -1)) padding = -1
			leg$setup$stretch = if (padding == -1) "padding" else "items"
		}

		titleP = s$title.padding[c(3,1)] * titleS * o$lin
		titleH = titleS * o$lin
		
		marH = s$margins[c(3,1)] * textS * o$lin
		
		item_paddings = c(rep(padding, leg$nitems - leg$na.show), {if (leg$na.show) paddingNA else NULL}) 
		item_totals = item_heights + item_paddings
		
		if (leg$setup$stretch == "none") {
			hsinch = c(titleP[1], titleH, titleP[2], marH[1], item_totals * textS * o$lin, marH[2])
			
			itemHsIn = grid::unit(item_heights * textS * o$lin, units = rep("inch", nlev))
			
			hs = grid::unit(hsinch, units = rep("inch", length(hsinch)))
			Hin = sum(hsinch)
		} else {
			Hin = leg$setup$height * textS * o$lin
			
			hsunits = c(titleP[1], titleH, titleP[2], marH[1], rep(1/nlev, nlev), marH[2])
			hs = grid::unit(hsunits, units = c(rep("inch", 4), rep("null", nlev), "inch"))
			
			if (leg$setup$stretch == "padding") {
				hsinch = c(titleP[1], titleH, titleP[2], marH[1], item_heights * textS * o$lin, marH[2])
				itemHsIn = grid::unit(item_heights * textS * o$lin, units = rep("inch", nlev))
			} else {
				hsinch = c(titleP[1], titleH, titleP[2], marH[1], item_paddings * textS * o$lin, marH[2])
				itemHsIn = grid::unit(1, "npc") - grid::unit(item_paddings * textS * o$lin, units = rep("inch", nlev))
			}
		}
		
		leg$itemHsIn = itemHsIn
		leg$Hin = Hin
		leg$hs = hs
		
		po(itemHsIn)
		

		leg
	},
	fun_width = function(leg, o) {
		s = leg$setup$settings
		
		textS = leg$setup$text.size
		titleS = if (leg$title == "") 0 else leg$setup$title.size
		
		marW = s$margins[c(2,4)] * textS * o$lin
		
		item_widths = if (leg$type == "symbols") rep(leg$gpar$size / textS, length.out = leg$nitems) else rep(s$item.width[leg$type], leg$nitems)
		item_widths_max = max(item_widths)
		
		
		tW = ifelse(leg$title == "", 0, titleS * (strwidth(leg$title, units = "inch") + sum(s$title.padding[c(2,4)]) * o$lin))
		iW = textS * strwidth(leg$labels, units = "inch") + (item_widths_max + s$margin.item.text) * textS * o$lin
		
		
		colW = max(tW, iW)
		
		txtW = colW - (item_widths_max + s$margin.item.text) * textS * o$lin

		itemWsIn = grid::unit(item_widths_max * textS * o$lin, units = "inch")
		
		
		ws = grid::unit(c(marW[1], item_widths_max * textS * o$lin, s$margin.item.text * textS * o$lin, txtW, marW[2]), units = rep("inch", 5))
		
		leg$Win = sum(as.numeric(ws))
		leg$itemWsIn = itemWsIn
		leg$ws = ws
		
		po(itemWsIn)
		
		leg
	},
	fun_plot = function(leg, o) {
		s = leg$setup$settings
		
		textS = leg$setup$text.size * leg$scale
		titleS = if (leg$title == "") 0 else leg$setup$title.size * leg$scale
		
		nlev = leg$nitems
		
		vp = grid::viewport(layout = grid::grid.layout(ncol = length(leg$ws),
													   nrow = length(leg$hs), 
													   widths = leg$ws * leg$scale,
													   heights = leg$hs * leg$scale))
		
		grTitle = gridCell(2, 2:(length(leg$ws)-1), grid::textGrob(leg$title, x = grid::unit(s$title.padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = grid::gpar(cex = titleS)))
		grText = lapply(1:nlev, function(i) gridCell(i+4, 4, grid::textGrob(leg$labels[i], x = 0, just = "left", gp = grid::gpar(cex = textS))))
		
		
		if (getOption("tmap.design.mode")) {
			df = expand.grid(col = 1:length(leg$ws),
						row = 1:length(leg$hs))
			
			grDesign = lapply(1:nrow(df), function(i) gridCell(df$row[i], df$col[i], grid::rectGrob(gp=gpar(fill=NA,col="red", lwd=2))))
		} else {
			grDesign = NULL
		}
		
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
						grid::rectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar1i),
						grid::rectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar2i))
				}), 1:nlev, gpars1, gpars2, SIMPLIFY = FALSE)
				
			} else {
				gpars = gp_to_gpar(gp, sel = "all", split_to_n = nlev)#lapply(gps, gp_to_gpar)
				#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
				grItems = mapply(function(i, gpari) gridCell(i+4, 2, grid::rectGrob(width = leg$itemWsIn * leg$scale, height = leg$itemHsIn[i] * leg$scale, gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			}
			

		} else if (leg$type == "symbols") {
			gpars = gp_to_gpar(gp, split_to_n = nlev)
			
			# scale down (due to facet use)
			gpars = lapply(gpars, rescale_gp, scale = o$scale_down)
			
			grItems = mapply(function(i, gpari) gridCell(i+4, 2, grid::pointsGrob(x=0.5, y=0.5, pch = gpari$shape, size = grid::unit(gpari$size, "lines"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
		}
		
		
		g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, grDesign, list(vp = vp)))
		
		g

	}
)





leg_landscape = list(
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
	},
	fun_height = function(leg, o) {
		space = get_legend_option(setup$space, type)
		space.na = get_legend_option(setup$space.na, type)
		
		lines_excl = if (type == "symbols") {
			rep(gpar$size, length.out = nitems)
		} else {
			rep(1, nitems)
		}
		
		
		lines = if (type == "symbols") {
			if (!is.na(setup$height)) {
				# specified height in lines: recalculate space (and set space.na the same)
				space = (setup$height - sum(rep(pmax(1, gpar$size), length.out = nitems))) / nitems
				space.na = space
			} else {
				# add little margin needed for large symbols
				space = space + gpar$size / 10
				space.na = space.na + gpar$size / 10
			}
			size = rep(gpar$size, length.out = nitems)
			rep(pmax(1, c(size[1:(nitems - na.show)] + space, {if (na.show) (size[nitems] + space.na) else NULL})), length.out = nitems)
		} else {
			if (!is.na(setup$height)) {
				space = (setup$height - nitems) / nitems
				space.na = space
			}
			c(rep(1 + space, length.out = nitems - na.show), {if (na.show) (1 + space.na) else NULL})
		}
		
		# final rescale to meet specified height
		if (!is.na(setup$height)) {
			if (sum(lines) != setup$height) {
				lines = lines / sum(lines) * setup$height
			}	
		}
		
		
		
		
		tH = ifelse(leg$title == "", 0, o$lin * o$legend.title.size * 1.375)
		h = tH + o$lin * (1.65 + 0.8) * o$legend.text.size # 0.8 ???
		
		leg$Hin = h
		leg
	},
	fun_width = function(leg, o) {
		s = leg$setup$settings
		
		w = if (leg$setup$landscape.setup$item.width == -1 || leg$setup$landscape.setup$space == -1) {
			Inf
		} else {
			nlines = sum(leg$lines)
			tW = ifelse(leg$title == "", 0, o$lin * o$legend.title.size * strwidth(leg$title, units = "inch"))
			iW = o$lin * (nlines + 0.8) * o$legend.text.size * (leg$setup$landscape.setup$item.width + leg$setup$landscape.setup$space)
			
			max(c(tW, iW)) + (o$lin * o$legend.text.size * leg$setup$landscape.setup$margin * 2)
		}
			
		
		leg$Win = w
		leg
	},
	fun_plot = function(leg, o) {
		o$legend.title.size = o$legend.title.size * leg$scale
		o$legend.text.size = o$legend.text.size * leg$scale
		
		
		nlev = leg$nitems
		lH = grid::convertHeight(grid::unit(1, "lines"), "inches", valueOnly = TRUE)
		
		if (leg$title == "") o$legend.title.size = 0
		
		#nlines = leg$lines * o$legend.text.size
		#nlines_excl = leg$lines_excl * o$legend.text.size
		

		#iwidth = max(nlines_excl) * lH
		
		vp = grid::viewport(layout = grid::grid.layout(ncol = nlev + 2, nrow = 7, 
													   widths = grid::unit(
													   	c(lH * o$legend.text.size * leg$setup$landscape.setup$margin,
													   	  rep(1/nlev, nlev),
													   	  lH * o$legend.text.size * leg$setup$landscape.setup$margin, 1), units = c("inches", rep("null", nlev), "inches")),
													   heights = grid::unit(
													   	c(lH * o$legend.title.size * c(0.25, 1),
													   	  lH * o$legend.title.size * .125 + lH * o$legend.text.size * .4,
													   	  iwidth, 
													   	  lH * o$legend.text.size * c(0.25, 1), 
													   	  1), units = c(rep("inches", 6), "null"))))
		
		grTitle = gridCell(1:3, 2, grid::textGrob(leg$title, x = 0, just = "left", gp = grid::gpar(cex = o$legend.title.size)))
		grText =lapply(1:nlev, function(i) gridCell(6, 1+i, grid::textGrob(leg$labels[i], x = 0.5, just = "center", gp = grid::gpar(cex = o$legend.text.size))))
		
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
				grItems = mapply(function(i, gpar1i, gpar2i) gridCell(4, i+1, {
					grid::grobTree(
						grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpar1i),
						grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpar2i))
				}), 1:nlev, gpars1, gpars2, SIMPLIFY = FALSE)
				
			} else {
				gpars = gp_to_gpar(gp, sel = "all", split_to_n = nlev)#lapply(gps, gp_to_gpar)
				#grItems = mapply(function(i, gpari) gridCell(4, i+1, grid::rectGrob(width = grid::unit(lH * o$legend.text.size, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
				#grItems = mapply(function(i, gpari) gridCell(4, i+1, grid::rectGrob(width = grid::unit(1, "npc"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
				if (leg$setup$landscape.setup$item.width == -1) {
					grItems = mapply(function(i, gpari) gridCell(4, i+1, grid::rectGrob(width = grid::unit(1,"npc") - grid::unit(lH * o$legend.text.size * (leg$setup$landscape.setup$space), "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
				} else {
					grItems = mapply(function(i, gpari) gridCell(4, i+1, grid::rectGrob(width = grid::unit(lH * o$legend.text.size * leg$setup$landscape.setup$item.width, "inches"), height = grid::unit(lH* o$legend.text.size, "inches"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
				}
				
			}
			
		} else if (leg$type == "symbols") {
			gpars = gp_to_gpar(gp, split_to_n = nlev)
			
			# scale down (due to facet use)
			gpars = lapply(gpars, rescale_gp, scale = o$scale_down)
			
			grItems = mapply(function(i, gpari) gridCell(4, i+1, grid::pointsGrob(x=0.5, y=0.5, pch = gpari$shape, size = grid::unit(gpari$size, "lines"), gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
		}
		
		
		g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, list(vp = vp)))

	}
)








tmapGridLegend = function(legs, o, facet_row = NULL, facet_col = NULL, facet_page, legend.stack = "vertical") {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)
	
	gt = gts[[facet_page]]
	
	rows = if (facet_row[1] == -2) g$meta_rows[1] else if (facet_row[1] == -1) g$meta_rows[2] else g$rows_facet_ids[facet_row]
	cols = if (facet_col[1] == -2) g$meta_cols[1] else if (facet_col[1] == -1) g$meta_cols[2] else g$cols_facet_ids[facet_col]
	

	maxH = sum(g$rowsIn[rows])
	maxW = sum(g$colsIn[cols])
	
	
	

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
	if (o$legend.resize.as.group) {
		clipT = rep(max(clipW, clipH), length(legs))
	} else {
		clipT = pmax(clipW, clipH)
	}
	
	
	legWin = legWin / clipT
	legHin = legHin / clipT
	
	if (o$legend.justified) {
		if (legend.stack == "horizontal") {
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
	
	
	legGrobs = legapply(legs, "fun_plot", o = o)
	
	
	
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
