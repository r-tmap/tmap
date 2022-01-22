tmapGridLegHeight = function(...) {
	UseMethod("tmapGridLegHeight")
}

tmapGridLegWidth = function(...) {
	UseMethod("tmapGridLegWidth")
}

tmapGridLegPlot = function(...) {
	UseMethod("tmapGridLegPlot")
}


#' @method tmapGridLegHeight tm_legend_standard_landscape
#' @export
tmapGridLegHeight.tm_legend_standard_landscape = function(leg, o) {
	nlev = leg$nitems
	textS = leg$text.size
	titleS = if (leg$title == "") 0 else leg$title.size
	
	height = get_legend_option(leg$item.height, leg$type)
	
	item_height = if (leg$type == "symbols") max(height, leg$gpar$size / textS) else height
	itemHsIn = grid::unit(item_height * textS * o$lin, units = "inch")
	
	titleP = leg$title.padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin
	
	marH = leg$margins[c(3,1)] * textS * o$lin
	

	hsinch = c(titleP[1], titleH, titleP[2], marH[1], item_height * textS * o$lin, c(leg$margin.item.text, 1) * textS * o$lin,marH[2])
	Hin = sum(hsinch)
	
	sides = switch(leg$position$just.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hsinch, sides = sides)
	
	
	
	#hsu = grid::unit(c(hsinch, 1), units = c(rep("inch", length(hsinch)), "null"))
	

	#leg$itemHsIn = itemHsIn
	leg$Hin = Hin
	leg$hsu = hsu

	leg
	
}


#' @method tmapGridLegWidth tm_legend_standard_landscape
#' @export
tmapGridLegWidth.tm_legend_standard_landscape = function(leg, o) {
	nlev = leg$nitems
	textS = leg$text.size
	titleS = if (leg$title == "") 0 else leg$title.size
	
	space = get_legend_option(leg$item.space, leg$type)
	spaceNA = get_legend_option(leg$item.na.space, leg$type)
	width = get_legend_option(leg$item.width, leg$type)
	widthNA = get_legend_option(leg$item.na.width, leg$type)
	if (is.na(widthNA)) widthNA = width
	
	# legend width can be 3: NA (components determine width), finite number (number of text lines), or Inf (whole width)
	# for the latter two, there are 3 ways of stretching the legend: padding (space between items), items (widths of all items), or itemsNNA (widths of non-NA items)
	
	if (leg$type == "symbols") {
		item_widths = pmax(width, rep(leg$gpar$size / textS, length.out = nlev))
		leg$stretch = if (!is.na(leg$width)) "padding" else "none"	
	} else if (leg$type == "rect") {
		item_widths = rep(width, nlev)
		if (leg$na.show) item_widths[nlev] = widthNA
		leg$stretch = if (!is.na(leg$width)) "items" else "none"	
	} else if (leg$type == "gradient") {
		item_widths = rep(width, nlev)
		if (leg$na.show) item_widths[nlev] = widthNA
		leg$stretch = if (!is.na(leg$width)) "itemsNNA" else "none"	
	}
	
	
	titleP = leg$title.padding[c(2,4)] * titleS * o$lin
	titleW = titleS * strwidth(leg$title, units = "inch", family = leg$title.fontfamily, font = fontface2nr(leg$title.fontface)) * o$lin
	
	marW = leg$margins[c(2,4)] * textS * o$lin

	
	item_space = c(rep(space, nlev - 1 - leg$na.show), {if (leg$na.show) spaceNA else NULL}) 
	items_all = c(rbind(item_widths[-nlev], item_space), item_widths[nlev])
	

		
	#item_paddings = c(rep(padding, leg$nitems - leg$na.show), {if (leg$na.show) paddingNA else NULL}) 
	#item_totals = item_widths + item_paddings
	
	titleP = leg$title.padding[c(2,4)] * titleS * o$lin
	titleW = titleS * strwidth(leg$title, units = "inch", family = leg$title.fontfamily, font = fontface2nr(leg$title.fontface)) * o$lin
	
	marW = leg$margins[c(2,4)] * textS * o$lin
	
	item_space = c(rep(space, nlev - 1 - leg$na.show), {if (leg$na.show) spaceNA else NULL}) 
	items_all = c(rbind(item_widths[-nlev], item_space), item_widths[nlev])
	

	ws = c(marW[1], items_all * textS * o$lin, marW[2])
	
	
	item_ids = seq(2, by = 2, length.out = nlev)
	pad_ids = seq(3, by = 2, length.out = nlev - 1L)
	
	wsu = if (leg$stretch == "padding") {
		set_unit_with_stretch(ws, pad_ids)
	} else if (leg$stretch == "items") {
		set_unit_with_stretch(ws, item_ids)
	} else if (leg$stretch == "itemsNNA") {
		if (leg$na.show) set_unit_with_stretch(ws, head(item_ids, -1)) else set_unit_with_stretch(ws, item_ids)
	} else {
		sides = switch(leg$position$just.h, left = "second", right = "first", "both")
		set_unit_with_stretch(ws, sides = sides)
	}
	
	Win = if (leg$stretch == "none") sum(ws) else leg$width * textS * o$lin
	
	leg$Win = Win
	#leg$hs = hs
	leg$wsu = wsu
	
	leg$item_ids = item_ids + 1L
	
	leg
	
	
	
	
	
	# 
	# 	
	# nlev = leg$nitems
	# textS = leg$text.size
	# titleS = if (leg$title == "") 0 else leg$title.size
	# 
	# padding = get_legend_option(leg$item.padding, leg$type)
	# paddingNA = get_legend_option(leg$item.na.padding, leg$type)
	# widths = get_legend_option(leg$item.width, leg$type)
	# 
	# item_widths = if (leg$type == "symbols") pmax(widths, rep(leg$gpar$size / textS, length.out = nlev)) else rep(widths, nlev)
	# 
	# 
	# if (is.na(leg$width) && (padding == -1 || any(item_widths == -1))) leg$width = Inf
	# if (is.infinite(leg$width)) {
	# 	if (leg$type == "symbols") padding = -1
	# 	if (padding != -1 && all(item_widths != -1)) padding = -1
	# 	if (padding == -1 && any(item_widths == -1)) item_widths = rep(1, nlev)
	# 	if (padding == -1 && leg$item.na.padding != -1) leg$item.na.padding == -1
	# 	leg$stretch = if (padding == -1) "padding" else "items"
	# } else if (is.na(leg$width)) {
	# 	leg$stretch = "none"
	# } else {
	# 	if (leg$type == "symbols") padding = -1
	# 	if (padding == -1 && any(item_widths == -1)) item_widths = rep(1, nlev)
	# 	if (padding != -1 || all(item_widths != -1)) padding = -1
	# 	leg$stretch = if (padding == -1) "padding" else "items"
	# }
	# 
	# titleP = leg$title.padding[c(2,4)] * titleS * o$lin
	# titleW = titleS * strwidth(leg$title, units = "inch", family = leg$title.fontfamily, font = fontface2nr(leg$title.fontface)) * o$lin
	# 
	# marW = leg$margins[c(2,4)] * textS * o$lin
	# 
	# item_paddings = c(rep(padding, leg$nitems - leg$na.show), {if (leg$na.show) paddingNA else NULL}) 
	# item_totals = item_widths + item_paddings
	# 
	# #textW = textS * strwidth(leg$labels, units = "inch", family = leg$text.fontfamily, font = fontface2nr(leg$text.fontface))
	# 
	# #browser()
	# if (leg$stretch == "none") {
	# 	extraTitleWidth = max(0, titleW - sum(item_totals * textS * o$lin))
	# 	
	# 	wsinch = c(marW[1], item_totals * textS * o$lin, extraTitleWidth, marW[2])
	# 	itemWsIn = grid::unit(item_widths * textS * o$lin, units = rep("inch", nlev))
	# 	
	# 	ws = grid::unit(c(wsinch, 1), units = c(rep("inch", length(wsinch)), "null"))
	# 	Win = sum(wsinch)
	# } else {
	# 	Win = leg$width * textS * o$lin
	# 	wsunits = c(marW[1], rep(1/nlev), 0, marW[2])
	# 	ws = grid::unit(wsunits, units = c("inch", rep("null", nlev), "inch", "inch"))
	# 	if (leg$stretch == "padding") {
	# 		#wsinch = c(marW[1], item_widths * textS * o$lin, extraTitleWidth, marW[2])
	# 		itemWsIn = grid::unit(item_widths * textS * o$lin, units = rep("inch", nlev))
	# 	} else {
	# 		itemWsIn = grid::unit(1, "npc") - grid::unit(item_paddings * textS * o$lin, units = rep("inch", nlev))
	# 	}
	# 	
	# }
	# 
	# leg$itemWsIn = itemWsIn # widths of the items
	# leg$Win = Win
	# leg$ws = ws # all widths (in units, for grid.layout)
	# 
	# leg
	
}


#' @method tmapGridLegPlot tm_legend_standard_landscape
#' @export
tmapGridLegPlot.tm_legend_standard_landscape = function(leg, o) {
	textS = leg$text.size * leg$scale
	titleS = if (leg$title == "") 0 else leg$title.size * leg$scale
	
	nlev = leg$nitems
	
	wsu = leg$wsu * leg$scale
	hsu = leg$hsu * leg$scale
	
	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu), 
												   widths = wsu,
												   heights = hsu))
	
	if (leg$title.just == "left") {
		grTitle = gridCell(3, 3:(length(leg$wsu)-2), grid::textGrob(leg$title, x = grid::unit(leg$title.padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = grid::gpar(cex = titleS)))
	} else if (leg$title.just == "right") {
		grTitle = gridCell(3, 3:(length(leg$wsu)-2), grid::textGrob(leg$title, x = grid::unit(1, "npc") - grid::unit(leg$title.padding[4] * titleS * o$lin, units = "inch"), just = "right", gp = grid::gpar(cex = titleS)))
	} else {
		grTitle = gridCell(3, 3:(length(leg$wsu)-2), grid::textGrob(leg$title, x = 0.5, just = "center", gp = grid::gpar(cex = titleS)))
	}
	
	
	
	grText = mapply(function(i, id) gridCell(8, id, grid::textGrob(leg$labels[i], x = 0.5, just = "center", gp = grid::gpar(cex = textS, fontface = leg$text.fontface, fontfamily = leg$text.fontfamily))), 1L:nlev, leg$item_ids, SIMPLIFY = FALSE)
	
	ticks = get_legend_option(leg$ticks, leg$type)
	ticks.disable.na = get_legend_option(leg$ticks.disable.na, leg$type)
	if (length(ticks)) {
		tick_col = if (is.na(leg$ticks.col)) leg$gp$col else leg$ticks.col
		if (is.na(tick_col)) tick_col = "white"
		
		ticks_in_margin = sapply(ticks, function(l) all(l>=1))
		
		ni = nlev - (leg$na.show && ticks.disable.na)
		
		# tick marks in margin (specified with x coordinates between 1 and 2)
		if (any(ticks_in_margin)) {
			grTicksMargin = do.call(c, lapply(ticks[ticks_in_margin], function(y) {
				mapply(function(i, id) gridCell(8, id, grid::linesGrob(x = c(0.5, 0.5), y = 1 - (y - 1), gp = grid::gpar(col = tick_col, lwd = leg$ticks.lwd * leg$scale))), 1L:ni, leg$item_ids[1L:ni], SIMPLIFY = FALSE)
			}))
		} else {
			grTicksMargin = NULL
		}
		
		if (any(!ticks_in_margin)) {
			grTicksItem = do.call(c, lapply(ticks[!ticks_in_margin], function(y) {
				mapply(function(i, id) gridCell(6, id, grid::linesGrob(x = c(0.5, 0.5), y = 1 - y, gp = grid::gpar(col = tick_col, lwd = leg$ticks.lwd * leg$scale))), 1L:ni, leg$item_ids[1L:ni], SIMPLIFY = FALSE)
			}))
		} else {
			grTicksItem = NULL
		}
		
		grTicks = c(grTicksMargin, grTicksItem)
	} else {
		grTicks = NULL
	}
	
	
	if (getOption("tmap.design.mode")) {
		df = expand.grid(col = 1:length(leg$wsu),
						 row = 1:length(leg$hsu))
		
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
		if (o$use.gradient) {
			id1 = which(!is.na(fill_list[[1]]))[1]
			id2 = tail(which(!is.na(fill_list[[length(nlev2)]])), 1)
			
			x1 = ((id1-1) / 10) / nlev2
			x2 = (id2 / 10) / nlev2 + ((nlev2-1)/nlev2)
			w = x2 - x1
			
			if (vary_fill) {
				cols = unlist(fill_list)[id1:(10*(nlev2-1) + id2)]
				cols_alph = paste0(cols, tmap::num_to_hex(gp$fill_alpha[1] * 255))
			} else {
				alph = unlist(alpha_list)[id1:(10*(nlev2-1) + id2)]
				cols_alph = paste0(col2hex(gp$fill[1]), tmap::num_to_hex(alph * 255))
			}
			grItems1 = list(gridCell(6, leg$item_ids[lvs], grid::rectGrob(x = x1 + 0.5*w, height= w, gp=gpar(fill = grid::linearGradient(colours = rev(cols_alph)), col = NA))))
		} else {
			grItems1 = mapply(function(id, f, a) {
				w = 1 / length(f)
				xs = seq(.5*w, by = w, length.out = length(f))
				#f[!is.na(f)] = "red"
				gpi = grid::gpar(fill = f, alpha = a, col = NA)
				gridCell(6, id, grid::rectGrob(x = xs, width = w, gp = gpi))
			}, leg$item_ids[lvs], fill_list, alpha_list, SIMPLIFY = FALSE)
		}
		
		if (vary_fill) {
			x1 = (sum(is.na(fill_list[[1]])) * .1) / nlev2
			x2 = (sum(is.na(fill_list[[nlev2]])) * .1) / nlev2
		} else {
			x1 = (sum(is.na(alpha_list[[1]])) * .1) / nlev2
			x2 = (sum(is.na(alpha_list[[nlev2]])) * .1) / nlev2
		}
		
		
		grItems2 = list(gridCell(6, leg$item_ids[lvs], grid::rectGrob(x = grid::unit(x1, "npc"), just = c("left", "center"), width = grid::unit(1-(x1+x2), "npc"), gp = gpars)))
		if (leg$na.show) {
			gpars$fill = gp$fill[ifelse(length(gp$fill), nlev, 1)]
			gpars$fill_alpha = gp$fill[ifelse(length(gp$fill_alpha), nlev, 1)]
			# fill and border for NA
			grItems1 = c(grItems1, list(gridCell(6, leg$item_ids[nlev], grid::rectGrob(gp = gpars))))
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
			grItems = mapply(function(i, gpar1i, gpar2i) gridCell(6, id, {
				grid::grobTree(
					grid::rectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar1i),
					grid::rectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar2i))
			}), leg$item_ids, gpars1, gpars2, SIMPLIFY = FALSE)
			
		} else {
			gpars = gp_to_gpar(gp, sel = "all", split_to_n = nlev)#lapply(gps, gp_to_gpar)
			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			grItems = mapply(function(id, gpari) gridCell(6, id, grid::rectGrob(gp = gpari)), leg$item_ids, gpars, SIMPLIFY = FALSE)
		}
		
		
	} else if (leg$type == "symbols") {
		if (length(gp$size) == 1) gp$size = min(gp$size, min(get_legend_option(leg$item.height, "symbols"),
															 get_legend_option(leg$item.width, "symbols")) * leg$textS)
		gpars = gp_to_gpar(gp, split_to_n = nlev)
		
		# scale down (due to facet use)
		gpars = lapply(gpars, rescale_gp, scale = o$scale_down)
		
		grItems = mapply(function(id, gpari) gridCell(6, id, grid::pointsGrob(x=0.5, y=0.5, pch = gpari$shape, size = grid::unit(gpari$size, "lines"), gp = gpari)), leg$item_ids, gpars, SIMPLIFY = FALSE)
	}
	
	
	g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, grTicks, grDesign, list(vp = vp)))
	
	g
	
}
