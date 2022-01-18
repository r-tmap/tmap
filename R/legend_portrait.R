tmapGridLegHeight = function(...) {
	UseMethod("tmapGridLegHeight")
}

tmapGridLegWidth = function(...) {
	UseMethod("tmapGridLegWidth")
}

tmapGridLegPlot = function(...) {
	UseMethod("tmapGridLegPlot")
}

set_unit_with_stretch = function(x, ids = NULL) {
	u = rep("inch", length(x))
	if (!is.null(ids)) {
		x[ids] = 1/length(ids)
		u[ids] = "null"
		x = c(x, 0)
		u = c(u, "null")
	} else {
		x = c(x, 1)
		u = c(u, "null")
	}
	grid::unit(x, units = u)
}


#' @method tmapGridLegHeight tm_legend_portrait
#' @export
tmapGridLegHeight.tm_legend_portrait = function(leg, o) {
	nlev = leg$nitems
	textS = leg$text.size
	titleS = if (leg$title == "") 0 else leg$title.size
	
	space = get_legend_option(leg$item.space, leg$type)
	spaceNA = get_legend_option(leg$item.na.space, leg$type)
	height = get_legend_option(leg$item.height, leg$type)
	heightNA = get_legend_option(leg$item.na.height, leg$type)
	if (is.na(heightNA)) heightNA = height
	
	# legend height can be 3: NA (components determine height), finite number (number of text lines), or Inf (whole height)
	# for the latter two, there are 3 ways of stretching the legend: padding (space between items), items (heights of all items), or itemsNNA (heights of non-NA items)

	if (leg$type == "symbols") {
		item_heights = pmax(height, rep(leg$gpar$size / textS, length.out = nlev))
		leg$stretch = if (!is.na(leg$height)) "padding" else "none"	
	} else if (leg$type == "rect") {
		item_heights = rep(height, nlev)
		if (leg$na.show) item_heights[nlev] = heightNA
		leg$stretch = if (!is.na(leg$height)) "items" else "none"	
	} else if (leg$type == "gradient") {
		item_heights = rep(height, nlev)
		if (leg$na.show) item_heights[nlev] = heightNA
		leg$stretch = if (!is.na(leg$height)) "itemsNNA" else "none"	
	}
	
	titleP = leg$title.padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin
	marH = leg$margins[c(3,1)] * textS * o$lin
	
	item_space = c(rep(space, nlev - 1 - leg$na.show), {if (leg$na.show) spaceNA else NULL}) 
	items_all = c(rbind(item_heights[-nlev], item_space), item_heights[nlev])
	
	hs = c(titleP[1], titleH, titleP[2], marH[1], items_all * textS * o$lin, marH[2])
	
	item_ids = seq(5, by = 2, length.out = nlev)
	pad_ids = seq(6, by = 2, length.out = nlev - 1L)
	
	hsu = if (leg$stretch == "padding") {
		set_unit_with_stretch(hs, pad_ids)
	} else if (leg$stretch == "items") {
		set_unit_with_stretch(hs, item_ids)
	} else if (leg$stretch == "itemsNNA") {
		if (le$na.show) set_unit_with_stretch(hs, head(item_ids, -1)) else set_unit_with_stretch(hs, item_ids)
	} else {
		set_unit_with_stretch(hs)
	}
	
	Hin = if (leg$stretch == "none") sum(hs) else leg$height * textS * o$lin
	
	leg$Hin = Hin
	#leg$hs = hs
	leg$hsu = hsu
	
	leg$item_ids = item_ids
	
	
	leg
	
}

fontface2nr = function(face) {
	# needed for strwidth
	switch(face,
		   "plain" = 1,
		   "bold" = 2,
		   "italic" = 3,
		   "bold.italic" = 4,
		   "oblique" = 3,
		   "cyrillic.oblique" = 3,
		   1)
}


#' @method tmapGridLegWidth tm_legend_portrait
#' @export
tmapGridLegWidth.tm_legend_portrait = function(leg, o) {
	textS = leg$text.size
	titleS = if (leg$title == "") 0 else leg$title.size
	
	marW = leg$margins[c(2,4)] * textS * o$lin
	
	width = get_legend_option(leg$item.width, leg$type)
	
	item_widths = if (leg$type == "symbols") pmax(width, rep(leg$gpar$size / textS, length.out = leg$nitems)) else rep(width, leg$nitems)
	item_widths_max = max(item_widths)
	
	
	tW = ifelse(leg$title == "", 0, titleS * (strwidth(leg$title, units = "inch", family = leg$title.fontfamily, font = fontface2nr(leg$title.fontface)) + sum(leg$title.padding[c(2,4)]) * o$lin))
	iW = textS * strwidth(leg$labels, units = "inch", family = leg$text.fontfamily, font = fontface2nr(leg$text.fontface)) + (item_widths_max + leg$margin.item.text) * textS * o$lin

	colW = max(tW, iW)
	
	txtW = colW - (item_widths_max + leg$margin.item.text) * textS * o$lin
	
	#itemWsIn = grid::unit(item_widths_max * textS * o$lin, units = "inch")
	n = if (leg$group.just["vertical"] == "left") {
		c(0, 1)
	} else if (leg$group.just["vertical"] == "right") {
		c(1, 0)
	} else {
		c(0.5, 0.5)
	}
	
	wsu = grid::unit(c(marW[1], 
					   n[1], 
					   item_widths_max * textS * o$lin, 
					   leg$margin.item.text * textS * o$lin, 
					   txtW, 
					   n[2], 
					   marW[2]), units = c("inch", "null", rep("inch", 3), "null", "inch"))
	
	leg$Win = sum(as.numeric(wsu)[c(1, 3:5, 7)])
	leg$wsu = wsu

	po(leg$Win)
	
	leg
	
}


#' @method tmapGridLegPlot tm_legend_portrait
#' @export
tmapGridLegPlot.tm_legend_portrait = function(leg, o) {
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
		grTitle = gridCell(2, 2:(length(leg$wsu)-1), grid::textGrob(leg$title, x = grid::unit(leg$title.padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = grid::gpar(cex = titleS)))
	} else if (leg$title.just == "right") {
		grTitle = gridCell(2, 2:(length(leg$wsu)-1), grid::textGrob(leg$title, x = grid::unit(1, "npc") - grid::unit(leg$title.padding[4] * titleS * o$lin, units = "inch"), just = "right", gp = grid::gpar(cex = titleS)))
	} else {
		grTitle = gridCell(2, 2:(length(leg$wsu)-1), grid::textGrob(leg$title, x = 0.5, just = "center", gp = grid::gpar(cex = titleS)))
	}
	
	grText = mapply(function(i, id) gridCell(id, 5, grid::textGrob(leg$labels[i], x = 0, just = "left", gp = grid::gpar(cex = textS, fontface = leg$text.fontface, fontfamily = leg$text.fontfamily))), 1L:nlev, leg$item_ids, SIMPLIFY = FALSE)
	
	ticks = get_legend_option(leg$ticks, leg$type)
	ticks.disable.na = get_legend_option(leg$ticks.disable.na, leg$type)
	if (length(ticks)) {
		tick_col = if (is.na(leg$ticks.col)) leg$gp$col else leg$ticks.col
		if (is.na(tick_col)) tick_col = "white"
		
		ticks_in_margin = sapply(ticks, function(l) all(l>=1))
		
		ni = nlev - (leg$na.show && ticks.disable.na)
		
		# tick marks in margin (specified with x coordinates between 1 and 2)
		if (any(ticks_in_margin)) {
			grTicksMargin = do.call(c, lapply(ticks[ticks_in_margin], function(x) {
				mapply(function(i, id) gridCell(id, 4, grid::linesGrob(x = x - 1, y = c(0.5, 0.5), gp = grid::gpar(col = tick_col, lwd = leg$ticks.lwd * leg$scale))), 1L:ni, leg$item_ids[1L:ni], SIMPLIFY = FALSE)
			}))
		} else {
			grTicksMargin = NULL
		}
		
		if (any(!ticks_in_margin)) {
			grTicksItem = do.call(c, lapply(ticks[!ticks_in_margin], function(x) {
				mapply(function(i, id) gridCell(id, 3, grid::linesGrob(x = x, y = c(0.5, 0.5), gp = grid::gpar(col = tick_col, lwd = leg$ticks.lwd * leg$scale))), 1L:ni, leg$item_ids[1L:ni], SIMPLIFY = FALSE)
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
		grItems1 = mapply(function(id, f, a) {
			h = 1 / length(f)
			ys = seq(1-.5*h, by = -h, length.out = length(f))
			#f[!is.na(f)] = "red"
			gpi = grid::gpar(fill = f, alpha = a, col = NA)
			gridCell(id, 3, grid::rectGrob(y = ys, height = h, gp = gpi))
		}, leg$item_ids[lvs], fill_list, alpha_list, SIMPLIFY = FALSE)
		
		if (vary_fill) {
			y1 = (sum(is.na(fill_list[[1]])) * .1) / nlev2
			y2 = (sum(is.na(fill_list[[nlev2]])) * .1) / nlev2
		} else {
			y1 = (sum(is.na(alpha_list[[1]])) * .1) / nlev2
			y2 = (sum(is.na(alpha_list[[nlev2]])) * .1) / nlev2
		}
		
		grItems2 = list(gridCell(leg$item_ids[lvs], 3, grid::rectGrob(y = grid::unit(y2, "npc"), just = c("center", "bottom"), height = grid::unit(1-(y1+y2), "npc"), gp = gpars)))
		if (leg$na.show) {
			gpars$fill = gp$fill[ifelse(length(gp$fill), nlev, 1)]
			gpars$fill_alpha = gp$fill[ifelse(length(gp$fill_alpha), nlev, 1)]
			# fill and border for NA
			grItems1 = c(grItems1, list(gridCell(leg$item_ids[nlev], 3, grid::rectGrob(gp = gpars))))
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
			grItems = mapply(function(i, gpar1i, gpar2i) gridCell(id, 3, {
				grid::grobTree(
					grid::rectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar1i),
					grid::rectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar2i))
			}), leg$item_ids, gpars1, gpars2, SIMPLIFY = FALSE)
			
		} else {
			gpars = gp_to_gpar(gp, sel = "all", split_to_n = nlev)#lapply(gps, gp_to_gpar)
			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			grItems = mapply(function(id, gpari) gridCell(id, 3, grid::rectGrob(gp = gpari)), leg$item_ids, gpars, SIMPLIFY = FALSE)
		}
		
		
	} else if (leg$type == "symbols") {
		gpars = gp_to_gpar(gp, split_to_n = nlev)
		
		# scale down (due to facet use)
		gpars = lapply(gpars, rescale_gp, scale = o$scale_down)
		
		grItems = mapply(function(id, gpari) gridCell(id, 3, grid::pointsGrob(x=0.5, y=0.5, pch = gpari$shape, size = grid::unit(gpari$size, "lines"), gp = gpari)), leg$item_ids, gpars, SIMPLIFY = FALSE)
	}
	
	
	g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, grTicks, grDesign, list(vp = vp)))
	
	g
		
}

