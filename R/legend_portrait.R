tmapGridCompHeight = function(...) {
	UseMethod("tmapGridCompHeight")
}

tmapGridCompWidth = function(...) {
	UseMethod("tmapGridCompWidth")
}

tmapGridLegPlot = function(...) {
	UseMethod("tmapGridLegPlot")
}


#' @method tmapGridCompPrepare tm_legend_standard_portrait
#' @export
tmapGridCompPrepare.tm_legend_standard_portrait = function(comp, o) {
	within(comp, {
		type = if (!is.na(gp$fill[1]) && any(nchar(gp$fill) > 50) || !is.na(gp$fill_alpha[1]) && any(nchar(gp$fill_alpha) > 50)) {
			"gradient"
		} else if (!is.na(gp$shape[1])) {
			"symbols"
		} else if (mfun == "Lines") {
			"lines"
		} else {
			"rect"
		}
		gpar = gp_to_gpar(gp)
	})
}


#' @method tmapGridCompHeight tm_legend_standard_portrait
#' @export
tmapGridCompHeight.tm_legend_standard_portrait = function(comp, o) {
	nlev = comp$nitems
	textS = comp$text.size * o$scale
	titleS = if (comp$title == "") 0 else comp$title.size * o$scale
	
	space = get_legend_option(comp$item.space, comp$type)
	spaceNA = get_legend_option(comp$item.na.space, comp$type)
	height = get_legend_option(comp$item.height, comp$type)
	heightNA = get_legend_option(comp$item.na.height, comp$type)
	if (is.na(heightNA)) heightNA = height
	
	# legend height can be 3: NA (components determine height), finite number (number of text lines), or Inf (whole height)
	# for the latter two, there are 3 ways of stretching the legend: padding (space between items), items (heights of all items), or itemsNNA (heights of non-NA items)

	if (comp$type == "symbols") {
		item_heights = pmax(height, rep(comp$gpar$size / textS, length.out = nlev))
		comp$stretch = if (!is.na(comp$height)) "padding" else "none"	
	} else if (comp$type %in% c("rect", "lines")) {
		item_heights = rep(height, nlev)
		if (comp$na.show) item_heights[nlev] = heightNA
		comp$stretch = if (!is.na(comp$height)) "items" else "none"	
	} else if (comp$type == "gradient") {
		item_heights = rep(height, nlev)
		if (comp$na.show) item_heights[nlev] = heightNA
		comp$stretch = if (!is.na(comp$height)) "itemsNNA" else "none"	
	}
	po(o$lin,titleS)
	titleP = comp$title.padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin
	marH = comp$margins[c(3,1)] * textS * o$lin

	if (nlev == 1) {
		items_all = item_heights
		if (comp$stretch == "padding") comp$stretch = "items"
	} else {
		item_space = c(rep(space, nlev - 1 - comp$na.show), {if (comp$na.show) spaceNA else NULL}) 
		items_all = c(rbind(item_heights[-nlev], item_space), item_heights[nlev])
	}
	
	
	
	hs = c(titleP[1], titleH, titleP[2], marH[1], items_all * textS * o$lin, marH[2])
	
	item_ids = seq(5, by = 2, length.out = nlev)
	pad_ids = seq(6, by = 2, length.out = nlev - 1L)
	
	
	hsu = if (comp$stretch == "padding") {
		set_unit_with_stretch(hs, pad_ids)
	} else if (comp$stretch == "items") {
		set_unit_with_stretch(hs, item_ids)
	} else if (comp$stretch == "itemsNNA") {
		if (comp$na.show) set_unit_with_stretch(hs, head(item_ids, -1)) else set_unit_with_stretch(hs, item_ids)
	} else {
		sides = switch(comp$position$just.v, top = "second", bottom = "first", "both")
		set_unit_with_stretch(hs, sides = sides)
	}


	Hin = if (comp$stretch == "none") sum(hs) else comp$height * textS * o$lin
	
	comp$flexRow = NA
	comp$Hin = Hin
	#comp$hs = hs
	comp$hsu = hsu
	
	comp$item_ids = item_ids + 1L
	
	comp
	
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


#' @method tmapGridCompWidth tm_legend_standard_portrait
#' @export
tmapGridCompWidth.tm_legend_standard_portrait = function(comp, o) {
	textS = comp$text.size * o$scale
	titleS = if (comp$title == "") 0 else comp$title.size * o$scale
	
	marW = comp$margins[c(2,4)] * textS * o$lin
	
	width = get_legend_option(comp$item.width, comp$type)
	
	item_widths = if (comp$type == "symbols") pmax(width, rep(comp$gpar$size / textS, length.out = comp$nitems)) else rep(width, comp$nitems)
	item_widths_max = max(item_widths)
	
	
	tW = ifelse(comp$title == "", 0, strwidth(comp$title, units = "inch", cex = titleS, family = comp$title.fontfamily, font = fontface2nr(comp$title.fontface) + sum(comp$title.padding[c(2,4)]) * o$lin * titleS))
	iW = strwidth(comp$labels, units = "inch", cex = textS, family = comp$text.fontfamily, font = fontface2nr(comp$text.fontface)) + (item_widths_max + comp$margin.item.text) * textS * o$lin

	colW = max(tW, iW)
	
	txtW = colW - (item_widths_max + comp$margin.item.text) * textS * o$lin
	
	n = switch(comp$position$align.h, left = c(0, 1), right = c(1, 0), c(0.5, 0.5))
	
	wsu = grid::unit(c(marW[1], 
					   n[1], 
					   item_widths_max * textS * o$lin, 
					   comp$margin.item.text * textS * o$lin, 
					   txtW, 
					   n[2], 
					   marW[2]), units = c("inch", "null", rep("inch", 3), "null", "inch"))
	
	Win = if (is.na(comp$width)) sum(as.numeric(wsu)[c(1, 3:5, 7)]) else comp$width * textS * o$lin
	
	comp$flexCol = 5 #column that will shrink if the null columns will otherwise be sizes < 0. (happens if legend.width < minimal width)
	comp$Win = Win
	comp$wsu = wsu

	comp
	
}


#' @method tmapGridLegPlot tm_legend_standard_portrait
#' @export
tmapGridLegPlot.tm_legend_standard_portrait = function(comp, o) {
	textS = comp$text.size * comp$scale * o$scale
	titleS = if (comp$title == "") 0 else comp$title.size * comp$scale * o$scale
	
	nlev = comp$nitems
	
	wsu = comp$wsu
	hsu = comp$hsu
	
	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu), 
												   widths = wsu,
												   heights = hsu))
	
	if (is.na(comp$title.just)) comp$title.just = comp$position$align.h
	
	if (comp$title.just == "left") {
		grTitle = gridCell(3, 2:(length(comp$wsu)-1), grid::textGrob(comp$title, x = grid::unit(comp$title.padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = grid::gpar(col = comp$title.color, cex = titleS)))
	} else if (comp$title.just == "right") {
		grTitle = gridCell(3, 2:(length(comp$wsu)-1), grid::textGrob(comp$title, x = grid::unit(1, "npc") - grid::unit(comp$title.padding[4] * titleS * o$lin, units = "inch"), just = "right", gp = grid::gpar(col = comp$title.color, cex = titleS)))
	} else {
		grTitle = gridCell(3, 2:(length(comp$wsu)-1), grid::textGrob(comp$title, x = 0.5, just = "center", gp = grid::gpar(col = comp$title.color, cex = titleS)))
	}
	
	textW = strwidth(comp$labels, units = "inch", cex = textS, family = comp$text.fontfamily, font = fontface2nr(comp$text.fontface))
	scale_labels = max(textW / grid::convertUnit(wsu[5], unitTo = "inch", valueOnly = TRUE), 1)
	
	
	grText = mapply(function(i, id) gridCell(id, 5, grid::textGrob(comp$labels[i], x = 0, just = "left", gp = grid::gpar(col = comp$text.color, cex = textS / scale_labels, fontface = comp$text.fontface, fontfamily = comp$text.fontfamily))), 1L:nlev, comp$item_ids, SIMPLIFY = FALSE)
	
	ticks = get_legend_option(comp$ticks, comp$type)
	ticks.disable.na = get_legend_option(comp$ticks.disable.na, comp$type)
	if (length(ticks)) {
		tick_col = if (is.na(comp$ticks.col)) comp$gp$col else comp$ticks.col
		if (is.na(tick_col)) tick_col = "white"
		
		ticks_in_margin = sapply(ticks, function(l) all(l>=1))
		
		ni = nlev - (comp$na.show && ticks.disable.na)
		
		# tick marks in margin (specified with x coordinates between 1 and 2)
		if (any(ticks_in_margin)) {
			grTicksMargin = do.call(c, lapply(ticks[ticks_in_margin], function(x) {
				mapply(function(i, id) gridCell(id, 4, grid::linesGrob(x = x - 1, y = c(0.5, 0.5), gp = grid::gpar(col = tick_col, lwd = comp$ticks.lwd * comp$scale))), 1L:ni, comp$item_ids[1L:ni], SIMPLIFY = FALSE)
			}))
		} else {
			grTicksMargin = NULL
		}
		
		if (any(!ticks_in_margin)) {
			grTicksItem = do.call(c, lapply(ticks[!ticks_in_margin], function(x) {
				mapply(function(i, id) gridCell(id, 3, grid::linesGrob(x = x, y = c(0.5, 0.5), gp = grid::gpar(col = tick_col, lwd = comp$ticks.lwd * comp$scale))), 1L:ni, comp$item_ids[1L:ni], SIMPLIFY = FALSE)
			}))
		} else {
			grTicksItem = NULL
		}
		
		grTicks = c(grTicksMargin, grTicksItem)
	} else {
		grTicks = NULL
	}
	

	
	if (getOption("tmap.design.mode")) {
		df = expand.grid(col = 1:length(comp$wsu),
						 row = 1:length(comp$hsu))
		
		grDesign = lapply(1:nrow(df), function(i) gridCell(df$row[i], df$col[i], grid::rectGrob(gp=gpar(fill=NA,col="red", lwd=2))))
	} else {
		grDesign = NULL
	}
	
	gp = comp$gp
	
	
	
	if (comp$type == "gradient") {
		# for borders
		gpars = gp_to_gpar(gp, id = 1L, sel = "col")
		
		# for gradient fill
		nlev2 = (nlev-comp$na.show) # nlev without na
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
			id2 = tail(which(!is.na(fill_list[[nlev2]])), 1)

			y1 = 1 - ((id1-1) / 10) / nlev2
			y2 = 1 - ((id2 / 10) / nlev2 + ((nlev2-1)/nlev2))
			h = y1 - y2
			
			if (vary_fill) {
				cols = unlist(fill_list)[id1:(10*(nlev2-1) + id2)]
				cols_alph = paste0(cols, tmap::num_to_hex(gp$fill_alpha[1] * 255))
			} else {
				alph = unlist(alpha_list)[id1:(10*(nlev2-1) + id2)]
				cols_alph = paste0(col2hex(gp$fill[1]), tmap::num_to_hex(alph * 255))
			}
			grItems1 = list(gridCell(comp$item_ids[lvs], 3, grid::rectGrob(y = y2 + 0.5*h, height= h, gp=gpar(fill = grid::linearGradient(colours = rev(cols_alph)), col = NA))))
		} else {
			grItems1 = mapply(function(id, f, a) {
				h = 1 / length(f)
				ys = seq(1-.5*h, by = -h, length.out = length(f))
				#f[!is.na(f)] = "red"
				gpi = grid::gpar(fill = f, alpha = a, col = NA)
				grid::rect(id, 3, grid::rectGrob(y = ys, height = h, gp = gpi))
			}, comp$item_ids[lvs], fill_list, alpha_list, SIMPLIFY = FALSE)
		}		
		
		
		
		if (vary_fill) {
			y1 = (sum(is.na(fill_list[[1]])) * .1) / nlev2
			y2 = (sum(is.na(fill_list[[nlev2]])) * .1) / nlev2
		} else {
			y1 = (sum(is.na(alpha_list[[1]])) * .1) / nlev2
			y2 = (sum(is.na(alpha_list[[nlev2]])) * .1) / nlev2
		}
		
		grItems2 = list(gridCell(comp$item_ids[lvs], 3, rndrectGrob(y = grid::unit(y2, "npc"), just = c("center", "bottom"), height = grid::unit(1-(y1+y2), "npc"), gp = gpars, r = comp$item.r)))
		if (comp$na.show) {
			gpars$fill = gp$fill[ifelse(length(gp$fill), nlev, 1)]
			gpars$fill_alpha = gp$fill[ifelse(length(gp$fill_alpha), nlev, 1)]
			# fill and border for NA
			grItems1 = c(grItems1, list(gridCell(comp$item_ids[nlev], 3, rndrectGrob(gp = gpars, r = comp$item.r))))
		}
		
		# borders
		grItems = c(grItems1, grItems2)
		
		
	} else if (comp$type == "rect") {
		#gps = split_gp(gp, n = nlev)
		
		diffAlpha = !any(is.na(c(gp$fill_alpha, gp$col_alpha))) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
		
		
		if (diffAlpha) {
			gpars1 = gp_to_gpar(gp, sel = "fill", split_to_n = nlev) #lapply(gps, gp_to_gpar_fill)
			gpars2 = gp_to_gpar(gp, sel = "col", split_to_n = nlev) #lapply(gps, gp_to_gpar_borders)
			
			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, rndrectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			grItems = mapply(function(i, gpar1i, gpar2i) gridCell(id, 3, {
				grid::grobTree(
					rndrectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar1i, r = comp$item.r),
					rndrectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar2i, r = comp$item.r))
			}), comp$item_ids, gpars1, gpars2, SIMPLIFY = FALSE)
			
		} else {
			gpars = gp_to_gpar(gp, sel = "all", split_to_n = nlev)#lapply(gps, gp_to_gpar)
			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			grItems = mapply(function(id, gpari) gridCell(id, 3, rndrectGrob(gp = gpari, r = comp$item.r)), comp$item_ids, gpars, SIMPLIFY = FALSE)
		}
	} else if (comp$type == "lines") {
		#gps = split_gp(gp, n = nlev)
		
		gpars = gp_to_gpar(gp, sel = "col", split_to_n = nlev)#lapply(gps, gp_to_gpar)
			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
		grItems = mapply(function(id, gpari) gridCell(id, 3, grid::linesGrob(y = grid::unit(c(0.5,0.5), "npc"), gp = gpari)), comp$item_ids, gpars, SIMPLIFY = FALSE)

		
	} else if (comp$type == "symbols") {
		if (length(gp$size) == 1) gp$size = min(gp$size, min(get_legend_option(comp$item.height, "symbols"),
															 get_legend_option(comp$item.width, "symbols")) * comp$textS)
		
		gpars = gp_to_gpar(gp, split_to_n = nlev)
		
		# scale down (due to facet use)
		gpars = lapply(gpars, rescale_gp, scale = o$scale_down)
		
#		sizes = 
		
		grItems = mapply(function(id, gpari) gridCell(id, 3, grid::pointsGrob(x=0.5, y=0.5, pch = gpari$shape, size = grid::unit(gpari$size, "lines"), gp = gpari)), comp$item_ids, gpars, SIMPLIFY = FALSE)
	}
	
	
	g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, grTicks, grDesign, list(vp = vp)))
	
	g
		
}

