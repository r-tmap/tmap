tmapGridLegHeight = function(...) {
	UseMethod("tmapGridLegHeight")
}

tmapGridLegWidth = function(...) {
	UseMethod("tmapGridLegWidth")
}

tmapGridLegPlot = function(...) {
	UseMethod("tmapGridLegPlot")
}


#' @method tmapGridLegHeight tm_legend_portrait
#' @export
tmapGridLegHeight.tm_legend_portrait = function(leg, o) {
	nlev = leg$nitems
	textS = leg$text.size
	titleS = if (leg$title == "") 0 else leg$title.size
	
	padding = get_legend_option(leg$item.padding, leg$type)
	paddingNA = get_legend_option(leg$item.na.padding, leg$type)
	heights = get_legend_option(leg$item.height, leg$type)
	
	item_heights = if (leg$type == "symbols") pmax(heights, rep(leg$gpar$size / textS, length.out = nlev)) else rep(heights, nlev)
	
	
	if (is.na(leg$height) && (padding == -1 || any(item_heights == -1))) leg$height = Inf
	if (is.infinite(leg$height)) {
		if (leg$type == "symbols") padding = -1
		if (padding != -1 && all(item_heights != -1)) padding = -1
		if (padding == -1 && any(item_heights == -1)) item_heights = rep(1, nlev)
		if (padding == -1 && leg$item.na.padding != -1) leg$item.na.padding == -1
		leg$stretch = if (padding == -1) "padding" else "items"
	} else if (is.na(leg$height)) {
		leg$stretch = "none"
	} else {
		if (leg$type == "symbols") padding = -1
		if (padding == -1 && any(item_heights == -1)) item_heights = rep(1, nlev)
		if (padding != -1 || all(item_heights != -1)) padding = -1
		leg$stretch = if (padding == -1) "padding" else "items"
	}
	
	titleP = leg$title.padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin
	
	marH = leg$margins[c(3,1)] * textS * o$lin
	
	item_paddings = c(rep(padding, leg$nitems - leg$na.show), {if (leg$na.show) paddingNA else NULL}) 
	item_totals = item_heights + item_paddings
	
	if (leg$stretch == "none") {
		hsinch = c(titleP[1], titleH, titleP[2], marH[1], item_totals * textS * o$lin, marH[2])
		itemHsIn = grid::unit(item_heights * textS * o$lin, units = rep("inch", nlev))
		
		hs = grid::unit(c(hsinch, 1), units = c(rep("inch", length(hsinch)), "null"))
		Hin = sum(hsinch)
	} else {
		Hin = leg$height * textS * o$lin
		
		hsunits = c(titleP[1], titleH, titleP[2], marH[1], rep(1/nlev, nlev), marH[2])
		hs = grid::unit(hsunits, units = c(rep("inch", 4), rep("null", nlev), "inch"))
		
		if (leg$stretch == "padding") {
			#hsinch = c(titleP[1], titleH, titleP[2], marH[1], item_heights * textS * o$lin, marH[2])
			itemHsIn = grid::unit(item_heights * textS * o$lin, units = rep("inch", nlev))
		} else {
			#hsinch = c(titleP[1], titleH, titleP[2], marH[1], item_paddings * textS * o$lin, marH[2])
			itemHsIn = grid::unit(1, "npc") - grid::unit(item_paddings * textS * o$lin, units = rep("inch", nlev))
		}
	}
	
	leg$itemHsIn = itemHsIn # heights of the items
	leg$Hin = Hin
	leg$hs = hs

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
	
	itemWsIn = grid::unit(item_widths_max * textS * o$lin, units = "inch")
	
	
	ws = grid::unit(c(marW[1], item_widths_max * textS * o$lin, leg$margin.item.text * textS * o$lin, txtW, marW[2], 1), units = c(rep("inch", 5), "null"))
	
	leg$Win = sum(as.numeric(ws)[1:5])
	leg$itemWsIn = itemWsIn
	leg$ws = ws
	
	po(itemWsIn)
	
	leg
	
}


#' @method tmapGridLegPlot tm_legend_portrait
#' @export
tmapGridLegPlot.tm_legend_portrait = function(leg, o) {
	textS = leg$text.size * leg$scale
	titleS = if (leg$title == "") 0 else leg$title.size * leg$scale
	
	nlev = leg$nitems
	
	vp = grid::viewport(layout = grid::grid.layout(ncol = length(leg$ws),
												   nrow = length(leg$hs), 
												   widths = leg$ws * leg$scale,
												   heights = leg$hs * leg$scale))
	
	grTitle = gridCell(2, 2:(length(leg$ws)-1), grid::textGrob(leg$title, x = grid::unit(leg$title.padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = grid::gpar(cex = titleS)))
	grText = lapply(1:nlev, function(i) gridCell(i+4, 4, grid::textGrob(leg$labels[i], x = 0, just = "left", gp = grid::gpar(cex = textS, fontface = leg$text.fontface, fontfamily = leg$text.fontfamily))))
	
	
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
			gridCell(i+4, 2, grid::rectGrob(y = ys, height = h, gp = gpi))
		}, lvs, fill_list, alpha_list, SIMPLIFY = FALSE)
		
		# border (for fill part)
		nlines_filled = as.numeric(leg$itemHsIn[lvs])
		
		if (vary_fill) {
			y1 = (sum(is.na(fill_list[[1]])) * .1) / nlev2
			y2 = (sum(is.na(fill_list[[nlev2]])) * .1) / nlev2
		} else {
			y1 = (sum(is.na(alpha_list[[1]])) * .1) / nlev2
			y2 = (sum(is.na(alpha_list[[nlev2]])) * .1) / nlev2
		}
		
		
		#h = (1 - y1 - y2) * sum(nlines_filled) * lH
		h = (1L - y1 - y2) * sum(nlines_filled)
		ym = (y2 + (1L - y1 - y2)/2) * sum(nlines_filled)
		
		grItems2 = list(gridCell(lvs+4, 2, grid::rectGrob(y = grid::unit(ym, "inches"), width = grid::unit(o$lin* textS, "inches"), height = grid::unit(h, "inches"), gp = gpars)))
		if (leg$na.show) {
			gpars$fill = gp$fill[ifelse(length(gp$fill), nlev, 1)]
			gpars$fill_alpha = gp$fill[ifelse(length(gp$fill_alpha), nlev, 1)]
			# fill and border for NA
			grItems1 = c(grItems1, list(gridCell(nlev+4, 2, grid::rectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpars))))
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

