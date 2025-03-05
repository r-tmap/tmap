tmapGridCompPrepare = function(comp, o) {
	UseMethod("tmapGridCompPrepare")
}

tmapGridCompHeight = function(comp, o) {
	UseMethod("tmapGridCompHeight")
}

tmapGridCompWidth = function(comp, o) {
	UseMethod("tmapGridCompWidth")
}

tmapGridLegPlot = function(comp, o, fH, fW) {
	UseMethod("tmapGridLegPlot")
}

#' @export
tmapGridCompPrepare.tm_legend_standard_landscape = function(comp, o) {
	tmapGridCompPrepare.tm_legend_standard_portrait(comp, o)
}




#' @export
tmapGridCompHeight.tm_legend_standard_landscape = function(comp, o) {
	nlev = comp$nitems
	textS = comp$text.size #* o$scale
	titleS = if (comp$title == "") 0 else comp$title.size * number_text_lines(comp$title)#* o$scale

	height = get_legend_option(comp$item.height, comp$type)

	item_height = if (comp$type == "symbols") {
		shps = rep(comp$gpar$shape, length.out = nlev)
		max(height, rep(comp$gpar$size, length.out = nlev) / textS * ifelse(shps > 999, comp$layer_args$icon.scale, 1))
	} else height
	itemHsIn = grid::unit(item_height * textS * o$lin, units = "inch")

	titleP = comp$title.padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin

	marH = comp$margins[c(3,1)] * textS * o$lin


	hsinch = c(titleP[1], titleH, titleP[2], marH[1], item_height * textS * o$lin, c(comp$item_text.margin, 1) * textS * o$lin,marH[2])

	Hin = if (is.na(comp$height)) sum(hsinch) else comp$height * textS * o$lin

	sides = switch(comp$position$just.v, top = "second", bottom = "first", "both")
	hsu = set_unit_with_stretch(hsinch, sides = sides)



	comp$flexRow = NA

	comp$Hin = Hin
	comp$hsu = hsu

	comp

}


#' @export
tmapGridCompWidth.tm_legend_standard_landscape = function(comp, o) {
	nlev = comp$nitems
	textS = comp$text.size #* o$scale
	titleS = if (comp$title == "") 0 else comp$title.size #* o$scale

	space = get_legend_option(comp$item.space, comp$type)
	spaceNA = get_legend_option(comp$item.na.space, comp$type)
	width = get_legend_option(comp$item.width, comp$type)
	widthNA = get_legend_option(comp$item.na.width, comp$type)
	if (is.na(widthNA)) widthNA = width

	labelW = graphics::strwidth(comp$labels, units = "inch", cex = textS, family = comp$text.fontfamily, font = fontface2nr(comp$text.fontface)) / o$lin

	# legend width can be 3: NA (components determine width), finite number (number of text lines), or Inf (whole width)
	# for the latter two, there are 3 ways of stretching the legend: padding (space between items), items (widths of all items), or itemsNNA (widths of non-NA items)

	if (comp$type == "symbols") {
		shps = rep(comp$gpar$shape, length.out = nlev)
		item_widths = pmax(width, rep(comp$gpar$size / textS, length.out = nlev) * ifelse(shps > 999, comp$layer_args$icon.scale, 1), labelW)
		comp$stretch = if (!is.na(comp$width)) "padding" else "none"
	} else if (comp$type %in% c("rect", "lines")) {
		item_widths = pmax(rep(width, nlev), labelW)
		if (comp$na.show) item_widths[nlev] = widthNA
		comp$stretch = if (!is.na(comp$width)) "items" else "none"
	} else if (comp$type == "gradient") {
		item_widths = rep(width, nlev)
		if (comp$na.show) item_widths[nlev] = widthNA
		comp$stretch = if (!is.na(comp$width)) "itemsNNA" else "none"
	} else if (comp$type == "text") {
		item_widths = pmax(width, rep(comp$gpar$size / textS, length.out = nlev), labelW)
		comp$stretch = if (!is.na(comp$width)) "padding" else "none"
	}


	titleP = comp$title.padding[c(2,4)] * titleS * o$lin
	titleW = if (titleS > 0) graphics::strwidth(comp$title, units = "inch", cex = titleS, family = comp$title.fontfamily, font = fontface2nr(comp$title.fontface)) * o$lin else 0

	marW = comp$margins[c(2,4)] * textS * o$lin



	item_space = c(rep(space, nlev - 1 - comp$na.show), {if (comp$na.show) spaceNA else NULL})
	items_all = c(rbind(item_widths[-nlev], item_space), item_widths[nlev])


	ws = c(marW[1], items_all * textS * o$lin, marW[2])


	item_ids = seq(2, by = 2, length.out = nlev)
	pad_ids = seq(3, by = 2, length.out = nlev - 1L)

	wsu = if (comp$stretch == "padding") {
		set_unit_with_stretch(ws, pad_ids)
	} else if (comp$stretch == "items") {
		set_unit_with_stretch(ws, item_ids)
	} else if (comp$stretch == "itemsNNA") {
		if (comp$na.show) set_unit_with_stretch(ws, head(item_ids, -1)) else set_unit_with_stretch(ws, item_ids)
	} else {
		sides = switch(comp$position$align.h, left = "second", right = "first", "both")
		set_unit_with_stretch(ws, sides = sides)
	}

	Win = if (comp$stretch == "none") sum(ws) else comp$width * textS * o$lin

	comp$flexCol = NA

	comp$Win = Win
	#comp$hs = hs
	comp$wsu = wsu

	comp$item_ids = item_ids + 1L

	comp


}


#' @export
tmapGridLegPlot.tm_legend_standard_landscape = function(comp, o, fH, fW) {
	if (comp$type != "gradient") comp$labels_select = TRUE # labels_select only needed for continuous legends (#1039)


	icons = all(comp$gp$shape >= 1000)

	# replace gp visual values with user-specified used (e.g. tm_shape(World) + tm_polygons("HPI", fill.legend = tm_legend(col = "red")))
	comp$gp = add_user_specified_values(comp$gp, comp[intersect(names(comp), names(comp$gp))])

	# icons replaced by 'normal' shapes: apply icon.scale
	if (icons && all(comp$gp$shape < 1000) && !is.null(comp$layer_args$icon.scale)) {
		comp$gp$size = comp$gp$size * comp$layer_args$icon.scale
	}

	textS = comp$text.size * comp$scale
	titleS = if (comp$title == "") 0 else comp$title.size * comp$scale

	nlev = comp$nitems

	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))

	if (is.na(comp$title.align)) comp$title.align = comp$position$align.h

	titleGP = grid::gpar(col = comp$title.color, cex = titleS, fontface = comp$title.fontface, fontfamily = comp$title.fontfamily, alpha = comp$title.alpha)

	if (comp$title.align == "left") {
		grTitle = gridCell(3, 3:(length(comp$wsu)-2), grid::textGrob(comp$title, x = grid::unit(comp$title.padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = titleGP))
	} else if (comp$title.align == "right") {
		grTitle = gridCell(3, 3:(length(comp$wsu)-2), grid::textGrob(comp$title, x = grid::unit(1, "npc") - grid::unit(comp$title.padding[4] * titleS * o$lin, units = "inch"), just = "right", gp = titleGP))
	} else {
		grTitle = gridCell(3, 3:(length(comp$wsu)-2), grid::textGrob(comp$title, x = 0.5, just = "center", gp = titleGP))
	}

	textW = graphics::strwidth(comp$labels, units = "inch", cex = textS, family = comp$text.fontfamily, font = fontface2nr(comp$text.fontface))
	scale_labels = max(textW / grid::convertUnit(wsu[comp$item_ids], unitTo = "inch", valueOnly = TRUE), 1)

	grText = mapply(function(i, id) gridCell(8, id, grid::textGrob(comp$labels[i], x = 0.5, just = "center", gp = grid::gpar(col = comp$text.color, cex = textS/scale_labels, fontface = comp$text.fontface, fontfamily = comp$text.fontfamily, alpha = comp$text.alpha))), (1L:nlev)[comp$labels_select], comp$item_ids[comp$labels_select], SIMPLIFY = FALSE)

	ticks = get_legend_option(comp$ticks, comp$type)
	ticks.disable.na = get_legend_option(comp$ticks.disable.na, comp$type)
	if (length(ticks)) {
		tick_col = if (is.na(comp$ticks.col)) comp$text.color else comp$ticks.col
		if (is.na(tick_col)) tick_col = "white"

		ticks_in_margin = sapply(ticks, function(l) all(l>=1))

		ni = nlev - (comp$na.show && ticks.disable.na)

		tck_ids = (1L:ni)[rep(comp$labels_select, length.out = ni)]

		# tick marks in margin (specified with y coordinates between 1 and 2)
		if (any(ticks_in_margin)) {
			grTicksMargin = do.call(c, lapply(ticks[ticks_in_margin], function(y) {
				mapply(function(i, id) gridCell(8, id, grid::linesGrob(x = c(0.5, 0.5), y = y, gp = grid::gpar(col = tick_col, lwd = comp$ticks.lwd * comp$scale * o$scale_down))), tck_ids, comp$item_ids[tck_ids], SIMPLIFY = FALSE)
			}))
		} else {
			grTicksMargin = NULL
		}

		if (!all(ticks_in_margin)) {
			grTicksItem = do.call(c, lapply(ticks[!ticks_in_margin], function(y) {
				mapply(function(i, id) gridCell(6, id, grid::linesGrob(x = c(0.5, 0.5), y = 1 - y, gp = grid::gpar(col = tick_col, lwd = comp$ticks.lwd * comp$scale * o$scale_down))), tck_ids, comp$item_ids[tck_ids], SIMPLIFY = FALSE)
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
		gpars = gp_to_gpar(gp, id = 1L, sel = "col", o = o, type = comp$type)

		# for gradient fill
		nlev2 = (nlev-comp$na.show) # nlev without na
		lvs = 1:nlev2

		vary_fill = (length(gp$fill) > 1)
		vary_fill_alpha = (length(gp$fill_alpha) > 1)

		vary_col = (length(gp$col) > 1)
		vary_col_alpha = (length(gp$col_alpha) > 1)


		# vary fill color
		if (vary_fill) {
			fill_list = cont_split(gp$fill[lvs])
			fill_list = lapply(fill_list, function(i) {
				i[i=="NA"] <- NA
				i
			})
		} else if (vary_col) {
			fill_list = cont_split(gp$col[lvs])
			fill_list = lapply(fill_list, function(i) {
				i[i=="NA"] <- NA
				i
			})
			gpars$col = NA
		} else {
			fill_list = rep(gp$fill[1], nlev2)
		}

		# vary fill alpha
		if (vary_fill_alpha) {
			alpha_list = cont_split(gp$fill_alpha[lvs])
			alpha_list = lapply(alpha_list, function(i) {
				i[i=="NA"] <- 0
				as.numeric(i)
			})
			if (!vary_fill) {
				fill_list = mapply(rep.int, fill_list, vapply(alpha_list, length, FUN.VALUE = integer(1)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
			}

		} else if (vary_col_alpha) {
			alpha_list = cont_split(gp$col_alpha[lvs])
			alpha_list = lapply(alpha_list, function(i) {
				i[i=="NA"] <- 0
				as.numeric(i)
			})

			if (!vary_col) {
				fill_list = mapply(rep.int, fill_list, vapply(alpha_list, length, FUN.VALUE = integer(1)), SIMPLIFY = FALSE, USE.NAMES = FALSE)
			}
		} else {
			if (!is.na(gp$fill_alpha[1])) {
				alpha_list = rep(gp$fill_alpha[1], nlev2)
			} else {
				alpha_list = rep(gp$col_alpha[1], nlev2)
			}
		}



		# fill
		nvv = o$continuous.nclass_per_legend_break

		if (o$use_gradient) {
			id1 = which(!is.na(fill_list[[1]]))[1]
			id2 = tail(which(!is.na(fill_list[[nlev2]])), 1)

			x1 = ((id1-1) / nvv) / nlev2
			x2 = (id2 / nvv) / nlev2 + ((nlev2-1)/nlev2)
			w = x2 - x1

			if (vary_fill) {
				cols = unlist(fill_list)[id1:(nvv*(nlev2-1) + id2)]
				cols_alph = paste0(cols, num_to_hex(gp$fill_alpha[1] * 255))
			} else {
				alph = unlist(alpha_list)[id1:(nvv*(nlev2-1) + id2)]
				cols_alph = paste0(col2hex(gp$fill[1]), num_to_hex(alph * 255))
			}
			grItems1 = list(gridCell(6, comp$item_ids[lvs], grid::rectGrob(x = x1 + 0.5*w, width= w, gp=gpar(fill = grid::linearGradient(colours = cols_alph), col = NA))))
		} else {
			grItems1 = mapply(function(id, f, a) {
				w = 1 / length(f)
				xs = seq(.5*w, by = w, length.out = length(f))
				#f[!is.na(f)] = "red"
				gpi = grid::gpar(fill = f, alpha = a, col = NA)
				gridCell(6, id, grid::rectGrob(x = xs, width = w, gp = gpi))
			}, comp$item_ids[lvs], fill_list, alpha_list, SIMPLIFY = FALSE)
		}

		if (vary_fill) {
			x1 = (sum(is.na(fill_list[[1]])) / nvv) / nlev2
			x2 = (sum(is.na(fill_list[[nlev2]])) / nvv) / nlev2
		} else {
			x1 = (sum(is.na(alpha_list[[1]])) / nvv) / nlev2
			x2 = (sum(is.na(alpha_list[[nlev2]])) / nvv) / nlev2
		}


		grItems2 = list(gridCell(6, comp$item_ids[lvs], rndrectGrob(x = grid::unit(x1, "npc"), just = c("left", "center"), width = grid::unit(1-(x1+x2), "npc"), gp = gpars, r = comp$item.r)))
		if (comp$na.show) {
			gpars$fill = gp$fill[ifelse(length(gp$fill), nlev, 1)]
			gpars$fill_alpha = gp$fill[ifelse(length(gp$fill_alpha), nlev, 1)]
			# fill and border for NA
			grItems1 = c(grItems1, list(gridCell(6, comp$item_ids[nlev], rndrectGrob(gp = gpars, r = comp$item.r))))
		}

		# borders
		grItems = c(grItems1, grItems2)


	} else if (comp$type == "rect") {
		#gps = split_gp(gp, n = nlev)

		diffAlpha = !any(is.na(c(gp$fill_alpha, gp$col_alpha))) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))


		if (diffAlpha) {
			gpars1 = gp_to_gpar(gp, sel = "fill", split_to_n = nlev, o = o, type = comp$type) #lapply(gps, gp_to_gpar_fill)
			gpars2 = gp_to_gpar(gp, sel = "col", split_to_n = nlev, o = o, type = comp$type) #lapply(gps, gp_to_gpar_borders)

			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			grItems = mapply(function(i, gpar1i, gpar2i) gridCell(6, id, {
				grid::grobTree(
					rndrectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar1i, r = comp$item.r),
					rndrectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar2i, r = comp$item.r))
			}), comp$item_ids, gpars1, gpars2, SIMPLIFY = FALSE)

		} else {
			gpars = gp_to_gpar(gp, sel = "all", split_to_n = nlev, o = o, type = comp$type)#lapply(gps, gp_to_gpar)
			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, rndrectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			grItems = mapply(function(id, gpari) gridCell(6, id, rndrectGrob(gp = gpari, r = comp$item.r)), comp$item_ids, gpars, SIMPLIFY = FALSE)
		}

	} else if (comp$type == "lines") {
		gpars = gp_to_gpar(gp, sel = "col", split_to_n = nlev, o = o, type = comp$type)#lapply(gps, gp_to_gpar)
		#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
		grItems = mapply(function(id, gpari) gridCell(6, id, grid::linesGrob(x = grid::unit(c(0.5,0.5), "npc"), gp = gpari)), comp$item_ids, gpars, SIMPLIFY = FALSE)
	} else if (comp$type == "symbols") {
		if (length(gp$size) == 1) gp$size = min(gp$size, min(get_legend_option(comp$item.height, "symbols"),
															 get_legend_option(comp$item.width, "symbols")) * comp$textS)

		gp = swap_pch_15_20(gp)

		gpars = gp_to_gpar(gp, split_to_n = nlev, o = o, type = comp$type)

		diffAlpha = !anyNA(c(gp$fill_alpha, gp$col_alpha)) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
		if (diffAlpha) {
			gpars1 = gp_to_gpar(gp, split_to_n = nlev, o = o, type = comp$type, sel = "fill")
			gpars2 = gp_to_gpar(gp, split_to_n = nlev, o = o, type = comp$type, sel = "col")
		} else {
			gpars1 = vector(mode = "list", length = nlev)
			gpars2 = vector(mode = "list", length = nlev)
		}

		# scale down (due to facet use)
		gpars = lapply(gpars, rescale_gp, scale = o$scale_down)


		shapeLib = get("shapeLib", envir = .TMAP)
		justLib = get("justLib", envir = .TMAP)

		grItems = mapply(function(id, gpari, gpari1, gpari2) {
			grobs = if (gpari$shape > 999) {
				grbs = gList(shapeLib[[gpari$shape-999]])
				grid::gTree(children=grbs, vp=viewport(x=0.5,
													   y=0.5,
													   width=unit(gpari$size*9/10, "lines"),
													   height=unit(gpari$size*9/10, "lines")))
			} else {
				if (diffAlpha) {
					grid::grobTree(
						grid::pointsGrob(x=0.5, y=0.5, pch = gpari1$shape, size = grid::unit(gpari1$size, "lines"), gp = gpari1),
						grid::pointsGrob(x=0.5, y=0.5, pch = gpari2$shape, size = grid::unit(gpari2$size, "lines"), gp = gpari2)
					)
				} else {
					grid::pointsGrob(x=0.5, y=0.5, pch = gpari$shape, size = grid::unit(gpari$size, "lines"), gp = gpari)
				}
			}
			#grb = gTree(children=do.call(gList, grobs), name=paste0("symbols_", id))
			gridCell(6, id, grobs)
		}, comp$item_ids, gpars, gpars1, gpars2, SIMPLIFY = FALSE)



	} else if (comp$type == "text") {
		gp$text[is.na(gp$text)] = getAesOption("value.const", o, aes = "text", layer = "text")

		if (length(gp$cex) == 1) gp$cex = min(gp$cex, min(get_legend_option(comp$item.height, "text"),
														  get_legend_option(comp$item.width, "text")) * comp$textS)

		bgcols = rep(gp$bgcol, length.out = nlev)
		bgcols_alpha = rep(gp$bgcol_alpha, length.out = nlev)

		# in case size is a continuous scale
		if (is.character(gp$cex)) gp$cex = vapply(cont_split(gp$cex), FUN = function(x) {
			as.numeric(x[round(length(x)/2)])
		}, FUN.VALUE = numeric(1))


		gpars = gp_to_gpar(gp, split_to_n = nlev, o = o, type = comp$type)

		# scale down (due to facet use)
		gpars = lapply(gpars, rescale_gp, scale = o$scale_down)

		grItems = mapply(function(id, gpari, txt, bgcol, bgcol_alpha, size) {
			gridCell(6, id, {
				grid::gList(
					rndrectGrob(width = grid::unit(grid::convertWidth(grid::stringWidth(txt), "inches")* size, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = grid::gpar(fill = bgcol, alpha = bgcol_alpha, col = NA), r = comp$item.r),
					grid::textGrob(x=0.5, y=0.5, label = txt, gp = gpari))
			})
		}, comp$item_ids, gpars, gp$text, bgcols, bgcols_alpha, gp$cex, SIMPLIFY = FALSE)

	}


	g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, grTicks, grDesign, list(vp = vp)))

	g

}
