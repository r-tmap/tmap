#' @export
tmapGridCompPrepare.tm_legend_standard_portrait = function(comp, o) {
	within(comp, {
		bg.color = do.call("process_color", c(list(col=bg.color), o$pc))
		title.color = do.call("process_color", c(list(col=title.color), o$pc))
		text.color = do.call("process_color", c(list(col=text.color), o$pc))

		type = if ("biv" %in% names(attributes(gp$fill))) {
			"bivariate"
		} else if (!is.na(gp$fill[1]) && any(nchar(gp$fill) > 20) || !is.na(gp$fill_alpha[1]) && any(nchar(gp$fill_alpha) > 20) ||
				   !is.na(gp$col[1]) && any(nchar(gp$col) > 20) || !is.na(gp$col_alpha[1]) && any(nchar(gp$col_alpha) > 20)) {
			"gradient"
		} else if (!is.na(gp$shape[1])) {
			"symbols"
		} else if (mfun == "Lines") {
			"lines"
		} else if (mfun == "Text") {
			"text"
		} else {
			"rect"
		}

		text.size = text.size * o$scale
		title.size = title.size * o$scale

		title.align = get_vector_id(title.align, type)
		xlab.align = get_vector_id(xlab.align, type)
		ylab.align = get_vector_id(ylab.align, type)

		if (!is.na(height) && height < 0) {
			height = -height / absolute_fontsize
		}
		if (!is.na(width) && width < 0) {
			width = -width / absolute_fontsize
		}


		gpar = gp_to_gpar(gp, o = o, type = comp$type)
	})
}


#' @export
tmapGridCompHeight.tm_legend_standard_portrait = function(comp, o) {

	nlev = if (comp$type == "bivariate") attr(comp$gp$fill, "m") + 1L else comp$nitems




	textS = comp$text.size #* o$scale
	titleS = if (comp$title == "") 0 else comp$title.size * number_text_lines(comp$title) # * o$scale

	space = get_legend_option(comp$item.space, comp$type)
	spaceNA = get_legend_option(comp$item.na.space, comp$type)
	height = get_legend_option(comp$item.height, comp$type)
	heightNA = get_legend_option(comp$item.na.height, comp$type)
	if (is.na(heightNA)) heightNA = height

	# legend height can be 3: NA (components determine height), finite number (number of text lines), or Inf (whole height)
	# for the latter two, there are 3 ways of stretching the legend: padding (space between items), items (heights of all items), or itemsNNA (heights of non-NA items)

	if (comp$type == "symbols") {
		shps = rep(comp$gpar$shape, length.out = nlev)
		item_heights = pmax(height, rep(comp$gpar$size / textS, length.out = nlev) * ifelse(shps > 999, comp$layer_args$icon.scale, 1))
		comp$stretch = if (!is.na(comp$height)) "padding" else "none"
	} else if (comp$type %in% c("rect", "lines", "bivariate")) {
		item_heights = rep(height, nlev)
		if (comp$na.show) item_heights[nlev] = heightNA
		comp$stretch = if (!is.na(comp$height)) "items" else "none"
	} else if (comp$type == "gradient") {
		item_heights = rep(height, nlev)
		if (comp$na.show) item_heights[nlev] = heightNA
		comp$stretch = if (!is.na(comp$height)) "itemsNNA" else "none"
	} else if (comp$type == "text") {
		item_heights = pmax(height, rep(comp$gpar$size / textS, length.out = nlev))
		comp$stretch = if (!is.na(comp$height)) "padding" else "none"
	}
	titleP = comp$title.padding[c(3,1)] * titleS * o$lin
	titleH = titleS * o$lin

	textH = textS * o$lin

	if (comp$type == "bivariate") {
		xlabS = if (comp$xlab == "") 0 else comp$xlab.size * number_text_lines(comp$xlab)
		xlabP = comp$xlab.padding[c(3,1)] * xlabS * o$lin
		xlabH = xlabS * o$lin

	} else {
		xlabP = c(0, 0)
		xlabH = 0
	}



	marH = comp$margins[c(3,1)] * textS * o$lin

	if (nlev == 1) {
		items_all = item_heights
		if (comp$stretch == "padding") comp$stretch = "items"
	} else {
		item_space = c(rep(space, nlev - 1 - comp$na.show), {if (comp$na.show) spaceNA else NULL})
		items_all = c(rbind(item_heights[-nlev], item_space), item_heights[nlev])
	}



	hs = c(titleP[1], titleH, titleP[2], marH[1], items_all * textS * o$lin, xlabP[1], xlabH, xlabP[2], marH[2])

	item_ids = seq(5, by = 2, length.out = nlev)
	pad_ids = seq(6, by = 2, length.out = nlev - 1L)


	hsu = if (comp$stretch == "padding") {
		set_unit_with_stretch(hs, pad_ids)
	} else if (comp$stretch == "items") {
		set_unit_with_stretch(hs, item_ids)
	} else if (comp$stretch == "itemsNNA") {
		if (comp$na.show) set_unit_with_stretch(hs, head(item_ids, -1)) else set_unit_with_stretch(hs, item_ids)
	} else {
		sides = switch(comp$position$align.v, top = "second", bottom = "first", "both")
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


#' @export
tmapGridCompWidth.tm_legend_standard_portrait = function(comp, o) {

	ni = if (comp$type == "bivariate") attr(comp$gp$fill, "n") else 1L
	mi = if (comp$type == "bivariate") attr(comp$gp$fill, "m") else 1L


	textS = comp$text.size #* o$scale
	titleS = if (comp$title == "") 0 else comp$title.size #* o$scale

	marW = comp$margins[c(2,4)] * textS * o$lin

	width = get_legend_option(comp$item.width, comp$type)

	item_widths = if (comp$type == "symbols") {
		shps = rep(comp$gpar$shape, length.out = comp$nitems)
		pmax(width, rep(comp$gpar$size / textS, length.out = comp$nitems) * ifelse(shps > 999, comp$layer_args$icon.scale, 1))
	} else rep(width, length.out = ni)


	if (comp$type == "bivariate") {
		item_widths_max = max(item_widths)
	} else {
		item_widths_max = max(item_widths)
	}




	tW = ifelse(comp$title == "", 0, graphics::strwidth(comp$title, units = "inch", cex = titleS, family = comp$title.fontfamily, font = fontface2nr(comp$title.fontface)) + sum(comp$title.padding[c(2,4)]) * o$lin * titleS)

	if (comp$type == "bivariate") {
		txtRowW = ifelse(comp$ylab == "", 0, graphics::strwidth(comp$ylab, units = "inch", cex = comp$ylab.size, family = comp$ylab.fontfamily, font = fontface2nr(comp$ylab.fontface)))

		margRowW = ifelse(txtRowW == 0, 0, comp$item_text.margin * comp$ylab.size * o$lin)
	}

	if (comp$type == "bivariate") {
		iW = graphics::strwidth(comp$labels[1:mi], units = "inch", cex = textS, family = comp$text.fontfamily, font = fontface2nr(comp$text.fontface)) + (item_widths_max * ni + comp$item_text.margin) * textS * o$lin
	} else {
		iW = graphics::strwidth(comp$labels, units = "inch", cex = textS, family = comp$text.fontfamily, font = fontface2nr(comp$text.fontface)) + (item_widths_max + comp$item_text.margin) * textS * o$lin
	}

	colW = max(tW, iW)

	if (comp$type == "bivariate") {
		txtW = colW - (item_widths_max * ni + comp$item_text.margin) * textS * o$lin
	} else {
		txtW = colW - (item_widths_max * ni + comp$item_text.margin) * textS * o$lin
	}

	n = switch(comp$position$align.h, left = c(0, 1), right = c(1, 0), c(0.5, 0.5))

	if (comp$type == "bivariate") {
		wsu = grid::unit(c(marW[1],
						   n[1],
						   txtRowW,
						   margRowW,
						   txtW,
						   comp$item_text.margin * textS * o$lin,
						   rep(item_widths_max * textS * o$lin, ni),
						   n[2],
						   marW[2]), units = c("inch", "null", rep("inch", 4+ni), "null", "inch"))
		comp$flexCol = 5 #column that will shrink if the null columns will otherwise be sizes < 0. (happens if legend.width < minimal width)
		Win = if (is.na(comp$width)) sum(as.numeric(wsu)[c(1, 3:(6+ni), 8+ni)]) else comp$width * textS * o$lin

	} else {
		wsu = grid::unit(c(marW[1],
						   n[1],
						   item_widths_max * textS * o$lin,
						   comp$item_text.margin * textS * o$lin,
						   txtW,
						   n[2],
						   marW[2]), units = c("inch", "null", rep("inch", 2+ni), "null", "inch"))
		comp$flexCol = 5 #column that will shrink if the null columns will otherwise be sizes < 0. (happens if legend.width < minimal width)
		Win = if (is.na(comp$width)) sum(as.numeric(wsu)[c(1, 3:5, 7)]) else comp$width * textS * o$lin
	}


	comp$Win = Win
	comp$wsu = wsu

	comp

}

add_user_specified_values = function(gp, usr) {
	if (!length(usr)) return(gp)

	for (v in names(usr)) {
		fun = paste0("tmapValuesCheck_", v)
		val = usr[[v]]
		if (!do.call(fun, list(x = val, is_var = TRUE))) stop("Values assigned to visual variable ", v, " in tm_legend incorrect")

		gp[[v]] = val
	}
	gp
}



#' @export
tmapGridLegPlot.tm_legend_standard_portrait = function(comp, o, fH, fW) {

	icons = all(comp$gp$shape >= 1000)

	# replace gp visual values with user-specified used (e.g. tm_shape(World) + tm_polygons("HPI", fill.legend = tm_legend(col = "red")))
	comp$gp = add_user_specified_values(comp$gp, comp[intersect(names(comp), names(comp$gp))])

	# icons replaced by 'normal' shapes: apply icon.scale
	if (icons && all(comp$gp$shape < 1000) && !is.null(comp$layer_args$icon.scale)) {
		comp$gp$size = comp$gp$size * comp$layer_args$icon.scale
	}

	textS = comp$text.size * comp$scale #* o$scale

	titleS = if (comp$title == "") 0 else comp$title.size * comp$scale #* o$scale

	n = if (comp$type == "bivariate") attr(comp$gp$fill, "n") else 1
	m = if (comp$type == "bivariate") attr(comp$gp$fill, "m") else 1

	nlev = comp$nitems

	wsu = comp$wsu
	hsu = comp$hsu

	vp = grid::viewport(layout = grid::grid.layout(ncol = length(wsu),
												   nrow = length(hsu),
												   widths = wsu,
												   heights = hsu))

	if (is.na(comp$title.align)) comp$title.align = comp$position$align.h

	shiftCol = if (comp$type == "bivariate") 2L else 0L

	titleGP = grid::gpar(col = comp$title.color, cex = titleS, fontface = comp$title.fontface, fontfamily = comp$title.fontfamily, alpha = comp$title.alpha)

	if (comp$title.align == "left") {
		grTitle = gridCell(3, (2 + shiftCol):(length(comp$wsu)-1), grid::textGrob(comp$title, x = grid::unit(comp$title.padding[2] * titleS * o$lin, units = "inch"), just = "left", gp = titleGP))
	} else if (comp$title.align == "right") {
		grTitle = gridCell(3, (2 + shiftCol):(length(comp$wsu)-1), grid::textGrob(comp$title, x = grid::unit(1, "npc") - grid::unit(comp$title.padding[4] * titleS * o$lin, units = "inch"), just = "right", gp = titleGP))
	} else {
		grTitle = gridCell(3, (2 + shiftCol):(length(comp$wsu)-1), grid::textGrob(comp$title, x = 0.5, just = "center", gp = titleGP))
	}


	if (comp$type == "bivariate") {
		if (comp$ylab != "") {
			# ignoring just for now, assuming "left"
			ylabS = comp$ylab.size * comp$scale
			grYlab = gridCell(comp$item_ids[1:m], 3,
							  grid::textGrob(comp$ylab,
							  			   x = grid::unit(comp$ylab.padding[2] * ylabS * o$lin, units = "inch"),
							  			   just = "left",
							  			   gp = grid::gpar(col = comp$ylab.color, cex = ylabS)))
		} else {
			grYlab = NULL
		}

		if (comp$xlab != "") {
			# ignoring just for now, assuming "left"
			xlabS = comp$xlab.size * comp$scale
			grXlab = gridCell(comp$item_ids[m] + 4, 7:(6+n),
							  grid::textGrob(comp$xlab,
							  			   x = grid::unit(comp$xlab.padding[2] * xlabS * o$lin, units = "inch"),
							  			   just = "left",
							  			   gp = grid::gpar(col = comp$xlab.color, cex = xlabS))
							  )
		} else {
			grXlab = NULL
		}

	} else {
		grXlab = NULL
		grYlab = NULL
	}


	if (comp$type == "bivariate") {
		textW = graphics::strwidth(comp$labels[1:m], units = "inch", cex = textS, family = comp$text.fontfamily, font = fontface2nr(comp$text.fontface))
		scale_labels = max(textW / grid::convertUnit(wsu[5], unitTo = "inch", valueOnly = TRUE), 1)
		grText1 = mapply(function(i, id) gridCell(id, 5, grid::textGrob(comp$labels[i], x = 0, just = "left", gp = grid::gpar(col = comp$text.color, cex = textS / scale_labels, fontface = comp$text.fontface, fontfamily = comp$text.fontfamily, alpha = comp$text.alpha))), m:1L, comp$item_ids[1L:m], SIMPLIFY = FALSE)

		colLabels = comp$labels[(m+1L):(n+m)]

		# easy solution: take first letter. Alternatives: rotation, ...
		colLabs = substr(colLabels, 1, 1)
		if (anyDuplicated(colLabs)) {
			colLabs = substr(colLabels, 1, 2)
			if (anyDuplicated(colLabs)) {
				message(paste0("Labels abbreviated by the first two letters, e.g.: \"", colLabels[1], "\" => \"", colLabs[1], "\". However, there are duplicated abbreviations."))
			} else {
				message(paste0("Labels abbreviated by the first two letters, e.g.: \"", colLabels[1], "\" => \"", colLabs[1], "\""))
			}
		} else {
			message(paste0("Labels abbreviated by the first letters, e.g.: \"", colLabels[1], "\" => \"", colLabs[1], "\""))
		}



		grText2 = mapply(function(i, j, id) gridCell(comp$item_ids[m+1], j, grid::textGrob(colLabs[i], x = 0.5, just = "center", gp = grid::gpar(col = comp$text.color, cex = textS / scale_labels, fontface = comp$text.fontface, fontfamily = comp$text.fontfamily, alpha = comp$text.alpha))), 1L:n, 7:(6+n), comp$item_ids[1L:n], SIMPLIFY = FALSE)

		grText = c(grText1, grText2)
	} else {
		textW = graphics::strwidth(comp$labels, units = "inch", cex = textS, family = comp$text.fontfamily, font = fontface2nr(comp$text.fontface))
		scale_labels = max(textW / grid::convertUnit(wsu[5], unitTo = "inch", valueOnly = TRUE), 1)
		grText = mapply(function(i, id) gridCell(id, 5, grid::textGrob(comp$labels[i], x = 0, just = "left", gp = grid::gpar(col = comp$text.color, cex = textS / scale_labels, fontface = comp$text.fontface, fontfamily = comp$text.fontfamily, alpha = comp$text.alpha))), 1L:nlev, comp$item_ids, SIMPLIFY = FALSE)
	}

	ticks = get_legend_option(comp$ticks, comp$type)
	ticks.disable.na = get_legend_option(comp$ticks.disable.na, comp$type)
	if (length(ticks)) {
		tick_col = if (is.na(comp$ticks.col)) comp$text.color else comp$ticks.col
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

		if (!all(ticks_in_margin)) {
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



		nvv = o$continuous.nclass_per_legend_break

		if (o$use_gradient) {
			id1 = which(!is.na(fill_list[[1]]))[1]
			id2 = tail(which(!is.na(fill_list[[nlev2]])), 1)

			y1 = 1 - ((id1-1) / nvv) / nlev2
			y2 = 1 - ((id2 / nvv) / nlev2 + ((nlev2-1)/nlev2))
			h = y1 - y2

			if (vary_fill) {
				cols = unlist(fill_list)[id1:(nvv*(nlev2-1) + id2)]
				cols_alph = paste0(cols, num_to_hex(gp$fill_alpha[1] * 255))
			} else {
				alph = unlist(alpha_list)[id1:(nvv*(nlev2-1) + id2)]
				cols_alph = paste0(col2hex(gp$fill[1]), num_to_hex(alph * 255))
			}
			grItems1 = list(gridCell(comp$item_ids[lvs], 3, grid::rectGrob(y = y2 + 0.5*h, height= h, gp=gpar(fill = grid::linearGradient(colours = rev(cols_alph)), col = NA))))
		} else {
			grItems1 = mapply(function(id, f, a) {
				h = 1 / length(f)
				ys = seq(1-.5*h, by = -h, length.out = length(f))
				#f[!is.na(f)] = "red"
				gpi = grid::gpar(fill = f, alpha = a, col = NA)
				gridCell(id, 3, grid::rectGrob(y = ys, height = h, gp = gpi))
			}, comp$item_ids[lvs], fill_list, alpha_list, SIMPLIFY = FALSE)
		}



		if (vary_fill) {
			y1 = (sum(is.na(fill_list[[1]])) /nvv) / nlev2
			y2 = (sum(is.na(fill_list[[nlev2]])) /nvv) / nlev2
		} else {
			y1 = (sum(is.na(alpha_list[[1]])) /nvv) / nlev2
			y2 = (sum(is.na(alpha_list[[nlev2]])) /nvv) / nlev2
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

		diffAlpha = !anyNA(c(gp$fill_alpha, gp$col_alpha)) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))


		if (diffAlpha) {
			gpars1 = gp_to_gpar(gp, sel = "fill", split_to_n = nlev, o = o, type = comp$type) #lapply(gps, gp_to_gpar_fill)
			gpars2 = gp_to_gpar(gp, sel = "col", split_to_n = nlev, o = o, type = comp$type) #lapply(gps, gp_to_gpar_borders)

			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, rndrectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			grItems = mapply(function(id, gpar1i, gpar2i) gridCell(id, 3, {
				grid::grobTree(
					rndrectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar1i, r = comp$item.r),
					rndrectGrob(width = grid::unit(o$lin* textS, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = gpar2i, r = comp$item.r))
			}), comp$item_ids, gpars1, gpars2, SIMPLIFY = FALSE)

		} else {
			gpars = gp_to_gpar(gp, sel = "all", split_to_n = nlev, o = o, type = comp$type)#lapply(gps, gp_to_gpar)
			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
			grItems = mapply(function(id, gpari) gridCell(id, 3, rndrectGrob(gp = gpari, r = comp$item.r)), comp$item_ids, gpars, SIMPLIFY = FALSE)
		}
	} else if (comp$type == "lines") {
		#gps = split_gp(gp, n = nlev)

		gpars = gp_to_gpar(gp, sel = "col", split_to_n = nlev, o = o, type = comp$type)#lapply(gps, gp_to_gpar)
			#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)
		grItems = mapply(function(id, gpari) gridCell(id, 3, grid::linesGrob(y = grid::unit(c(0.5,0.5), "npc"), gp = gpari)), comp$item_ids, gpars, SIMPLIFY = FALSE)


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
#browser()
		grItems = mapply(function(id, gpari, gpari1, gpari2) {
			grobs = if (gpari$shape > 999) {
				grbs = gList(shapeLib[[gpari$shape-999]])
				grid::gTree(children=grbs, vp=viewport(x=0.5,
												 y=0.5,
												 width=unit(gpari$size*9/10 * comp$layer_args$icon.scale, "lines"),
												 height=unit(gpari$size*9/10 * comp$layer_args$icon.scale, "lines")))
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
			gridCell(id, 3, grobs)
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
			gridCell(id, 3, {
				grid::gList(
					rndrectGrob(width = grid::unit(grid::convertWidth(grid::stringWidth(txt), "inches")* size, "inches"), height = grid::unit(o$lin* textS, "inches"), gp = grid::gpar(fill = bgcol, alpha = bgcol_alpha, col = NA), r = comp$item.r),
					grid::textGrob(x=0.5, y=0.5, label = txt, gp = gpari))
			})
		}, comp$item_ids, gpars, gp$text, bgcols, bgcols_alpha, gp$cex, SIMPLIFY = FALSE)

	} else if (comp$type == "bivariate") {
		gpars = gp_to_gpar(gp, sel = "all", split_to_n = n*m, o = o, type = comp$type)#lapply(gps, gp_to_gpar)
		#grItems = mapply(function(i, gpari) gridCell(i+3, 2, grid::rectGrob(gp = gpari)), 1:nlev, gpars, SIMPLIFY = FALSE)

		ind = expand.grid(row = comp$item_ids[m:1], col = 7:(6+n))

		grItems = mapply(function(i, j, gpari) gridCell(i, j, rndrectGrob(gp = gpari, r = comp$item.r)), ind$row, ind$col, gpars, SIMPLIFY = FALSE)
	}


	g = do.call(grid::grobTree, c(list(grTitle, grXlab, grYlab), grText, grItems, grTicks, grDesign, list(vp = vp)))

	g

}

