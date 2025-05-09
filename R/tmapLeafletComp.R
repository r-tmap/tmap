leaflet_pos = function(pos) {
	if (pos$type %in% c("out", "autoout")) {
		sel = c("cell.v", "cell.h")
	} else {
		sel = c("pos.v", "pos.h")
	}
	x = tolower(unlist(pos[sel]))

	if (x[1] %in% c("center", "centre")) x[1] = "top"
	if (x[2] %in% c("center", "centre")) x[2] = "left"

  paste(x, collapse = "")
}


gp_to_lpar = function(gp, mfun, shape = 20, pick_middle = TRUE, size_factor = 20, rename_prop = TRUE) {
	# create a list of gp elements

	lst = c(list(fill = {if (!all(is.na(gp$fill))) gp$fill else "#000000"},
				 col = {if (!all(is.na(gp$col))) gp$col else "#000000"},
				 fill_alpha = {if (!all(is.na(gp$fill_alpha))) gp$fill_alpha else 0},
				 col_alpha = {if (!all(is.na(gp$col_alpha))) gp$col_alpha else 0},
				 lwd = {if (!all(is.na(gp$lwd))) gp$lwd else 0},
				 lty = {if (!all(is.na(gp$lty))) lty2dash(gp$lty) else "none"},
				 size = {if (!all(is.na(gp$size))) gp$size else 1},
				 shape = {if (!all(is.na(gp$shape))) gp$shape else shape}))

	lst_isnum = c(fill = FALSE,
				  col = FALSE,
				  fill_alpha = TRUE,
				  col_alpha = TRUE,
				  lwd = TRUE,
				  lty = FALSE,
				  size = TRUE,
				  shape = TRUE)

	if (rename_prop) {
		newNames = c("fillColor",
					  "color",
					  "fillOpacity",
					  "opacity",
					  "stroke-width",
					  "stroke-dash",
					  "size",
					  "shape")
		names(lst) = newNames
		names(lst_isnum) = newNames
	}


	lst = mapply(function(lsti, nm, isnum) {
		if (!is.character(lsti)) return(lsti)

		if (nchar(lsti[1]) > 50) {
			x = cont_split(lsti)
			x = lapply(x, function(i) {
				i[i=="NA"] <- NA
				i
			})
			if (isnum) x = lapply(x, as.numeric)
			if (nm %in% c("fillColor", "color")) {
				# create ramp of 10
				not1 = which(vapply(x, length, FUN.VALUE = integer(1)) != 1L)
				if (length(not1) != length(x)) {
					# add NA
					xNA = x[[length(x)]]
				} else {
					xNA = NULL
				}
				x[not1[-1]] = lapply(x[not1[-1]], tail, -1)
				x = na.omit(unlist(x[not1]))
				x = c(x[seq(1, length(x), length.out = length(not1) * 2 + 1)], xNA)
			} else if (pick_middle) {
				x = sapply(x, function(i) {
					if (all(is.na(i))) NA else {
						sq = c(5,6,4,7,3,8,2,9,1,10) # priority for middle values
						i[sq[which(!is.na(i)[sq])[1]]]
					}
				})
			}
			return(x)

		} else {
			return(lsti)
		}
	}, lst, names(lst), lst_isnum[names(lst)], SIMPLIFY = FALSE)

	pch2shp = c("rect", "circle", "triangle", "plus", "cross", "diamond", "triangle",
				"cross", "star", "diamond", "circle", "polygon", "plus", "cross",
				"triangle", "rect", "circle", "triangle", "diamond", "circle",
				"circle", "circle", "rect", "diamond", "triangle", "polygon", "stadium") # shapes for pch 0:25 + 26 for stadium (NOTE: last one is a triangle upside-down. Since 21:25 are the defaults, and a polygon is chosen to differentiate from the other triangle)
	lst$shape = get_pch_names(lst$shape)

	if (mfun == "Lines") lst$shape = "line"

	lst$width = lst$size * size_factor
	lst$height = lst$size * size_factor
	#lst$width[]
	lst$size = NULL
	lst
}



make_equal_list = function(x) {
	cls = class(x)
	n = max(vapply(x, length, integer(1)))
	structure(lapply(x, rep, length.out = n), class = cls)
}


tmapLeaflet_legend = function(cmp, lf, o, orientation) {
	group = cmp$group
	leg_className = paste("info legend", gsub(" ", "", group, fixed = TRUE))



	if ("layerId" %in% names(cmp)) {
		layerId = cmp$layerId
		group = "always_on"
	} else {
		layerId =  paste0("legend", sprintf("%02d", .TMAP_LEAFLET$leg_id)) # "legend401" #todo
		.TMAP_LEAFLET$leg_id = .TMAP_LEAFLET$leg_id + 1
	}



	lab = cmp$labels
	val = c(cmp$dvalues)
	title = if (nonempty_text(cmp$title)) expr_to_char(cmp$title) else NULL

	if (!is.null(title) && !is.null(cmp$title.color)) {

		if (cmp$type == "gradient") {
			title = htmltools::tags$div(
				title,
				style = paste0('color: ', cmp$title.color, ';')
			)
		} else {
			title = htmltools::tags$div(
				title,
				style = paste0('font-size: 14px; text-align: left; margin-bottom: 5px; color: ', cmp$title.color, ';')
			)
		}

	}

	legpos = leaflet_pos(cmp$position)

	lf2 = if (cmp$type == "none") {
		#message("Text based legends not supported in view mode")
		lf
	} else if (cmp$type == "gradient") {
		nbins = length(val)



		incl.na = cmp$na.show
		if (incl.na) {
			sel = head(cmp$labels_select, -1)
		} else {
			sel = cmp$labels_select
		}

		bins = val[sel]
		val = val[sel]

		if (!head(sel, 1)) {
			val = c(cmp$limits[1], val)
		} else {
			val = c(val[1] - (val[2] - val[1]) * 0.5, val)
		}

		if (!tail(sel, 1)) {
			val = c(val, cmp$limits[2])
		} else {
			val = c(val, val[length(val)] + diff(tail(val, 2))/2)
		}

		vary = if ("fill" %in% cmp$varying) "fillColor" else "color"
		#vary_alpha = paste0(vary, "_alpha")

		incl.na = cmp$na.show
		if (incl.na) {
			pal = head(cmp$gp2[[vary]], -1)
			colNA = tail(cmp$gp2[[vary]], 1)
			textNA = lab[length(lab)]
			labs = head(lab, -1)[sel]
			val = c(val, NA)
			#labs = c(labs, "")
		} else {
			pal = cmp$gp2[[vary]]
			colNA = NA
			textNA = NA
			labs = lab[sel]
		}
		pal = colorNumeric(palette = pal,
						   domain = val,
						   na.color=colNA,
						   alpha = FALSE)

		brks = pretty(cmp$limits, 7)

		trns = function(x) {
			f = (x-cmp$limits[1]) / diff(cmp$limits)
			y = cmp$tr$fun(cmp$limits)
			x2 = y[1] + diff(y) * f
			cmp$tr$rev(x2)
		}

		opacity = if (vary == "fillColor") {
			cmp$gp2$fillOpacity
		} else {
			cmp$gp2$opacity
		}

		if (orientation == "horizontal") {
			cli::cli_inform("{.field [landscape legend in view mode]} doesn't support labels yet",
							.frequency_id = "landscape_legend_view",
							.frequency = "once")
			labs = NULL

		}

		lf %>% leaflegend::addLegendNumeric(position=legpos,
										   orientation = orientation,
										   group = group,
										   height = cmp$height,
										   width = cmp$width,
										   pal=pal,
										   values=val,
										   # numberFormat = function(x) {
										   # 	prettyNum(trns(x), format = "f", big.mark = ",", digits =
										   # 			  	3, scientific = FALSE)
										   # },
										   labels = labs,
										   bins = bins,
										   naLabel = textNA,
										   title=title,
										   fillOpacity=opacity,
										   layerId = layerId,
										   className = leg_className)

	} else {
		vary = if ("fill" %in% cmp$varying) "fill" else if ("col" %in% cmp$varying) "col" else NA



		gp2 = make_equal_list(cmp$gp2)
		#gp2$baseSize = gp2$baseSize[1]

		sn = suppressWarnings(as.numeric(gp2$shape))
		sid = which(!is.na(sn))

		#symbols = do.call(Map, c(list(f = leaflegend::makeSymbol), cmp$gp2))

		# alternative:
		if (length(sid)) {
			gp2$shape[sid] = "circle" # as dummy
		}

		names(gp2)[names(gp2) == 'stroke-width'] = "strokeWidth"
		gp2$baseSize = 20
		#symbols = do.call(leaflegend::makeSymbolIcons, gp2)#$iconUrl


		symbols = do.call(makeSymbolIcons2, gp2)
		#po(sort(gp2$width, decreasing = T))

		symbols$iconWidth = gp2$width#, length(symbols$iconUrl))
		symbols$iconHeight = gp2$height#, length(symbols$iconUrl))

		if (length(sid)) {
			iconLib <- get("shapeLib", envir = .TMAP)[sn[sid]-999]
			symbols_icons <- merge_icons(iconLib)

			size = gp2$width[sid] / gp2$baseSize * cmp$layer_args$icon.scale/3

			for (i in seq_along(sid)) {
				symbols$iconUrl[sid[i]] = symbols_icons$iconUrl[i]
				symbols$iconWidth[sid[i]] <- symbols_icons$iconWidth[i] * size[i]
				symbols$iconHeight[sid[i]] <- symbols_icons$iconHeight[i] * size[i]
				if (all(c("iconAnchorX", "iconAnchorY") %in% names(symbols_icons))) {
					symbols$iconAnchorX[sid[i]] <- symbols_icons$iconAnchorX[i] * size[i]
					symbols$iconAnchorY[sid[i]] <- symbols_icons$iconAnchorY[i] * size[i]

				}
			}
		}

		#symbols = symbols$iconUrl


		lf %>% leaflegend::addLegendImage(symbols$iconUrl,
										  group = group,
										 labels = lab,
										 width = symbols$iconWidth + 2*gp2$strokeWidth,
										 height = symbols$iconHeight + 2*gp2$strokeWidth,
										 position = legpos,
										 orientation = orientation,
										 labelStyle = "font-size: 14px; vertical-align: middle; margin: 0px;",
										 title = title,
										 layerId = layerId,
										 className = leg_className)
	}
	backg <- htmltools::tags$style(paste0("#", layerId, " { background: ", substr(cmp$bg.color,1,7), "; opacity: ", cmp$bg.alpha, "}"))
	if (!.TMAP$in.shiny) {
		htmlwidgets::prependContent(lf2, backg)
	} else {
		lf2
	}
}

#' @export
tmapLeafletLegPlot.tm_legend_standard_portrait = function(comp, lf, o) {
	tmapLeaflet_legend(comp, lf, o, orientation = "vertical")
}

#' @export
tmapLeafletLegPlot.tm_legend_standard_landscape = function(comp, lf, o) {
	tmapLeaflet_legend(comp, lf, o, orientation = "horizontal")
}


tmapLeafletComp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)


	for (cmp in comp) {
		lf = tmapLeafletLegPlot(cmp, lf, o)
	}



	assign_lf(lf, facet_row, facet_col, facet_page)
	NULL
}
