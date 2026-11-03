leaflet_pos = function(pos, collapse = "") {
	if (pos$type %in% c("out", "autoout")) {
		sel = c("cell.v", "cell.h")
	} else {
		sel = c("pos.v", "pos.h")
	}
	x = tolower(unlist(pos[sel]))

	if (x[1] %in% c("center", "centre")) x[1] = "top"
	if (x[2] %in% c("center", "centre")) x[2] = "left"

  paste(x, collapse = collapse)
}


gp_to_lpar = function(gp, mfun, shape = 20, pick_middle = TRUE, size_factor = 20, rename_prop = TRUE) {
	# create a list of gp elements

	lst = c(list(fill = {if (!all(is.na(gp$fill))) gp$fill else "#000000"},
				 col = {if (!all(is.na(gp$col))) gp$col else "#000000"},
				 fill_alpha = {if (!all(is.na(gp$fill_alpha))) gp$fill_alpha else 0},
				 col_alpha = {if (!all(is.na(gp$col_alpha))) gp$col_alpha else 0},
				 lwd = {if (!all(is.na(gp$lwd))) gp$lwd else 0},
				 lty = {if (!all(is.na(gp$lty))) lty2dash(gp$lty, gp$lwd) else "none"},
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

	if ("tm_data_lines" %in% mfun) lst$shape = "line"

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
		breaks_show =  ("is_discrete" %in% names(cmp)) && cmp$is_discrete


		nbins = length(val)

		incl.na = cmp$na.show

		if (incl.na) {
			sel = head(cmp$labels_select, -1)
		} else {
			sel = cmp$labels_select
		}

		bins = val[sel]
		val = val[sel]

		if (!breaks_show) {
			if (cmp$scale == "intervals") cli::cli_warn("{.field tm_scale_intervals} {.code label.style = {.str continuous}} implementation in view mode work in progress")
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
		} else {
			brks = val
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

		pfun = function(x) {
			r = brks[c(1, length(brks))]
			d = (x * (r[2] - r[1])) + r[1]
			ids = cut(d, breaks = brks, include.lowest = TRUE, right = TRUE, labels = FALSE)
			pal2[ids]
		}
		# pfun = function(x) {
		# 	ifelse(x < 0.5, "blue", "red")
		# }

		if (breaks_show) {
			# when labels_cutpoints is enabled in tm_scale_intervals
			pal2 = unname(head(pal, -1)[-1][seq(2L, length(pal)-2L, by = 2L)])
			pal = colorNumeric(palette = pfun,
							   domain = val,
							   na.color=colNA,
							   alpha = FALSE)
		} else {
			pal = colorNumeric(palette = pal,
							   domain = val,
							   na.color=colNA,
							   alpha = FALSE)
		}


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

	} else if (cmp$type == "pattern") {
		# legend items are PNG images of the swatches: fill, pattern, and border
		n = length(lab)
		gpvars = c("fill", "fill_alpha", "col", "col_alpha", "lwd",
				   "pattern", "pattern_col", "pattern_size", "pattern_density")
		gp = lapply(cmp$gp[gpvars], function(x) {
			rep_len(if (is.null(x) || all(is.na(x))) NA else x, n)
		})
		px = 24
		images = vapply(seq_len(n), function(i) {
			pattern_swatch_png(gp$pattern[i], density = gp$pattern_density[i], size = gp$pattern_size[i],
							   col = gp$pattern_col[i], fill = gp$fill[i], fill_alpha = gp$fill_alpha[i],
							   border = gp$col[i], border_alpha = gp$col_alpha[i], lwd = gp$lwd[i],
							   scale = o$scale, px = px)
		}, character(1))
		# with pattern.zoom, the patterns of the swatches zoom with the map: the PNG images are
		# replaced by inline SVG swatches that use zooming SVG patterns (see leaflet_add_pattern_defs)
		zpat = if (isTRUE(cmp$layer_args$pattern.zoom)) {
			gpz = list(pattern = gp$pattern,
					   pattern_col = ifelse(is.na(gp$pattern_col), "grey25", gp$pattern_col),
					   pattern_size = ifelse(is.na(gp$pattern_size), 1, gp$pattern_size),
					   pattern_density = ifelse(is.na(gp$pattern_density), 0.8, gp$pattern_density))
			suffix = substr(rlang::hash(list(Sys.time(), layerId, lab)), 1, 8)
			leaflet_pattern_ids(gpz, n, o, zoom = TRUE, suffix = suffix)
		}
		cls = if (is.null(zpat)) leg_className else paste(leg_className, paste0("tmap-pattern-legend-", suffix))
		lf = lf %>% leaflegend::addLegendImage(images,
										  group = group,
										  labels = lab,
										  width = px,
										  height = px,
										  position = legpos,
										  orientation = orientation,
										  labelStyle = "font-size: 14px; vertical-align: middle; margin: 0px;",
										  title = title,
										  layerId = layerId,
										  className = cls)
		if (!is.null(zpat)) {
			lf = lf %>%
				leaflet_add_pattern_defs(zpat$defs, zoom = unique(stats::na.omit(zpat$ids))) %>%
				leaflet_pattern_legend_swatches(paste0("tmap-pattern-legend-", suffix), ids = zpat$ids,
												gp = gp, px = px)
		}
		lf
	} else if (cmp$type == "text") {
		# Opus5.5: text legend items (e.g. for tm_text(size = <var>)), drawn as svg text symbols (#1226)
		n = length(lab)
		gp = lapply(cmp$gp[c("text", "cex", "col", "col_alpha", "fontfamily")], rep_len, length.out = n)

		cex = ifelse(is.na(gp$cex), 1, gp$cex)
		col = ifelse(is.na(gp$col), "#000000", substr(gp$col, 1, 7))
		alpha = ifelse(is.na(gp$col_alpha), 1, gp$col_alpha)
		fontfamily = if (is.na(gp$fontfamily[1])) NULL else gp$fontfamily[1]

		fontSize = round(cex * 12) # same as in tmapLeafletDataPlot.tm_data_text
		widths = ceiling(fontSize * 0.6 * nchar(gp$text)) + 2
		heights = ceiling(fontSize * 1.2) + 2

		symbols = lapply(seq_len(n), function(i) {
			leaflegend::makeSymbolText(text = gp$text[i], width = widths[i], height = heights[i],
									   color = col[i], fillColor = col[i], opacity = alpha[i], fillOpacity = alpha[i],
									   fontSize = fontSize[i], fontFamily = fontfamily)
		})

		lf %>% leaflegend::addLegendImage(symbols,
										  group = group,
										  labels = lab,
										  width = widths,
										  height = heights,
										  position = legpos,
										  orientation = orientation,
										  labelStyle = "font-size: 14px; vertical-align: middle; margin: 0px;",
										  title = title,
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
		names(gp2)[names(gp2) == 'stroke-dash'] = "dashArray"
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
										 width = symbols$iconWidth + max(2*gp2$strokeWidth),
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
tmapLeafletCompPlot.tm_legend_portrait = function(comp, lf, o) {
	tmapLeaflet_legend(comp, lf, o, orientation = "vertical")
}

#' @export
tmapLeafletCompPlot.tm_legend_landscape = function(comp, lf, o) {
	tmapLeaflet_legend(comp, lf, o, orientation = "horizontal")
}


tmapLeafletComp = function(comp, o, facet_row = NULL, facet_col = NULL, facet_page, class, stack, stack_auto, pos.h, pos.v, bbox) {
	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	# grp = tmapGetCompGroupArgs(comp) none of the tm_components args are used yet in view mode

	for (cmp in comp) {
		lf = tmapLeafletCompPlot(cmp, lf, o)
	}



	assign_lf(lf, facet_row, facet_col, facet_page)
	NULL
}


# Replace the PNG swatches of a pattern legend (the legend control with class cls) by inline SVG
# swatches: fill, pattern (fill = "url(#id)", so that zooming patterns are updated), and border
leaflet_pattern_legend_swatches = function(lf, cls, ids, gp, px) {
	hex = function(x) {
		ifelse(is.na(x), "none", substr(grDevices::rgb(t(grDevices::col2rgb(x)), maxColorValue = 255), 1, 7))
	}
	num = function(x, d) ifelse(is.na(x), d, x)
	items = lapply(seq_along(ids), function(i) {
		list(id = if (is.na(ids[i])) "" else ids[i],
			 fill = hex(gp$fill[i]), fill_alpha = num(gp$fill_alpha[i], 1),
			 col = hex(gp$col[i]), col_alpha = num(gp$col_alpha[i], 1), lwd = num(gp$lwd[i], 1))
	})
	htmlwidgets::onRender(lf, "function(el, x, data) {
		var leg = el.querySelector('.' + data.cls);
		if (!leg) return;
		var imgs = leg.querySelectorAll('img');
		var ns = 'http://www.w3.org/2000/svg';
		var rect = function(attrs) {
			var r = document.createElementNS(ns, 'rect');
			for (var k in attrs) r.setAttribute(k, attrs[k]);
			return r;
		};
		[].concat(data.items).forEach(function(it, i) {
			var img = imgs[i];
			if (!img) return;
			var s = data.px, w = it.lwd;
			var svg = document.createElementNS(ns, 'svg');
			svg.setAttribute('width', s);
			svg.setAttribute('height', s);
			svg.setAttribute('style', img.getAttribute('style') || '');
			var box = {x: w / 2, y: w / 2, width: s - w, height: s - w};
			svg.appendChild(rect(Object.assign({fill: it.fill, 'fill-opacity': it.fill_alpha}, box)));
			if (it.id) svg.appendChild(rect(Object.assign({fill: 'url(#' + it.id + ')'}, box)));
			svg.appendChild(rect(Object.assign({fill: 'none', stroke: it.col, 'stroke-opacity': it.col_alpha,
				'stroke-width': w}, box)));
			img.parentNode.replaceChild(svg, img);
		});
	}", data = list(cls = cls, items = items, px = px))
}
