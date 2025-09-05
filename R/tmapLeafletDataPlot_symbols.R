#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_symbols = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt[!is.na(dt$size) & !is.na(dt$shape), ])
	shp = res$shp
	if (o$crs_leaflet$crsClass  == "L.CRS.Simple") {
		shp = sf::st_set_crs(shp, NA)
	}
	dt = res$dt

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	coords = sf::st_coordinates(shp)

	# in case shp is a multipoint (point_per == "segment"), expand gp:
	cp = expand_coords_gp(coords, gp, ndt = nrow(dt))
	coords = cp$coords
	gp = cp$gp

	if (cp$expanded) {
		shpTM_match = match(shpTM$tmapID_expanded, shpTM$tmapID)
	} else {
		shpTM_match = TRUE
	}

	if (!is.null(idt)) {
		idt = idt$id[match(dt$tmapID__[shpTM_match], idt$tmapID__)]
	} else {
		idt = sprintf("%07d", dt$tmapID__)[shpTM_match]
	}
	if (!is.null(hdt)) {
		hdt = hdt$hover[match(dt$tmapID__[shpTM_match], hdt$tmapID__)]
		hdt = lapply(hdt, htmltools::HTML, FUN.VALUE = character(1))
	}
	if (is.null(pdt)) {
		popups = NULL
	} else {
		mtch = match(dt$tmapID__[shpTM_match], pdt$tmapID__)
		pdt = pdt[mtch][, tmapID__ := NULL]

		popups = view_format_popups(id = idt, titles = names(pdt), values = pdt, format = popup.format)
	}


	submit_labels(idt, "symbols", pane, group)

	interactive = (!is.null(pdt) || !is.null(hdt))

	opt = leaflet::pathOptions(interactive = interactive, pane = pane)

	o$use_WebGL = impute_webgl(o$use_WebGL, dt, supported = c("fill", "size"), checkif = list(shape = c(1,16,19,20,21)), type = "symbols", hover = !is.null(hdt), popup = !is.null(pdt), crs_class = o$crs_leaflet$crsClass)

	use_circleMarkers = o$use_circle_markers && all(gp$shape %in% c(1, 16, 19, 20, 21))


	gp2 = gp_to_lpar(gp, mfun = "Symbols", size_factor = 14) # 14 based on similarity with plot mode and consistency with tmap3
	#gp = gp2leafgp(gp)
	names(gp2)[names(gp2) == 'stroke-width'] = "strokeWidth"
	gp2$baseSize = 20

	#po(sort(gp2$width, decreasing = T))

	shp2 = sf::st_as_sf(as.data.frame(coords), coords = c("X", "Y"), crs = st_crs(shp))

	if (o$use_WebGL) {
		lf |>  leafgl::addGlPoints(shp2, fillColor = gp2$fillColor, radius = gp2$width, fillOpacity = gp2$fillOpacity[1], pane = pane, group = group, label = hdt, popup = popups) %>%
			assign_lf(facet_row, facet_col, facet_page)
	} else if (use_circleMarkers) {
		gp2$strokeWidth[gp2$shape %in% c("solid-circle-md", "solid-circle-bg", "solid-circle-sm")] = 0
		gp2$fillOpacity[gp2$shape %in% "open-circle"] = 0
		multiplier = ifelse(gp2$shape == "solid-circle-md", 0.28, ifelse(gp2$shape == "solid-circle-sm", 0.25, 0.5)) # manually calibrated with 4k screen

		lf |>  leaflet::addCircleMarkers(data = shp2, layerId = idt,
										 color = gp2$color,
										 opacity = gp2$opacity,
										 weight = gp2$strokeWidth,
										 dashArray = gp2[["stroke-dasharray"]],
										 fillColor = gp2$fillColor,
										 fillOpacity = gp2$fillOpacity,
										 radius = gp2$width * multiplier,
										 options = opt,
										 group = group, label = hdt, popup = popups) |>
			assign_lf(facet_row, facet_col, facet_page)

	} else {

		sn = suppressWarnings(as.numeric(gp2$shape))

		is_num = !is.na(sn)
		sid = which(is_num)
		nid = which(!is_num)


		# ="circle" to make makeSymbolsIcons2 work
		# shape_orig to let unique pick unique rows (one for each )
		gp2$shape_orig = gp2$shape
		gp2$shape[sid] = "circle"

		# faster than symbols2 = do.call(makeSymbolIcons2, gp2)
		gp2df = as.data.table(gp2)
		gp2dfU = unique(gp2df)

		k = nrow(gp2dfU)

		symbols = do.call(makeSymbolIcons2, as.list(gp2dfU[,-(ncol(gp2dfU)),with=F]))

		gp2dfU[, id:=1L:.N]
		gp2join = gp2df[gp2dfU, id:= id, on=names(gp2df)]
		ids = gp2join$id

		coords_grps = split.data.frame(coords, ids)
		idt_grps = split(idt, ids)
		if (!is.null(hdt)) {
			hdt_grps = split(hdt, ids)
		} else {
			hdt_grps = replicate(k, list(NULL))
		}

		if (is.null(popups)) {
			popups_grps =  rep(list(NULL), k)
		} else {
			popups_grps = split(popups, ids)
		}


		symbols$iconWidth = rep(NA, k)
		symbols$iconHeight = rep(NA, k)


		if (length(sid)) {
			sym_shapes = suppressWarnings(as.numeric(gp2dfU$shape_orig))
			sid2 = which(!is.na(sym_shapes))

			iconLib <- get("shapeLib", envir = .TMAP)[sym_shapes[sid2]-999]
			symbols_icons <- merge_icons(iconLib)

			size = gp2dfU$width[sid2] / gp2dfU$baseSize[sid2]
			size = size * a$icon.scale/3

			for (i in seq_along(sid2)) {
				symbols$iconUrl[sid2[i]] = symbols_icons$iconUrl[i]
				symbols$iconWidth[sid2[i]] <- symbols_icons$iconWidth[i] * size[i]
				symbols$iconHeight[sid2[i]] <- symbols_icons$iconHeight[i] * size[i]
				if (all(c("iconAnchorX", "iconAnchorY") %in% names(symbols_icons))) {
					symbols$iconAnchorX[sid2[i]] <- symbols_icons$iconAnchorX[i] * size[i]
					symbols$iconAnchorY[sid2[i]] <- symbols_icons$iconAnchorY[i] * size[i]

				}
			}
		}

		for (i in 1L:k) {
			opt$zIndexOffset = i * 1e5
			.TMAP_LEAFLET$markerLayers = c(.TMAP_LEAFLET$markerLayers, opt$pane) # register for additional css
			lf = lf |>
				leaflet::addMarkers(lng = coords_grps[[i]][, 1],
									lat = coords_grps[[i]][, 2],
									icon = lapply(symbols, "[", i),
									group = group,
									layerId = idt_grps[[i]],
									label = hdt_grps[[i]],
									popup = popups_grps[[i]],
									options = opt
				)
		}
		lf |> assign_lf(facet_row, facet_col, facet_page)

	}


	# if (o$use_WebGL) {
	# 	lf %>%
	# 		leafgl::addGlPoints(sf::st_sf(shp), fillColor = gp$fill, radius = gp$size*10, fillOpacity = gp$fill_alpha, color = gp$col, opacity = gp$color_alpha, weight = gp$lwd, pane = pane, group = group) %>%
	# 		assign_lf(facet_row, facet_col, facet_page)
	# } else {
	# 	lf %>%
	# 		leaflet::addCircleMarkers(lng = coords[, 1], lat = coords[, 2], fillColor = gp$fill, radius = gp$size*4, fillOpacity = gp$fill_alpha, color = gp$col, opacity = gp$color_alpha, weight = gp$lwd, group = group, options = opt) %>%
	# 		assign_lf(facet_row, facet_col, facet_page)
	# }

	NULL
}


#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_dots = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_bubbles = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_squares = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_markers = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}
