submit_labels = function(labels, cls, pane, group) {
	layerIds = get("layerIds", envir = .TMAP_LEAFLET)

	if (length(layerIds)) {
		labels = local({
			labels_all = unlist(lapply(layerIds, function(l) l$Lid), use.names = FALSE)
			pos <- length(labels_all)
			labels_all = gsub("_", ".", labels_all,fixed = TRUE)
			labels_all = make.names(c(labels_all, labels), unique = TRUE)
			labels_all = gsub(".", "_", labels_all,fixed = TRUE)
			labels_all[(pos + 1): length(labels_all)]
		})
	} else {
		labels = make.names(labels, unique = TRUE)
		labels = gsub(".", "_", labels,fixed = TRUE)
	}

	layerIds = c(layerIds, list(list(name = pane, type = cls, group = group, Lid = labels)))

	assign("layerIds", layerIds, envir = .TMAP_LEAFLET)
	labels
}

impute_webgl = function(use_WebGL, dt, supported, checkif = NULL, type, hover, popup, crs_class) {
	if (!identical(use_WebGL, FALSE)) {
		if (crs_class == "L.CRS.Simple") {
			message_webgl_crs_simple()
			use_WebGL = FALSE
		} else {

			vary = vapply(dt, function(x) {
				any(is.na(x)) || any(x!=x[1])
			}, FUN.VALUE = logical(1))

			vary = vary[setdiff(names(vary), c(supported, "tmapID__", "ord__"))]

			if (!is.null(checkif)) {
				checks = vapply(seq_along(checkif), function(i) {
					if (!(checkif[i] %in% names(dt))) {
						TRUE
					} else {
						dt[[names(checkif)[i]]][1] %in% checkif[[i]]
					}
				}, FUN.VALUE = logical(1))
			} else {
				checks = TRUE
			}

			if (any(vary)) {
				if (is.na(use_WebGL)) {
					use_WebGL = FALSE
				} else {
					message_webgl_vars(supported, vary)
				}
			} else if (!all(checks)) {
				if (is.na(use_WebGL)) {
					use_WebGL = FALSE
				} else {
					checkif = lapply(checkif, function(x) {
						if (is.character(x)) paste0("\"", x, "\"") else x
					})

					checkif = lapply(checkif, function(x) {
						if (length(x) == 1) {
							x
						} else {
							k = length(x)
							paste(paste(head(x, -1), collapse = ", "), tail(x, 1), sep = " or ")
						}
					})

					message_webgl_checks(checks, checkif)

				}
			} else if (type != "symbols" && hover) {
				if (!is.na(use_WebGL)) {
					message_webgl_hover(type)
				} else {
					use_WebGL = FALSE
				}
			} else if ((is.na(use_WebGL))) {
				n = nrow(dt)
				use_WebGL = (n >= 500)
			}
		}
	}
	use_WebGL
}


expand_coords_gp = function(coords, gp, ndt) {
	expanded = (ncol(coords) == 3L)
	if  (expanded) {
		gp = lapply(gp, function(gpi) {
			if (is.list(gpi)) {
				unlist(gpi)
			} else if (length(gpi) == ndt) {
				gpi[coords[,3L]]
			} else {
				gpi
			}
		})
		coords = coords[, 1:2, drop=FALSE]
	}
	list(coords = coords, gp = gp, expanded = expanded)
}

#' @export
#' @rdname tmap_internal
tmapLeafletPolygons = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	if (o$crs_leaflet$crsClass  == "L.CRS.Simple") {
		shp = sf::st_set_crs(shp, NA)
	}

	dt = res$dt

	if (!is.null(idt)) {
		idt = idt$id[match(dt$tmapID__, idt$tmapID__)]
	}
	if (!is.null(hdt)) {
		hdt = hdt$hover[match(dt$tmapID__, hdt$tmapID__)]
	}

	if (is.null(pdt)) {
		popups = NULL
	} else {
		pdt = pdt[match(dt$tmapID__, pdt$tmapID__)][, tmapID__ := NULL]
		if (is.null(idt) && !is.null(hdt)) {
			popups = view_format_popups(id = hdt, titles = names(pdt), values = pdt, format = popup.format)
		} else {
			popups = view_format_popups(id = idt, titles = names(pdt), values = pdt, format = popup.format)
		}
	}


	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)
	gp = gp_to_lpar(gp, mfun = "Polygons", rename_prop = FALSE)

	interactive = (!is.null(pdt) || !is.null(hdt))

	opt = leaflet::pathOptions(interactive = interactive, pane = pane)

	idt = (if (is.null(idt))dt$tmapID__ else idt) |>
		submit_labels("polygons", pane, group)


	o$use_WebGL = impute_webgl(o$use_WebGL, dt, supported = c("fill", "col"), checkif = list(lty = "solid"), type = "polygons", hover = !is.null(hdt), popup = !is.null(pdt), crs_class = o$crs_leaflet$crsClass)

	if (o$use_WebGL) {
		shp2 = sf::st_sf(id = seq_along(shp), geom = shp)
		shp3 = sf_expand(shp2)

		shp3lines = suppressWarnings(sf::st_cast(shp3, "LINESTRING"))
		gp3 = lapply(gp, function(gpi) {if (length(gpi) == 1) gpi else gpi[shp3$split__id]})
		popups2 = popups[shp3$split__id]

		# opacity channel from fill (e.g. "#FF000099") is ignored by addGlPolygons
		fill_alpha = split_alpha_channel(gp3$fill[1], alpha = gp3$fill_alpha[1])$opacity

		lf |>
			leafgl::addGlPolygons(data = shp3, layerId = idt, label = hdt,
								  fillColor = gp3$fill, fillOpacity = fill_alpha,
								  group = group, pane = pane, popup = popups2) %>%
			{if (gp3$lwd[1]!=0 && gp3$col[1] != "#00000000") leafgl::addGlPolylines(., data = shp3lines, color = gp3$col, opacity = gp3$col_alpha[1], weight = gp3$lwd[1]/4, pane = pane, group = group, layerId = idt) else .} %>%
			assign_lf(facet_row, facet_col, facet_page)
	} else {
		lf %>%
			leaflet::addPolygons(data = shp, layerId = idt, label = hdt, color = gp$col, opacity = gp$col_alpha, fillColor = gp$fill, fillOpacity = gp$fill_alpha, weight = gp$lwd, options = opt, group = group, dashArray = gp$lty, popup = popups) %>%
			assign_lf(facet_row, facet_col, facet_page)
	}
	NULL
}


lty2dash = function(lty) {
	tab = c(solid = "", dashed = "4 4", dotted = "1 3", dotdash = "1 3 4 3", longdash = "7 3", twodash = "2 2 6 2")
	are_words = (lty %in% names(tab))
	if (all(are_words)) {
		unname(tab[lty])
	} else {
		are_letters = (suppressWarnings(!is.na(as.numeric(lty))))

		if (!all(are_letters | are_words)) {
			stop("Incorrect lty specification: ", lty[which(!are_letters & !are_words)[1]])
		} else {
			lty[are_words] = unname(tab[lty[are_words]])
			lty[are_letters] = vapply(strsplit(lty[are_letters], ""), FUN = function(x) paste(x, collapse = " "), FUN.VALUE = character(1))
		}
		lty
	}

}

#' @export
#' @rdname tmap_internal
tmapLeafletLines = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt[!is.na(dt$lwd), ])
	shp = res$shp
	if (o$crs_leaflet$crsClass  == "L.CRS.Simple") {
		shp = sf::st_set_crs(shp, NA)
	}

	names(shp) = NULL # also required for other layers?
	dt = res$dt

	if (!is.null(idt)) {
		idt = idt$id[match(dt$tmapID__, idt$tmapID__)]
	}
	if (!is.null(hdt)) {
		hdt = hdt$hover[match(dt$tmapID__, hdt$tmapID__)]
	}

	if (is.null(pdt)) {
		popups = NULL
	} else {
		pdt = pdt[match(dt$tmapID__, pdt$tmapID__)][, tmapID__ := NULL]
		popups = view_format_popups(id = idt, titles = names(pdt), values = pdt, format = popup.format)
	}

	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	interactive = (!is.null(pdt) || !is.null(hdt))

	opt = leaflet::pathOptions(interactive = interactive, pane = pane)

	idt = (if (is.null(idt))dt$tmapID__ else idt) |>
		submit_labels("lines", pane, group)

	o$use_WebGL = impute_webgl(o$use_WebGL, dt, supported = "col", checkif = list(lty = "solid"), type = "lines", hover = !is.null(hdt), popup = !is.null(pdt), crs_class = o$crs_leaflet$crsClass)

	if (o$use_WebGL) {
		shp2 = sf::st_sf(id = seq_along(shp), geom = shp)
		shp3 = suppressWarnings(sf::st_cast(shp2, "LINESTRING"))
		gp3 = lapply(gp, function(gpi) {if (length(gpi) == 1) gpi else gpi[shp3$id]})
		lf %>%
			leafgl::addGlPolylines(data = shp3, color = gp3$col, opacity = gp3$col_alpha[1], weight = gp3$lwd[1]/4, pane = pane, group = group, layerId = idt, label = hdt, popup= popups) %>%
			assign_lf(facet_row, facet_col, facet_page)
	} else {

		lf %>%
			leaflet::addPolylines(data = shp, layerId = idt, label = hdt, color = gp$col, opacity = gp$col_alpha, weight = gp$lwd, group = group, options = opt, dashArray = lty2dash(gp$lty), popup = popups) %>%
			assign_lf(facet_row, facet_col, facet_page)

	}
	NULL
}

makeSymbolIcons2  = function (shape, color, fillColor = color, opacity, fillOpacity = opacity,
		  strokeWidth = 1, width, height = width, ...)
{
	symbols <- Map(leaflegend::makeSymbol, shape = shape, width = width,
				   height = height, color = color, fillColor = fillColor,
				   opacity = opacity, fillOpacity = fillOpacity, `stroke-width` = strokeWidth,
				   ...)
	leaflet::icons(iconUrl = unname(symbols), iconAnchorX = width/2 + strokeWidth,
				   iconAnchorY = height/2 + strokeWidth)
}

#' @export
#' @rdname tmap_internal
tmapLeafletSymbols = function(shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	args = list(...)
	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt[!is.na(dt$size) & !is.na(dt$shape), ])
	shp = res$shp
	if (o$crs_leaflet$crsClass  == "L.CRS.Simple") {
		shp = sf::st_set_crs(shp, NA)
	}
	dt = res$dt

	if (!is.null(idt)) {
		idt = idt$id[match(dt$tmapID__, idt$tmapID__)]
	}
	if (!is.null(hdt)) {
		hdt = hdt$hover[match(dt$tmapID__, hdt$tmapID__)]
	}

	if (is.null(pdt)) {
		popups = NULL
	} else {
		pdt = pdt[match(dt$tmapID__, pdt$tmapID__)][, tmapID__ := NULL]

		popups = view_format_popups(id = idt, titles = names(pdt), values = pdt, format = popup.format)
	}

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

	idt = (if (is.null(idt)) sprintf("%07d", dt$tmapID__)[shpTM_match] else idt[shpTM_match]) |>
		submit_labels("symbols", pane, group)


	interactive = (!is.null(pdt) || !is.null(hdt))

	opt = leaflet::pathOptions(interactive = interactive, pane = pane)

	o$use_WebGL = impute_webgl(o$use_WebGL, dt, supported = c("fill", "size"), checkif = list(shape = c(1,16,19,20,21)), type = "symbols", hover = !is.null(hdt), popup = !is.null(pdt), crs_class = o$crs_leaflet$crsClass)

	use_circleMarkers = o$use_circle_markers && all(gp$shape %in% c(1, 16, 19, 20, 21))


	gp2 = gp_to_lpar(gp, mfun = "Symbols", size_factor = 14) # 14 based on similarity with plot mode and consistency with tmap3
	#gp = gp2leafgp(gp)
	names(gp2)[names(gp2) == 'stroke-width'] = "strokeWidth"
	gp2$baseSize = 20

	#po(sort(gp2$width, decreasing = T))


	if (o$use_WebGL) {
		lf |>  leafgl::addGlPoints(sf::st_sf(shp), fillColor = gp2$fillColor, radius = gp2$width, fillOpacity = gp2$fillOpacity[1], pane = pane, group = group, label = hdt, popup = popups) %>%
			assign_lf(facet_row, facet_col, facet_page)
	} else if (use_circleMarkers) {
		gp2$strokeWidth[gp2$shape %in% c("solid-circle-md", "solid-circle-bg", "solid-circle-sm")] = 0
		gp2$fillOpacity[gp2$shape %in% "open-circle"] = 0
		multiplier = ifelse(gp2$shape == "solid-circle-md", 0.28, ifelse(gp2$shape == "solid-circle-sm", 0.25, 0.5)) # manually calibrated with 4k screen

		lf |>  leaflet::addCircleMarkers(data = sf::st_sf(shp), layerId = idt,
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
			size = size * args$icon.scale/3

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

split_alpha_channel <- function(x, alpha) {
	if (is.null(x)) {
		list(col=NULL, opacity=0)
	} else {
		RGBA <- col2rgb(x, alpha = TRUE)
		col <- rgb(RGBA[1,], RGBA[2,], RGBA[3,], maxColorValue = 255)
		opacity <- unname(RGBA[4,]/255 * alpha)
		list(col=col, opacity=opacity)
	}
}

#' @export
#' @rdname tmap_internal
tmapLeafletRaster = function(shpTM, dt, gp, pdt, popup.format, hdt, idt, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {

	rc_text = frc(facet_row, facet_col)


	shp = shpTM$shp
	tmapID = shpTM$tmapID

	if (is_regular_grid(shp)) {

		tid = intersect(tmapID, dt$tmapID__)

		color = rep(NA, length(tmapID)) # NA

		sel = which(tmapID %in% tid)
		tid2 = tmapID[sel]

		color[sel] = dt$col[match(tid2, dt$tmapID__)]


		#color = rep("#FFFFFF", length(tmapID))
		#color[match(dt$tmapID__, tmapID)] = dt$color

		pal <- na.omit(unique(color))
		pal <- pal[substr(pal, 8,10)!="00"] ## remove transparant colors

		if (!length(pal)) return(NULL)

		res <- split_alpha_channel(pal, alpha = 1)
		pal_col <- res$col
		pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)

		if ("col_alpha" %in% names(dt)) pal_opacity = max(dt$col_alpha)


		col_ids <- match(color, pal)

		m <- matrix(col_ids, ncol = ncol(shp))

		#matrix(color, ncol = ncol(shp))

		#m <- matrix(tmapID, ncol = ncol(shp))


		#m = tmapID

		#m[1,5] = 4

		shp2 = stars::st_as_stars(m, dimensions = shp)

		lf = get_lf(facet_row, facet_col, facet_page)

		opts = leaflet::gridOptions(pane = pane)

		lf %>%
			leafem::addStarsImage(shp2, band = 1, colors = pal_col, opacity = pal_opacity, group = group, options = opts) %>%
			assign_lf(facet_row, facet_col, facet_page)
	} else {
		#shp2 = stars::st_as_stars(list(values = tmapID), dimensions = shp)
		#shpTM = shapeTM(sf::st_geometry(sf::st_as_sf(shp2)), as.vector(tmapID))

		m = matrix(tmapID, nrow = nrow(shp), ncol = ncol(shp))
		shp2 = structure(list(tmapID = m), class = "stars", dimensions = shp)

		shp3 = sf::st_geometry(sf::st_as_sf(shp2))

		crs = get_option_class(o$crs_step4, "sf")

		shpTM = shapeTM(sf::st_transform(shp3, crs), tmapID)


		gp$lty = "solid"
		tmapLeafletPolygons(shpTM, dt, pdt, popup.format = NULL, hdt = NULL, idt = NULL, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
		#grid.shape(s, gp=gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
}

#' @export
#' @rdname tmap_internal
tmapLeafletText = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	args = list(...)

	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt

	#shadow = gp$shadow

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

	text = as.character(dt$text[shpTM_match])
	if (all(text == "")) return(NULL)

	idt = dt$tmapID__[shpTM_match] |>
		submit_labels("text", pane, group)

	opt = leaflet::pathOptions(interactive = FALSE, pane = pane)



	cex_set = unique(gp$cex)
	alpha_set = unique(gp$col_alpha)
	face_set = unique(gp$fontface)
	col_set = unique(gp$col)

	bgcol_set = unique(gp$bgcol)

	if (any(bgcol_set != "#00000000")) {
		message("Variable bgcol and bgcol_alpha not supported by view mode")
	}


	if (length(face_set) != 1) message("Variable fontfaces not supported by view mode")

	vary = (length(cex_set) != 1) || (length(alpha_set) != 1) || (length(face_set) != 1) || (length(col_set) != 1)

	sizeChar <- paste(round(gp$cex * 12), "px", sep="")

	# direction <- ifelse(gpl$text.just == "left", "right",
	# 			 ifelse(gpl$text.just == "right", "left",
	# 			 ifelse(gpl$text.just == "top", "bottom",
	# 			 ifelse(gpl$text.just == "bottom", "top", "center"))))
	direction = "right"

	clustering = args$clustering

	if (identical(clustering, FALSE)) {
		clustering = NULL
	} else if (identical(clustering, TRUE)) {
		clustering = leaflet::markerClusterOptions()
	}

	# apply xmod and ymod
	delta = delta_per_lineheight(bbx)

	coords[,1] = coords[,1] + delta * gp$cex * gp$xmod
	coords[,2] = coords[,2] + delta * gp$cex * gp$ymod




	if (!vary) {
		lf = lf %>% addLabelOnlyMarkers(lng = coords[, 1], lat = coords[,2],
										 label=text,
										 group=group,
										 layerId = idt,
										 labelOptions = labelOptions(noHide = TRUE,
										 							textOnly = TRUE,
										 							pane = pane,
										 							direction = direction,
										 							opacity=gp$col_alpha[1],
										 							textsize=sizeChar[1],
										 							style=list(color=gp$col[1])),
										 options = markerOptions(pane = pane),
										 clusterOptions = clustering)
	} else {
		for (i in 1:length(text)) {
			lf = lf %>% addLabelOnlyMarkers(lng = coords[i,1], lat = coords[i,2],
											 label=text[i],
											 group=group,
											 layerId = idt[i],
											 labelOptions = labelOptions(noHide = TRUE,
											 							textOnly = TRUE,
											 							pane = pane,
											 							direction = direction,
											 							opacity=gp$col_alpha[i],
											 							textsize=sizeChar[i],
											 							style=list(color=gp$col[i])),
											 options = markerOptions(pane = pane),
											 clusterOptions = clustering)
		}
	}
	assign_lf(lf, facet_row, facet_col, facet_page)



	NULL
}
