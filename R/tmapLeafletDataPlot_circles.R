#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_circles = function(a, shpTM, dt, pdt, popup.format,
											   hdt, idt, gp, bbx,
											   facet_row, facet_col, facet_page,
											   id, pane, group, glid, o, ...) {
	dots         = list(...)
	ptdt         = dots$ptdt
	popup.layout = dots$popup.layout

	lf      = get_lf(facet_row, facet_col, facet_page)
	rc_text = frc(facet_row, facet_col)

	# Filter to rows where size is known (no shape filter: circles have none)
	res = select_sf(shpTM, dt[!is.na(dt$size), ])
	shp = res$shp
	if (o$crs_leaflet$crsClass == "L.CRS.Simple") {
		shp = sf::st_set_crs(shp, NA)
	}
	dt = res$dt

	gp = impute_gp(gp, dt)
	# NOTE: do NOT call rescale_gp — that converts size to "lines" units.
	# For tm_circles, gp$size is already in metres (the direct scale output).

	coords = sf::st_coordinates(shp)
	cp     = expand_coords_gp(coords, gp, ndt = nrow(dt))
	coords = cp$coords
	gp     = cp$gp

	if (cp$expanded) {
		shpTM_match = match(shpTM$tmapID_expanded, shpTM$tmapID)
	} else {
		shpTM_match = TRUE
	}

	# ---- IDs / popups / hover ------------------------------------------------
	idt_null = is.null(idt)
	if (!idt_null) {
		idt = idt$id[match(dt$tmapID__[shpTM_match], idt$tmapID__)]
	} else {
		idt = sprintf("%07d", dt$tmapID__)[shpTM_match]
	}

	if (!is.null(hdt)) {
		hdt = hdt$hover[match(dt$tmapID__[shpTM_match], hdt$tmapID__)]
		hdt = lapply(hdt, htmltools::HTML, FUN.VALUE = character(1))
	}

	if (!is.null(ptdt)) {
		pttl = ptdt$title[match(dt$tmapID__[shpTM_match], ptdt$tmapID__)]
	} else {
		pttl = NULL
	}

	if (is.null(pdt)) {
		popups = NULL
	} else {
		mtch = match(dt$tmapID__[shpTM_match], pdt$tmapID__)
		pdt  = pdt[mtch][, tmapID__ := NULL]
		if (!is.null(pttl)) {
			popups = view_format_popups(id = pttl, titles = names(pdt), values = pdt,
										format = popup.format, layout = popup.layout)
		} else if (idt_null) {
			popups = view_format_popups(titles = names(pdt), values = pdt,
										format = popup.format, layout = popup.layout)
		} else {
			popups = view_format_popups(id = idt, titles = names(pdt), values = pdt,
										format = popup.format, layout = popup.layout)
		}
	}

	interactive = (!is.null(pdt) || !is.null(hdt))
	opt = leaflet::pathOptions(interactive = interactive, pane = pane)

	# ---- Radius in metres ----------------------------------------------------
	# gp$size is the direct scale output, interpreted as metres.
	# With tm_scale_asis() the raw data values are used as-is.
	# With other scales, users control the output range via values.range /
	# values.scale in the scale function.
	radius_m = gp$size

	# ---- Colour / stroke parameters -----------------------------------------
	gp2 = gp_to_lpar(gp, mfun = "Symbols", size_factor = 14)

	shp2 = sf::st_as_sf(as.data.frame(coords), coords = c("X", "Y"),
						 crs = sf::st_crs(shp))

	idt = submit_labels(idt, "circles", pane, group)

	lf |>
		leaflet::addCircles(
			data        = shp2,
			layerId     = idt,
			radius      = radius_m,           # metres — scales with zoom natively
			color       = gp2$color,          # stroke colour
			opacity     = gp2$opacity,
			weight      = gp2$strokeWidth,
			dashArray   = gp2[["stroke-dasharray"]],
			fillColor   = gp2$fillColor,
			fillOpacity = gp2$fillOpacity,
			options     = opt,
			group       = group,
			label       = hdt,
			popup       = popups
		) |>
		assign_lf(facet_row, facet_col, facet_page)

	NULL
}
