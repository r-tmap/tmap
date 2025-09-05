#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_lines = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
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
		hdt = lapply(hdt, htmltools::HTML, FUN.VALUE = character(1))
	}

	if (is.null(pdt)) {
		popups = NULL
	} else {
		mtch = match(dt$tmapID__, pdt$tmapID__)
		pdt = pdt[mtch][, tmapID__ := NULL]

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

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_iso = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	# isn't called, but needed to make tm_iso visible in tmap_overview
	NextMethod()
}

