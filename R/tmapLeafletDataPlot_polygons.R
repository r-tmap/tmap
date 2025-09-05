#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_polygons = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
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
		hdt = lapply(hdt, htmltools::HTML, FUN.VALUE = character(1))
	}

	if (is.null(pdt)) {
		popups = NULL
	} else {
		mtch = match(dt$tmapID__, pdt$tmapID__)
		pdt = pdt[mtch][, tmapID__ := NULL]

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


#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_fill = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_borders = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}
