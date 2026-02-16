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


	# process hitbox option:
	hitbox = if (a$hitbox == "auto") {
		thin = median(gp$lwd) < 4
		light = length(shp) < 10000
		if (thin && light) "pmax8" else "none"
	} else {
		a$hitbox
	}

	k = nrow(dt)
	if (hitbox == "none") {
		opt1 = leaflet::pathOptions(interactive = interactive, pane = pane, lineCap = gp$lineend, lineJoin = gp$linejoin)

		idt = (if (is.null(idt)) dt$tmapID__ else idt) |>
			submit_labels("lines", pane, group)
	} else {
		opt1 = leaflet::pathOptions(interactive = FALSE, pane = pane, lineCap = gp$lineend, lineJoin = gp$linejoin)
		opt2 = leaflet::pathOptions(interactive = interactive, pane = pane, lineCap = gp$lineend, lineJoin = gp$linejoin)
		idt = (if (is.null(idt))rep(dt$tmapID__, 2) else rep(idt, 2)) |>
			submit_labels("lines", pane, group)
	}




	# idt2 = (if (is.null(tmp_idt))dt$tmapID__ else tmp_idt) |>
	# 	submit_labels("lines", pane, group)


	o$use_WebGL = impute_webgl(o$use_WebGL, dt, supported = "col", checkif = list(lty = "solid"), type = "lines", hover = !is.null(hdt), popup = !is.null(pdt), crs_class = o$crs_leaflet$crsClass)

	if (o$use_WebGL) {
		shp2 = sf::st_sf(id = seq_along(shp), geom = shp)
		shp3 = suppressWarnings(sf::st_cast(shp2, "LINESTRING"))
		gp3 = lapply(gp, function(gpi) {if (length(gpi) == 1) gpi else gpi[shp3$id]})
		popups = popups[shp3$id]

		if (hitbox == "none") {
			lf %>%
				lf %>%
				leafgl::addGlPolylines(
					data = shp3,
					color = gp3$col,
					opacity = gp3$col_alpha[1],
					weight = gp3$lwd[1]/4,
					pane = pane,
					group = group,
					layerId = idt,
					label = hdt,
					popup= popups) %>%
				assign_lf(facet_row, facet_col, facet_page)
		} else {
			lf %>%
				# Fast WebGL rendering
				leafgl::addGlPolylines(
					data    = shp3,
					color   = gp3$col,
					opacity = gp3$col_alpha[1],
					weight  = gp3$lwd[1] / 4,
					pane    = pane,
					group   = group,
					popup = NULL,
					layerId = idt[1:k]
				) %>%

				# Invisible interaction layer
				leaflet::addPolylines(
					data    = shp3,
					opacity = 0,
					weight  = max(gp3$lwd / 4, 4),
					group   = group,
					layerId = idt[(k+1):(2*k)],
					label   = hdt,
					options = opt2,
					popup   = popups
				) %>%

				assign_lf(facet_row, facet_col, facet_page)
		}

	} else {
		if (hitbox == "none") {
			lf %>%
				leaflet::addPolylines(
					data = shp,
					layerId = idt,
					label = hdt,
					color = gp$col,
					opacity = gp$col_alpha,
					weight = gp$lwd,
					group = group,
					options = opt,
					dashArray = lty2dash(gp$lty, gp$lwd),
					popup = popups) %>%
				assign_lf(facet_row, facet_col, facet_page)
		} else {
			lf %>%
				# Visible styled layer (no popup here)
				leaflet::addPolylines(
					data      = shp,
					layerId = idt[1:k],
					color     = gp$col,
					opacity   = gp$col_alpha,
					weight    = gp$lwd,
					group     = group,
					options   = opt1,
					dashArray = lty2dash(gp$lty, gp$lwd)
				) %>%

				# Invisible interaction layer (fatter hitbox)
				leaflet::addPolylines(
					data    = shp,
					layerId = idt[(k+1):(2*k)],
					label   = hdt,
					popup   = popups,
					options = opt2,
					opacity = 0,
					weight  = pmax(gp$lwd, 8),
					group   = group
				) %>%

				assign_lf(facet_row, facet_col, facet_page)

		}


		# lf %>%
		# 	leaflet::addPolylines(data = shp, layerId = idt, label = hdt, color = gp$col, opacity = gp$col_alpha, weight = gp$lwd, group = group, options = opt, dashArray = lty2dash(gp$lty, gp$lwd), popup = popups) %>%
		# 	assign_lf(facet_row, facet_col, facet_page)

	}
	NULL
}

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_iso = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	# isn't called, but needed to make tm_iso visible in tmap_overview
	NextMethod()
}

