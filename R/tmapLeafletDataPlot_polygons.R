#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_polygons = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	# popup.title data (ptdt) and popup layout are passed by the tmap core via
	# `...` rather than as named formals, so that this method's signature stays
	# identical to the tmapLeafletDataPlot generic (S3 consistency) and so that
	# extension methods that do not handle them keep working unchanged.
	dots = list(...)
	ptdt = dots$ptdt
	popup.layout = dots$popup.layout

	lf = get_lf(facet_row, facet_col, facet_page)

	rc_text = frc(facet_row, facet_col)

	res = select_sf(shpTM, dt)
	shp = res$shp
	if (o$crs_leaflet$crsClass  == "L.CRS.Simple") {
		shp = sf::st_set_crs(shp, NA)
	}

	dt = res$dt

	idt_null = is.null(idt)

	if (!idt_null) {
		idt = idt$id[match(dt$tmapID__, idt$tmapID__)]
	} else {
		idt = sprintf("%07d", dt$tmapID__)
	}
	if (!is.null(hdt)) {
		hdt = hdt$hover[match(dt$tmapID__, hdt$tmapID__)]
		hdt = lapply(hdt, htmltools::HTML, FUN.VALUE = character(1))
	}

	# popup title: explicit popup.title (ptdt) takes precedence; otherwise fall
	# back to the previous behaviour (id, or hover when no id).
	if (!is.null(ptdt)) {
		pttl = ptdt$title[match(dt$tmapID__, ptdt$tmapID__)]
	} else {
		pttl = NULL
	}

	if (is.null(pdt)) {
		popups = NULL
	} else {
		mtch = match(dt$tmapID__, pdt$tmapID__)
		pdt = pdt[mtch][, tmapID__ := NULL]

		if (!is.null(pttl)) {
			popups = view_format_popups(id = pttl, titles = names(pdt), values = pdt, format = popup.format, layout = popup.layout)
		} else if (idt_null && !is.null(hdt)) {
			popups = view_format_popups(id = hdt, titles = names(pdt), values = pdt, format = popup.format, layout = popup.layout)
		} else if (idt_null && is.null(hdt)) {
			popups = view_format_popups(id = NULL, titles = names(pdt), values = pdt, format = popup.format, layout = popup.layout)
		} else {
			popups = view_format_popups(id = idt, titles = names(pdt), values = pdt, format = popup.format, layout = popup.layout)
		}
	}


	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)
	gp_pat = list(pattern = gp$pattern, pattern_col = gp$pattern_col,
				  pattern_size = gp$pattern_size, pattern_density = gp$pattern_density)
	gp = gp_to_lpar(gp, mfun = "Polygons", rename_prop = FALSE)

	interactive = (!is.null(pdt) || !is.null(hdt))

	opt = leaflet::pathOptions(interactive = interactive, pane = pane)

	o$use_WebGL = impute_webgl(o$use_WebGL, dt, supported = c("fill", "col"), checkif = list(lty = "solid"), type = "polygons", hover = !is.null(hdt), popup = !is.null(pdt), crs_class = o$crs_leaflet$crsClass)

	# patterns are drawn as SVG patterns, which are not supported by WebGL
	pattern.zoom = isTRUE(a$pattern.zoom)
	pat = leaflet_pattern_ids(gp_pat, length(shp), o, zoom = pattern.zoom, suffix = if (pattern.zoom) {
		substr(rlang::hash(list(Sys.time(), facet_row, facet_col, facet_page, pane)), 1, 8)
	} else NULL)
	if (!is.null(pat) && isTRUE(o$use_WebGL)) {
		cli::cli_inform("{.field [view mode]} WebGL is not used for polygons with patterns",
						.frequency = "once", .frequency_id = "pattern_webgl")
	}
	if (!is.null(pat)) o$use_WebGL = FALSE

	if (o$use_WebGL) {
		idt = rep({if (is.null(idt))dt$tmapID__[1] else idt[1]}, 2) |>
			submit_labels("polygonsGL", pane, group)

		shp2 = sf::st_sf(id = seq_along(shp), geom = shp)
		shp3 = sf_expand(shp2)

		shp3lines = suppressWarnings(sf::st_cast(shp3, "LINESTRING"))
		gp3 = lapply(gp, function(gpi) {if (length(gpi) == 1) gpi else gpi[shp3$split__id]})
		popups2 = popups[shp3$split__id]

		# opacity channel from fill (e.g. "#FF000099") is ignored by addGlPolygons
		fill_alpha = split_alpha_channel(gp3$fill[1], alpha = gp3$fill_alpha[1])$opacity

		lf |>
			leafgl::addGlPolygons(data = shp3, layerId = idt[1], label = hdt,
								  color = gp3$fill, opacity = fill_alpha,
								  group = group, pane = pane, popup = popups2) %>%
			{if (gp3$lwd[1]!=0 && gp3$col[1] != "#00000000") leafgl::addGlPolylines(., data = shp3lines, color = gp3$col, opacity = gp3$col_alpha[1], weight = gp3$lwd[1]/4, pane = pane, group = group, layerId = idt[2]) else .} %>%
			blend_lf(a$blend, pane) %>%
			assign_lf(facet_row, facet_col, facet_page)
	} else {
		idt = (if (is.null(idt))dt$tmapID__ else idt) |>
			submit_labels("polygons", pane, group)

		lf = lf %>%
			leaflet::addPolygons(data = shp, layerId = idt, label = hdt, color = gp$col, opacity = gp$col_alpha, fillColor = gp$fill, fillOpacity = gp$fill_alpha, weight = gp$lwd, options = opt, group = group, dashArray = gp$lty, popup = popups)

		if (!is.null(pat)) {
			# patterns on top of the fill (not interactive, so popups and hover labels of the
			# polygons below still work), and the borders on top of the patterns
			opt2 = leaflet::pathOptions(interactive = FALSE, pane = pane)
			sel = which(!is.na(pat$ids))
			zoom_ids = if (pattern.zoom) unique(na.omit(pat$ids)) else character()
			lf = lf %>%
				leaflet_add_pattern_defs(pat$defs, zoom = zoom_ids) %>%
				leaflet::addPolygons(data = shp[sel], fillColor = sprintf("url(#%s)", pat$ids[sel]),
									 fillOpacity = 1, stroke = FALSE, options = opt2, group = group) %>%
				leaflet::addPolygons(data = shp, color = gp$col, opacity = gp$col_alpha, fill = FALSE,
									 weight = gp$lwd, dashArray = gp$lty, options = opt2, group = group)
		}

		lf %>%
			blend_lf(a$blend, pane) %>%
			assign_lf(facet_row, facet_col, facet_page)
	}
	NULL
}


#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_fill = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	NextMethod()
}

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_borders = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	NextMethod()
}


# SVG pattern ids per feature (NA: no pattern) and the SVG <pattern> definitions, from the pattern
# map variables (gp elements pattern, pattern_col, pattern_size, pattern_density). Ids are derived
# from the pattern definitions, so equal patterns share an id (also across maps on one page).
# Zooming patterns (zoom = TRUE) depend on the zoom level of a map, so their ids get a suffix
# that is unique per map.
leaflet_pattern_ids = function(gp, n, o, zoom = FALSE, suffix = NULL) {
	unresolved = function(x) is.null(x) || (is.character(x) && any(startsWith(x, "__"), na.rm = TRUE))
	if (unresolved(gp$pattern) || all(is.na(gp$pattern) | gp$pattern == "none")) return(NULL)
	pdt = data.frame(pattern = rep_len(gp$pattern, n),
					 col = rep_len(if (unresolved(gp$pattern_col)) "grey25" else gp$pattern_col, n),
					 size = rep_len(if (unresolved(gp$pattern_size)) 1 else gp$pattern_size, n),
					 density = rep_len(if (unresolved(gp$pattern_density)) 0.8 else gp$pattern_density, n),
					 stringsAsFactors = FALSE)
	key = do.call(paste, c(unname(as.list(pdt)), sep = "|"))
	ukey = unique(key)
	hashes = vapply(ukey, function(k) substr(rlang::hash(k), 1, 12), character(1), USE.NAMES = FALSE)
	uids = paste0("tmap-pattern-", hashes,
				  if (zoom) paste0("-z", suffix) else "")
	updt = pdt[match(ukey, key), , drop = FALSE]
	defs = mapply(function(p, col, size, density, id) {
		if (is.na(p) || p == "none" || is.na(density)) return(NA_character_)
		x = pattern_svg(p, density = density, size = size, col = col, id = id, scale = o$scale)
		if (is.null(x)) NA_character_ else x
	}, updt$pattern, updt$col, updt$size, updt$density, uids, USE.NAMES = FALSE)
	ids = uids[match(key, ukey)]
	ids[is.na(defs[match(key, ukey)])] = NA_character_
	defs = defs[!is.na(defs)]
	if (!length(defs)) return(NULL)
	list(ids = ids, defs = defs)
}

# Add SVG pattern definitions to the page: a hidden <svg> with <defs> in the map container.
# Leaflet sets the fill of paths to the fillColor, so fillColor = "url(#id)" refers to them.
# The patterns use userSpaceOnUse units (screen pixels), so they keep their size when zooming.
# Zooming patterns (ids in zoom) get a patternTransform that is updated after each zoom: a scale
# factor relative to the initial zoom level, and a translation that anchors the pattern to a fixed
# location (the initial map center), so that the pattern sticks to the map.
leaflet_add_pattern_defs = function(lf, defs, zoom = character()) {
	htmlwidgets::onRender(lf, "function(el, x, data) {
		var map = this;
		var svg = el.querySelector('svg.tmap-pattern-defs');
		if (!svg) {
			svg = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
			svg.setAttribute('class', 'tmap-pattern-defs');
			svg.setAttribute('width', '0');
			svg.setAttribute('height', '0');
			svg.style.position = 'absolute';
			svg.appendChild(document.createElementNS('http://www.w3.org/2000/svg', 'defs'));
			el.appendChild(svg);
		}
		var defs = svg.querySelector('defs');
		[].concat(data.defs).forEach(function(d) {
			var tmp = document.createElementNS('http://www.w3.org/2000/svg', 'svg');
			tmp.innerHTML = d;
			var p = tmp.firstChild;
			if (p && !document.getElementById(p.id)) defs.appendChild(p);
		});
		var zoomIds = [].concat(data.zoom || []);
		if (zoomIds.length) {
			var z0 = map.getZoom();
			var anchor = map.getCenter();
			var update = function() {
				var s = Math.pow(2, map.getZoom() - z0);
				var p = map.latLngToLayerPoint(anchor);
				zoomIds.forEach(function(id) {
					var pat = document.getElementById(id);
					if (pat) pat.setAttribute('patternTransform', 'translate(' + p.x + ',' + p.y + ') scale(' + s + ')');
				});
			};
			map.on('zoomend viewreset moveend', update);
			update();
		}
	}", data = list(defs = as.list(defs), zoom = as.list(zoom)))
}
