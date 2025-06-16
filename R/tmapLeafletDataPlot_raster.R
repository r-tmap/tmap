#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_raster = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {

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
		a2 = structure(list(), class = "tm_data_polygons")
		tmapLeafletDataPlot(a2, shpTM, dt, pdt, popup.format = NULL, hdt = NULL, idt = NULL, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o)
		#grid.shape(s, gp=gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
}
