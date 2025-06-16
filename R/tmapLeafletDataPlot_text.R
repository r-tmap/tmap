#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_text = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
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


#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_labels = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_labels_highlighted = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o, ...) {
	NextMethod()
}

