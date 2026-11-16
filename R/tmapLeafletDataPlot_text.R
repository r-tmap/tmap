#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_text = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
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

	text = decode_expr_chr(as.character(dt$text[shpTM_match]))
	if (all(text == "")) return(NULL)

	idt = dt$tmapID__[shpTM_match] |>
		submit_labels("text", pane, group)

	opt = leaflet::pathOptions(interactive = FALSE, pane = pane)


	clusterOpts = getClusterOpts(a$clustering)

	cidt = if (!is.null(clusterOpts)) submit_labels(idt[1], "cluster", pane, group) else NULL


	cex_set = unique(gp$cex)
	alpha_set = unique(gp$col_alpha)
	face_set = unique(gp$fontface)
	col_set = unique(gp$col)

	bgcol_set = unique(gp$bgcol)
	bgcol_alpha = unique(gp$bgcol_alpha)

	if (all(bgcol_set == "#00000000")) {
		bg_enabled = FALSE
	} else {
		bgres = split_alpha_channel(gp$bgcol, gp$bgcol_alpha)
		gp$bgcol = bgres$col
		gp$bgcol_alpha = bgres$opacity
		bg_enabled = TRUE
	}



	if (length(face_set) != 1) message("Variable fontfaces not supported by view mode")

	vary = (length(cex_set) != 1) || (length(alpha_set) != 1) || (length(face_set) != 1) || (length(col_set) != 1) || (length(bgcol_set) != 1) || (length(bgcol_alpha) != 1)

	sizeChar <- paste(round(gp$cex * 12), "px", sep="")

	# direction <- ifelse(gpl$text.just == "left", "right",
	# 			 ifelse(gpl$text.just == "right", "left",
	# 			 ifelse(gpl$text.just == "top", "bottom",
	# 			 ifelse(gpl$text.just == "bottom", "top", "center"))))
	direction = "right"

	# label placement (tm_labels): polygon labels are centered on the center of the largest inscribed circle
	is_poly = if ("poly" %in% names(dt)) !vapply(dt$poly, is.null, logical(1)) else FALSE
	if (!is.null(a$placement) && any(is_poly)) direction = "center"


	# apply xmod and ymod
	delta = delta_per_lineheight(bbx)

	coords[,1] = coords[,1] + delta * gp$cex * gp$xmod
	coords[,2] = coords[,2] + delta * gp$cex * gp$ymod

	scale_px = function(textlines, cex) {
		ceiling(textlines * cex * 12)
	}


	make_shadow = function(cex, textcol) {
		if (a$halo) {
			if (is.na(a$halo.col)) a$halo.col = ifelse(is_light(textcol), "#000000DD", "#FFFFFFDD")
			make_halo_css(col = a$halo.col, mode = "halo", width = scale_px(a$halo.width, cex), blur = scale_px(a$halo.blur, cex), diag_scale = sqrt(2)/2, alpha = a$halo.alpha)
		} else if (a$shadow) {
			if (is.na(a$shadow.col)) a$shadow.col = ifelse(is_light(textcol), "#000000", "#FFFFFF")
			make_halo_css(col = a$shadow.col, mode = "shadow", offset.x = scale_px(a$shadow.offset.x, cex), offset.y = scale_px(a$shadow.offset.y, cex), alpha = 1)
		} else {
			NULL
		}
	}

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
																	style=list("color"=gp$col[1],
																			   "background-color" = if (bg_enabled) paste0("rgba(", paste(col2rgb(gp$bgcol[1]), collapse = ", "), ",", gp$bgcol_alpha[1], ")") else NULL,
																			  "border" = if (a$bg.border) paste0(a$bg.border.lwd, "px solid rgba(", paste(col2rgb(a$bg.border.col[1]), collapse = ", "), ",", gp$bgcol_alpha[1], ")") else NULL,
																			  "line-height" = sizeChar[1],
																			  "text-shadow" = make_shadow(gp$cex[1], gp$col[1]),
																			  "padding" = paste0(scale_px(a$bg.padding / 2, gp$cex[1]), "px"))),
										options = markerOptions(pane = pane),
										clusterOptions = clusterOpts,
										clusterId = cidt)
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
																		style=list("color"=gp$col[i],
																				   "background-color" = if (bg_enabled) paste0("rgba(", paste(col2rgb(gp$bgcol[i]), collapse = ", "), ",", gp$bgcol_alpha[i], ")") else NULL,
																				   "border" = if (a$bg.border) paste0(a$bg.border.lwd, "px solid rgba(", paste(col2rgb(a$bg.border.col[i]), collapse = ", "), ",", gp$bgcol_alpha[i], ")") else NULL,
																				   "line-height" = sizeChar[i],
																				   "text-shadow" = make_shadow(gp$cex[i], gp$col[i]),
																				   "padding" = paste0(scale_px(a$bg.padding / 2, gp$cex[i]), "px"))),
											options = markerOptions(pane = pane),
											clusterOptions = clusterOpts,
											clusterId = cidt)
		}
	}
	# label placement (tm_labels): hide overlapping labels after each zoom/pan, in the order of priority
	# (option D: the collision of labels is resolved by the renderer)
	if (!is.null(a$placement) && utils::tail(a$placement$fallback, 1) == "drop" && is.null(clusterOpts) && length(text) > 1L) {
		prio = if ("priority" %in% names(dt) && length(unique(dt$priority)) > 1L) {
			as.numeric(dt$priority[shpTM_match])
		} else if (any(is_poly) && !cp$expanded) {
			vapply(dt$poly, function(p) if (is.null(p)) 0 else as.numeric(sf::st_area(p)), numeric(1))
		} else {
			rep_len(gp$cex, length(text))
		}
		lf = leaflet_label_collision(lf, ids = as.character(idt), priority = prio)
	}

	assign_lf(lf, facet_row, facet_col, facet_page)

	NULL
}

# Label collision in view mode (leaflet): after each zoom and pan, labels are shown in the order of priority, and
# labels that overlap with a label shown before are hidden. Labels reappear when there is space (e.g. after zooming
# in).
leaflet_label_collision = function(lf, ids, priority) {
	htmlwidgets::onRender(lf, "function(el, x, data) {
		var map = this;
		var ids = [].concat(data.ids);
		var prio = [].concat(data.prio);
		var order = ids.map(function(d, i) { return i; }).sort(function(a, b) { return prio[b] - prio[a]; });
		var update = function() {
			var placed = [];
			order.forEach(function(i) {
				var lyr = map.layerManager.getLayer('marker', ids[i]);
				if (!lyr || !map.hasLayer(lyr) || !lyr.getTooltip) return;
				var tt = lyr.getTooltip();
				var elm = tt && tt.getElement();
				if (!elm) return;
				elm.style.visibility = '';
				var r = elm.getBoundingClientRect();
				if (r.width === 0 && r.height === 0) return;
				var hit = placed.some(function(p) {
					return r.left < p.right && r.right > p.left && r.top < p.bottom && r.bottom > p.top;
				});
				if (hit) {
					elm.style.visibility = 'hidden';
				} else {
					placed.push(r);
				}
			});
		};
		map.on('zoomend moveend overlayadd overlayremove', update);
		map.whenReady(function() { setTimeout(update, 0); });
	}", data = list(ids = ids, prio = priority))
}


#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_labels = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	NextMethod()
}

#' @export
#' @rdname tmapGridLeaflet
tmapLeafletDataPlot.tm_data_labels_highlighted = function(a, shpTM, dt, pdt, popup.format, hdt, idt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, glid, o, ...) {
	NextMethod()
}




make_halo_css <- function(
		col = "white",
		alpha = 0.9,
		width = 1,
		blur = 0,
		diagonals = TRUE,
		diag_scale = 1,
		mode = c("halo", "shadow"),
		offset.x = 2,
		offset.y = 2
) {

	mode <- match.arg(mode)

	# convert color to rgba
	rgb <- grDevices::col2rgb(col)
	col_css <- sprintf(
		"rgba(%d,%d,%d,%s)",
		rgb[1], rgb[2], rgb[3], alpha
	)

	# --- Simple drop shadow mode ---
	if (mode == "shadow") {
		return(sprintf(
			"%spx %spx %spx %s",
			offset.x, offset.y, blur, col_css
		))
	}

	# --- Halo mode ---
	# Cardinal directions
	offsets <- rbind(
		c(-1, 0),
		c( 1, 0),
		c( 0,-1),
		c( 0, 1)
	)

	# Optional diagonals
	if (diagonals) {
		diag_offsets <- rbind(
			c(-1,-1),
			c( 1,-1),
			c(-1, 1),
			c( 1, 1)
		)

		# allow smaller diagonal distance for rounder halo
		diag_offsets <- diag_offsets * diag_scale

		offsets <- rbind(offsets, diag_offsets)
	}

	# Scale by width
	offsets <- round(offsets * width, 2)

	parts <- apply(offsets, 1, function(x) {
		sprintf("%spx %spx 0 %s", x[1], x[2], col_css)
	})

	# Optional glow
	if (blur > 0) {
		parts <- c(parts, sprintf("0 0 %spx %s", blur, col_css))
	}

	paste(parts, collapse = ", ")
}
