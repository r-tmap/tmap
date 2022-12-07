
tmapLeafletPolygons = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)
	
	opt = leaflet::pathOptions(interactive = TRUE, pane = pane)
	
	if (o$use.WebGL) {
		shp2 = sf::st_sf(id = 1:length(shp), geom = shp)
		shp3 = suppressWarnings(sf::st_cast(shp2, "POLYGON"))
		gp3 = lapply(gp, function(gpi) {if (length(gpi) == 1) gpi else gpi[shp3$id]})
		lf |> 
			leafgl::addGlPolygons(data = shp3, color = gp3$col, opacity = gp3$col_alpha, fillColor = gp3$fill, fillOpacity = gp3$fill_alpha, weight = gp3$lwd, group = group, pane = pane) |> 
			assign_lf(facet_row, facet_col, facet_page)
	} else {
		lf |> 
			leaflet::addPolygons(data = shp, color = gp$col, opacity = gp$col_alpha, fillColor = gp$fill, fillOpacity = gp$fill_alpha, weight = gp$lwd, options = opt, group = group) |> 
			assign_lf(facet_row, facet_col, facet_page)
	}
	NULL	
}

tmapLeafletLines = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)
	
	opt = leaflet::pathOptions(interactive = TRUE, pane = pane)
	
	if (o$use.WebGL) {
		shp2 = sf::st_sf(id = 1:length(shp), geom = shp)
		shp3 = suppressWarnings(sf::st_cast(shp2, "LINESTRING"))
		gp3 = lapply(gp, function(gpi) {if (length(gpi) == 1) gpi else gpi[shp3$id]})
		lf |> 
			leafgl::addGlPolylines(data = shp3, color = gp3$col, opacity = gp3$col_alpha, weight = gp3$lwd, pane = pane, group = group) |> 
			assign_lf(facet_row, facet_col, facet_page)
	} else {
		lf |> 
			leaflet::addPolylines(data = shp, color = gp$col, opacity = gp$col_alpha, weight = gp$lwd, group = group, options = opt) |> 
			assign_lf(facet_row, facet_col, facet_page)
	}
	NULL	
}

tmapLeafletSymbols = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	gp = impute_gp(gp, dt)
	gp = rescale_gp(gp, o$scale_down)

	coords = sf::st_coordinates(shp)
	
	opt = leaflet::pathOptions(interactive = TRUE, pane = pane)
	
	if (o$use.WebGL) {
		lf |> 
			leafgl::addGlPoints(sf::st_sf(shp), fillColor = gp$fill, radius = gp$size*10, fillOpacity = gp$fill_alpha, color = gp$col, opacity = gp$color_alpha, weight = gp$lwd, pane = pane, group = group) |> 
			assign_lf(facet_row, facet_col, facet_page)
	} else {
		lf |> 
			leaflet::addCircleMarkers(lng = coords[, 1], lat = coords[, 2], fillColor = gp$fill, radius = gp$size*4, fillOpacity = gp$fill_alpha, color = gp$col, opacity = gp$color_alpha, weight = gp$lwd, group = group, options = opt) |> 
			assign_lf(facet_row, facet_col, facet_page)
	}
	
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


tmapLeafletRaster = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, pane, group, o) {
	
	rc_text = frc(facet_row, facet_col)
	
	
	#bb_target <- bbx #attr(shp, "bbox")
	#bb_real <- bbx #sf::st_bbox(shp)
	
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
		
		res <- split_alpha_channel(pal, alpha = 1)
		pal_col <- res$col
		pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)
		
		if ("col_alpha" %in% names(dt)) pal_opacity = max(dt$col_alpha)
		
		
		col_ids <- match(color, pal)
		
		m <- matrix(col_ids, ncol = ncol(shp))
		
		shp2 = st_as_stars(m, dimensions = shp)

		lf = get_lf(facet_row, facet_col, facet_page)
		
		#shp2 = transwarp(shp, crs = st_crs(3857), raster.warp = TRUE)
		
		lf |> 
			leafem::addStarsImage(shp2, band = 1, colors = pal_col, opacity = pal_opacity, group = group) |> 
			assign_lf(facet_row, facet_col, facet_page)
	} else {
		shpTM <- shapeTM(sf::st_as_sf(shp), tmapID)
		tmapLeafletPolygons(shpTM, dt, facet_row, facet_col, facet_page, id, pane, group, o)
		#grid.shape(s, gp=gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
} 


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
	text = as.character(gp$text)
	
	coords = sf::st_coordinates(shp)
	
	opt = leaflet::pathOptions(interactive = TRUE, pane = pane)
	
	cex_set = unique(gp$cex)
	alpha_set = unique(gp$col_alpha) 
	face_set = unique(gp$fontface)
	col_set = unique(gp$col)
	
	if (length(face_set) != 1) warning("Variable fontfaces not supported by view mode")
	
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
	
	
	
	
	
	if (!vary) {
		lf = lf |> addLabelOnlyMarkers(lng = coords[, 1], lat = coords[,2], 
										 label=text,
										 group=group, 
										 #layerId = ids, 
										 labelOptions = labelOptions(noHide = TRUE, 
										 							textOnly = TRUE, 
										 							direction = direction, 
										 							opacity=gp$col_alpha[1],
										 							textsize=sizeChar[1],
										 							style=list(color=gp$col[1])),
										 clusterOptions = clustering,
										 options = markerOptions(pane = pane))
	} else {
		for (i in 1:length(text)) {
			lf = lf |> addLabelOnlyMarkers(lng = coords[i,1], lat = coords[i,2], 
											 label=text[i],
											 group=group, 
											 #layerId = ids[i], 
											 labelOptions = labelOptions(noHide = TRUE, 
											 							textOnly = TRUE, 
											 							direction = direction, 
											 							opacity=gp$col_alpha[i],
											 							textsize=sizeChar[i],
											 							style=list(color=gp$col[i])),
											 clusterOptions = clustering,
											 options = markerOptions(pane = pane))	
		}
	}
	assign_lf(lf, facet_row, facet_col, facet_page)
	
	# if (o$use.WebGL) {
	# 	lf |> 
	# 		leafgl::addGlPoints(sf::st_sf(shp), fillColor = gp$fill, radius = gp$size*10, fillOpacity = gp$fill_alpha, color = gp$col, opacity = gp$color_alpha, weight = gp$lwd, pane = pane, group = group) |> 
	# 		assign_lf(facet_row, facet_col, facet_page)
	# } else {
	# 	lf |> 
	# 		leaflet::addCircleMarkers(lng = coords[, 1], lat = coords[, 2], fillColor = gp$fill, radius = gp$size*4, fillOpacity = gp$fill_alpha, color = gp$col, opacity = gp$color_alpha, weight = gp$lwd, group = group, options = opt) |> 
	# 		assign_lf(facet_row, facet_col, facet_page)
	# }
	
	NULL	
}
