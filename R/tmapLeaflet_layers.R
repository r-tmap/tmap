
tmapLeafletPolygons = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, o) {
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	fill = if ("fill" %in% names(dt)) dt$fill else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	lf |> 
		leaflet::addPolygons(data = shp, color = color, opacity = 1, fillColor = fill, fillOpacity = 1) |> 
		assign_lf(facet_row, facet_col, facet_page)
	NULL	
}


tmapLeafletSymbols = function(shpTM, dt, bbx, facet_row, facet_col, facet_page) {
	lf = get_lf(facet_row, facet_col, facet_page)
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	size = if ("size" %in% names(dt)) dt$size else rep(NA, nrow(dt))
	shape = if ("shape" %in% names(dt)) dt$shape else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	coords = sf::st_coordinates(shp)
	
	
	lf |> 
		leaflet::addCircleMarkers(lng = coords[, 1], lat = coords[, 2], fillColor = color, radius = size*4, fillOpacity = 1, color = "black", opacity = 1, weight = 1) |> 
		assign_lf(facet_row, facet_col, facet_page)
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


tmapLeafletRaster = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, o) {
	
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
		
		color[sel] = dt$color[match(tid2, dt$tmapID__)]
		
		
		
		#color = rep("#FFFFFF", length(tmapID))
		#color[match(dt$tmapID__, tmapID)] = dt$color
		
		pal <- na.omit(unique(color))
		pal <- pal[substr(pal, 8,10)!="00"] ## remove transparant colors
		
		res <- split_alpha_channel(pal, alpha = 1)
		pal_col <- res$col
		pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)
		
		col_ids <- match(color, pal)
		
		shp[[1]] <- matrix(col_ids, ncol = ncol(shp))
		
		lf = get_lf(facet_row, facet_col, facet_page)
		
		#shp2 = transwarp(shp, crs = st_crs(3857), raster.warp = TRUE)
		
		lf |> 
			leafem::addStarsImage(shp, band = 1, colors = pal) |> 
			assign_lf(facet_row, facet_col, facet_page)
	} else {
		shpTM <- shapeTM(sf::st_as_sf(shp), tmapID)
		tmapLeafletPolygons(shpTM, dt, facet_row, facet_col, facet_page)
		#grid.shape(s, gp=gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
} 