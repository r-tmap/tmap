tmapLeafletInit = function(nrow, ncol) {
	if (!requireNamespace("leaflet")) stop("grid package required but not installed yet.")
	#bbx = unname(bbx)
	
	n = nrow * ncol
	
	lfs = lapply(1L:n, function(i) {
		leaflet::leaflet() %>% leaflet::addTiles()# %>% leaflet::fitBounds(bbx[1], bbx[2], bbx[3], bbx[4])
	})
	
	assign("lfs", lfs, envir = .TMAP_LEAFLET)
	assign("nrow", nrow, envir = .TMAP_LEAFLET)
	assign("ncol", ncol, envir = .TMAP_LEAFLET)
	NULL
}

get_facet_id = function(row, col, nrow, ncol) {
	col + (row - 1L) * ncol
}

get_lf = function(facet_row, facet_col) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)
	
	lfid = get_facet_id(facet_row, facet_col, nrow, ncol)
	
	lfs[[lfid]]
}

assign_lf = function(lf) {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)
	
	lfid = get_facet_id(facet_row, facet_col, nrow, ncol)
	
	lfs[[lfid]] = lf
	assign("lfs", lfs, envir = .TMAP_LEAFLET)
	NULL
}


tmapLeafletShape = function(bbx, facet_row, facet_col) {
	get_lf(facet_row, facet_col) %>% 
		leaflet::fitBounds(bbx[1], bbx[2], bbx[3], bbx[4]) %>% 
		assign_lf()
	NULL
}



tmapLeafletPolygons = function(shpTM, dt, facet_row, facet_col) {
	lf = get_lf(facet_row, facet_col)
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	fill = if ("fill" %in% names(dt)) dt$fill else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	lf %>% 
		leaflet::addPolygons(data = shp, color = color, fillColor = fill) %>% 
		assign_lf()
	NULL	
}


tmapLeafletSymbols = function(shpTM, dt, facet_row, facet_col) {
	lf = get_lf(facet_row, facet_col)
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	size = if ("size" %in% names(dt)) dt$size else rep(NA, nrow(dt))
	shape = if ("shape" %in% names(dt)) dt$shape else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	coords = sf::st_coordinates(shp)

		
	lf %>% 
		leaflet::addCircleMarkers(lng = coords[, 1], lat = coords[, 2], fillColor = color, radius = size) %>% 
		assign_lf()
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

tmapLeafletRaster = function(shpTM, dt, facet_row, facet_col) {

	rc_text = frc(facet_row, facet_col)
	
	
	bb_target <- bbx #attr(shp, "bbox")
	bb_real <- bbx #sf::st_bbox(shp)
	
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	
	if (is_regular_grid(shp)) {
		color = rep("#FFFFFF", length(tmapID))
		color[match(dt$tmapID__, tmapID)] = dt$color
		
		pal <- na.omit(unique(color))
		pal <- pal[substr(pal, 8,10)!="00"] ## remove transparant colors
		
		res <- split_alpha_channel(pal, alpha = 1)
		pal_col <- res$col
		pal_opacity <- if (length(res$opacity) == 0L) 0 else max(res$opacity)
		
		col_ids <- match(color, pal)
		
		shp[[1]] <- matrix(col_ids, ncol = ncol(shp))
		
		lf = get_lf(facet_row, facet_col)
		lf %>% 
			leafem::addStarsImage(shp, band = 1, colors = pal) %>% 
			assign_lf()
	} else {
		shpTM <- shapeTM(sf::st_as_sf(shp), tmapID)
		tmapLeafletPolygons(shpTM, dt)
		#grid.shape(s, gp=gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
} 


tmapLeafletRun = function() {
	lfs = get("lfs", envir = .TMAP_LEAFLET)
	nrow = get("nrow", envir = .TMAP_LEAFLET)
	ncol = get("ncol", envir = .TMAP_LEAFLET)
	
	if (nrow == 1 && ncol == 1) {
		print(lfs[[1]])
	} else {
		print(do.call(leafsync::sync(c(lfs, list(ncol = ncol)))))
	}
}
