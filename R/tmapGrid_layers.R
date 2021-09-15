rescale_gp = function(gp, scale, skip = character()) {
	if ("lwd" %in% names(gp) && (!"lwd" %in% skip)) gp$lwd = gp$lwd * scale
	if ("size" %in% names(gp) && (!"size" %in% skip)) gp$size = gp$size * sqrt(scale)
	if ("cex" %in% names(gp) && (!"cex" %in% skip)) gp$cex = gp$cex * sqrt(scale)
	gp
}

tmapGridPolygons = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, o) {
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	gp = impute_gp(gp, dt)
	
	gp = rescale_gp(gp, o$scale_down)
	
	# none should contain NA's && (length or content should be different)
	diffAlpha = !any(is.na(c(gp$fill_alpha, gp$col_alpha))) && !(length(gp$fill_alpha) == length(gp$col_alpha) && all(gp$fill_alpha == gp$col_alpha))
	
	
	if (diffAlpha) {
		gp1 = gp_to_gpar(gp, sel = "fill")
		gp2 = gp_to_gpar(gp, sel = "col")
		grb1 = sf::st_as_grob(shp, gp = gp1, name = paste0("polygons_", id))
		grb2 = sf::st_as_grob(shp, gp = gp2, name = paste0("polygon_borders_", id))
		grb = grid::grobTree(grb1, grb2)
	} else {
		gp = gp_to_gpar(gp, sel = "all")
		grb = sf::st_as_grob(shp, gp = gp, name = paste0("polygons_", id))
	}
	
	
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))
	
	gts[[facet_page]] = gt
	
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
}

appendGlist = function(glist, x) {
	glist = grid::gList(glist, x)
	names(glist)[length(glist)] = x$name
	glist
}


tmapGridSymbols = function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, o) {
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	gp = impute_gp(gp, dt)
	
	gp = gp_to_gpar(gp, sel = "all")
	
	gp = rescale_gp(gp, o$scale_down)
	
	
	coords = sf::st_coordinates(shp)
	
	grb = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = gp$shape, size = grid::unit(gp$size, "lines"), gp = gp, name = paste0("symbols_", id))
	
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath(gt_name))
	
	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
	
}


zero_one_to_hex = function(x) {
	u = unique(x)
	
	x255 = round(u * 255)
	
	nc = c(0:9, LETTERS[1:6])
	
	y1 = (x255 %/% 16) + 1
	y2 = (x255 - (y1 - 1) * 16) + 1
	
	r = paste0(nc[y1], nc[y2])
	r[match(x, u)]
}


tmapGridRaster <- function(shpTM, dt, gp, bbx, facet_row, facet_col, facet_page, id, o) {
	gts = get("gts", .TMAP_GRID)
	#bbx = get("bbx", .TMAP_GRID)
	
	g = get("g", .TMAP_GRID)
	
	g$fasp
	
	
	rc_text = frc(facet_row, facet_col)
	
	
	bb_target <- bb_asp(bbx, g$fasp)
	bb_real <-  stm_bbox_all(shpTM)
	
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	
	if (is_regular_grid(shp)) {
		
		if (nrow(dt) == length(tmapID)) {
			# shortcut for else case
			alpha = zero_one_to_hex(dt$col_alpha)
			
			color = paste0(dt$col, alpha)
			#color = rgb(col[,1], col[,2], col[,3], alpha = alpha, maxColorValue = 255)
			
			
		} else {
			# to be improved
			tid = intersect(tmapID, dt$tmapID__)
			
			color = rep(NA, length(tmapID)) #"#FFFFFF"
			
			sel = which(tmapID %in% tid)
			tid2 = tmapID[sel]
			
			color[sel] = dt$col[match(tid2, dt$tmapID__)]
		}
		
		
		if (all(abs(bb_real-bb_target)< 1e-3)) {
			width <- 1
			height <- 1
			cent <- c(mean.default(c(bb_target[1], bb_target[3])), mean.default(c(bb_target[2], bb_target[4])))
		} else {
			width <- (bb_real[3] - bb_real[1]) / (bb_target[3] - bb_target[1])
			height <- (bb_real[4] - bb_real[2]) / (bb_target[4] - bb_target[2])
			cent <- c(mean.default(c(bb_real[1], bb_real[3])), mean.default(c(bb_real[2], bb_real[4])))
		}
		
		cx <- (cent[1] - bb_target[1]) / (bb_target[3] - bb_target[1])
		cy <- (cent[2] - bb_target[2]) / (bb_target[4] - bb_target[2])
		
		m <- matrix(color, ncol=nrow(shp), nrow=ncol(shp), byrow = TRUE)
		
		y_is_neg <- all(diff(stars::st_get_dimension_values(shp, "y")) < 0)
		if (!y_is_neg) {
			m <- m[nrow(m):1L, ]
		}
		m[is.na(m)] = NA #"#0000FF"
		
		grb = grid::rasterGrob(m, x=cx, y=cy, width=width, height=height, interpolate = FALSE, name = paste0("raster_", id)) #gpl$raster.misc$interpolate
		gt = grid::addGrob(gts[[facet_page]], grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
		gts[[facet_page]] = gt
		assign("gts", gts, envir = .TMAP_GRID)
	} else {
		shp[[1]][tmapID] = tmapID
		shpTM <- shapeTM(sf::st_geometry(sf::st_as_sf(shp)), tmapID)
		tmapGridPolygons(shpTM, dt, facet_row, facet_col, facet_page, id, o)
		#grid.shape(s, gp=grid::gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
} 
