tmapGridInit = function(ncol, nrow) {
	if (!requireNamespace("grid")) stop("grid package required but not installed yet.")
	#grid.newpage()
	
	devsize = dev.size()
	dasp = devsize[1] / devsize[2]
	
	
	fasp = dasp * nrow / ncol
	
	if (dasp > 1) {
		cw <- dasp
		ch <- 1
	} else {
		ch <- 1/dasp
		cw <- 1
	}
	
	vp_tree = grid::vpStack(grid::viewport(width = grid::unit(cw, "snpc"), height = grid::unit(ch, "snpc"), name = "vp_container"),
							grid::viewport(layout = grid::grid.layout(nrow = nrow, ncol = ncol), name = "vp_facets"))
	
	#gt = grobTree(rectGrob(gp=gpar(fill="red")), vp = vp_tree)
	
	gt = grid::grobTree(grid::grobTree(name = "gt_facets"), 
						grid::rectGrob(gp=grid::gpar(col="red", lwd = 4, fill = NA), name = "red_frame"),
						vp = vp_tree, name = "tmap_grob_tree")
	
	# gt = grid::grobTree(grid::grobTree(name = "gt_map"), 
	# 					grid::rectGrob(gp=gpar(col="red", lwd = 4, fill = NA), name = "red_frame"),
	# 					vp = vp_tree, name = "tmap_grob_tree")
	
	assign("devsize", devsize, envir = .TMAP_GRID)
	assign("dasp", dasp, envir = .TMAP_GRID)
	assign("fasp", fasp, envir = .TMAP_GRID)
	assign("gt", gt, envir = .TMAP_GRID)
	#assign("bbx", bbx, envir = .TMAP_GRID)

	NULL
}

frc = function(row, col) paste0(sprintf("%02d", row), "_", sprintf("%02d", col))


tmapGridShape = function(bbx, facet_row, facet_col) {
	gt = get("gt", .TMAP_GRID)
	fasp = get("fasp", .TMAP_GRID)
	
	sasp = (bbx[3] - bbx[1]) / (bbx[4] - bbx[2])

	if (sasp > fasp) {
		width = 1
		height = fasp / sasp
	} else {
		height = 1
		width = sasp / fasp
	}
	rc_text = frc(facet_row, facet_col)
	
	gtmap = grid::grobTree(grid::rectGrob(gp=grid::gpar(col="blue", lwd = 4, fill = NA), name = paste0("blue_rect_", rc_text)),
						   vp = grid::vpStack(grid::viewport(layout.pos.col = facet_col, layout.pos.row = facet_row, name = paste0("vp_facet_", rc_text)),
						   				   grid::viewport(width = width, height = height, xscale = bbx[c(1,3)], yscale = bbx[c(2,4)], name = paste0("vp_map_", rc_text), clip = TRUE)), name = paste0("gt_facet_", rc_text))
	
	gt = grid::addGrob(gt, gtmap, gPath = grid::gPath("gt_facets"))
	
	#assign("devsize", devsize, envir = .TMAP_GRID)
	#assign("dasp", dasp, envir = .TMAP_GRID)
	assign("gt", gt, envir = .TMAP_GRID)
	assign("bbx", bbx, envir = .TMAP_GRID)
	NULL
}


select_sf = function(shpTM, dt) {
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	
	tmapIDdt = dt$tmapID__
	
	tid = intersect(tmapID, tmapIDdt)
	
	shpSel = shp[match(tid, tmapID)] #st_cast(shp[match(tid, tmapID)], "MULTIPOLYGON")
	
	
	dt = dt[match(tid, tmapIDdt), ]
	list(shp = shpSel, dt = dt)
}

tmapGridPolygons = function(shpTM, dt, facet_row, facet_col) {
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	fill = if ("fill" %in% names(dt)) dt$fill else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	gp = grid::gpar(fill = fill, col = color) # lwd=border.lwd, lty=border.lty)
	#grb = gTree(sf::st_as_grob(shpSel, gp = gp), name = "polygons")
	
	grb = sf::st_as_grob(shp, gp = gp, name = "polygons")
	gt = get("gt", .TMAP_GRID)
	
	gt_name = paste0("gt_facet_", rc_text)
	gt$children$gt_facets$children[[gt_name]]$children = appendGlist(gt$children$gt_facets$children[[gt_name]]$children, grb)
	gt$children$gt_facets$children[[gt_name]]$childrenOrder = names(gt$children$gt_facets$children[[gt_name]]$children)
	
	assign("gt", gt, envir = .TMAP_GRID)
	NULL	
}

appendGlist = function(glist, x) {
	glist = grid::gList(glist, x)
	names(glist)[length(glist)] = x$name
	glist
}


tmapGridSymbols = function(shpTM, dt, facet_row, facet_col) {
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	size = if ("size" %in% names(dt)) dt$size else rep(NA, nrow(dt))
	shape = if ("shape" %in% names(dt)) dt$shape else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	coords = sf::st_coordinates(shp)
	
	gp = grid::gpar(fill = color) # lwd=border.lwd, lty=border.lty)
	grb = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = shape, size = grid::unit(size, "native"), gp = gp, name = "symbols")
	
	gt = get("gt", .TMAP_GRID)
	
	gt_name = paste0("gt_facet_", rc_text)
	gt$children$gt_facets$children[[gt_name]]$children = appendGlist(gt$children$gt_facets$children[[gt_name]]$children, grb)
	gt$children$gt_facets$children[[gt_name]]$childrenOrder = names(gt$children$gt_facets$children[[gt_name]]$children)
	
	
	#gt = grid::addGrob(gt, grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
	
	assign("gt", gt, envir = .TMAP_GRID)
	NULL	
	
}


tmapGridRaster <- function(shpTM, dt, facet_row, facet_col) {
	gt = get("gt", .TMAP_GRID)
	bbx = get("bbx", .TMAP_GRID)

	rc_text = frc(facet_row, facet_col)
	
	
	bb_target <- bbx #attr(shp, "bbox")
	bb_real <- bbx #sf::st_bbox(shp)
	
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	
	if (is_regular_grid(shp)) {
		color = rep("#FFFFFF", length(tmapID))
		color[match(dt$tmapID__, tmapID)] = dt$color
		
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
		
		y_is_neg <- all(diff(st_get_dimension_values(shp, "y")) < 0)
		if (!y_is_neg) {
			m <- m[nrow(m):1L, ]
		}
		
		grb = grid::rasterGrob(m, x=cx, y=cy, width=width, height=height, interpolate = FALSE) #gpl$raster.misc$interpolate
		gt = grid::addGrob(gt, grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
		assign("gt", gt, envir = .TMAP_GRID)
	} else {
		shp[[1]][tmapID] = tmapID
		shpTM <- shapeTM(sf::st_geometry(sf::st_as_sf(shp)), tmapID)
		tmapGridPolygons(shpTM, dt, facet_row, facet_col)
		#grid.shape(s, gp=gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
} 




tmapGridRun = function() {
	gt = get("gt", .TMAP_GRID)
	grid::grid.newpage()
	grid::grid.draw(gt)
}
