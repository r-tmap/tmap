tmapGridInit2 = function(o) {
	if (!requireNamespace("grid")) stop("grid package required but not installed yet.")
	#grid.newpage()
	
	devsize = dev.size()
	dasp = devsize[1] / devsize[2]
	
	
	fasp = dasp * o$nrows / o$ncols
	
	if (dasp > 1) {
		cw <- dasp
		ch <- 1
	} else {
		ch <- 1/dasp
		cw <- 1
	}
	
	
	lineH = convertHeight(unit(1, "lines"), unitTo = "inch")
	
	buffer = lineH / 2
	
	meta.buffers = sign(o$meta.margins) * buffer # outside and inside
	# panel.buffers = if (o$panel.type == "wrap") {
	# 	as.integer(o$panel.wrap.pos == c("bottom", "left", "top", "right")) * buffer
	# } else if (o$panel.type == "xtab") {
	# 	as.integer(c("bottom", "left", "top", "right") %in% o$panel.xtab.pos) * buffer
	# } else rep(0, 4) # outside only
	
	
	panel.xtab.size = if (o$panel.type == "xtab") {
		c(ifelse("bottom" %in% o$panel.xtab.pos, o$panel.label.height * lineH, 0),
		  ifelse("left" %in% o$panel.xtab.pos, o$panel.label.height * lineH, 0),
		  ifelse("top" %in% o$panel.xtab.pos, o$panel.label.height * lineH, 0),
		  ifelse("right" %in% o$panel.xtab.pos, o$panel.label.height * lineH, 0))
	} else c(0, 0, 0, 0)
	
	panel.wrap.size = if (o$panel.type == "wrap") {
		c(ifelse(o$panel.wrap.pos == "bottom", o$panel.label.height * lineH, 0),
		  ifelse(o$panel.wrap.pos == "left", o$panel.label.height * lineH, 0),
		  ifelse(o$panel.wrap.pos == "top", o$panel.label.height * lineH, 0),
		  ifelse(o$panel.wrap.pos == "right", o$panel.label.height * lineH, 0))
	} else c(0, 0, 0, 0)
	
	xylab.margins = rep(0, 4)
	if (o$xlab.show) xylab.margins[ifelse(o$xlab.pos == "bottom", 1, 3)] = o$xylab.height * lineH
	if (o$ylab.show) xylab.margins[ifelse(o$xlab.pos == "left", 2, 4)] = o$xylab.height * lineH
	
	
	grid.buffers = if (o$grid.show) {
		as.integer(o$grid.label.pos == c("bottom", "left", "top", "right")) * buffer
	} else {
		rep(0, 4)
	}
	
	grid.margins = if (o$grid.show) {
		as.integer(o$grid.label.pos == c("bottom", "left", "top", "right")) * o$grid.mark.height * lineH
	} else {
		rep(0, 4)
	}
	between.margin = o$between.margins * lineH
	
	
	
	
	rows = with(o, {
		x = c(outer.margins.top = outer.margins[3],
			  meta.buffers.top.out = meta.buffers[3],
			  meta.margins.top = meta.margins[3],
			  meta.buffers.top.in = meta.buffers[3],
			  xylab.margins.top = xylab.margins[3],
			  
			  #panel.buffers.top = panel.buffers[3],
			  panel.xtab.top = panel.xtab.size[3],
			  grid.buffers.top = grid.buffers[3],
			  grid.margins.top = grid.margins[3],
			  
			  {if (o$nrows > 1) rep(c(panel.wrap.size[3], 1, panel.wrap.size[1], between.margin), o$nrows -1) else NULL},
			  panel.wrap.size[3], 1, panel.wrap.size[1],
			  
			  grid.margins.top = grid.margins[1],
			  grid.buffers.top = grid.buffers[1],
			  panel.xtab.bottom = panel.xtab.size[1],
			  #panel.buffers.bottom = panel.buffers[1],
			  
			  xylab.margins.bottom = xylab.margins[1],
			  meta.buffers.bottom.in = meta.buffers[1],
			  meta.margins.bottom = meta.margins[1],
			  meta.buffers.bottom.out = meta.buffers[1],
			  outer.margins.bottom = outer.margins[1])
		types = c("npc", "inch", "npc", "inch", "inch", 
				  "inch", "inch", "inch",
				  
				  {if (o$nrows > 1) rep(c("inch", "null", "inch", "inch"), o$nrows - 1) else NULL},
				  "inch", "null", "inch",
				  
				  "inch", "inch", "inch",
				  "inch", "inch", "npc", "inch", "npc")
		
		u = unit(x, types)
		print(u)
		
		u2 = convertHeight(u, "npc")
		
		
		#isplot = which(types == "null")
		
		#u2[isplot] = (unit(1,"npc") - sum(u2)) / length(isplot)
		
		names(u2) = names(x)
		u2
	})
	
	
	cols = with(o, {
		x = c(outer.margins.left = outer.margins[2],
			  meta.buffers.left.out = meta.buffers[2],
			  meta.margins.left = meta.margins[2],
			  meta.buffers.left.in = meta.buffers[2],
			  xylab.margins.left = xylab.margins[2],
			  
			  #panel.buffers.left = panel.buffers[2],
			  panel.xtab.left = panel.xtab.size[2],
			  grid.buffers.left = grid.buffers[2],
			  grid.margins.left = grid.margins[2],
			  
			  {if (o$ncols > 1) rep(c(panel.wrap.size[2], 1, panel.wrap.size[4], between.margin), o$ncols -1) else NULL},
			  panel.wrap.size[2], 1, panel.wrap.size[4],
			  
			  grid.margins.left = grid.margins[4],
			  grid.buffers.left = grid.buffers[4],
			  panel.xtab.right = panel.xtab.size[4],
			  #panel.buffers.right = panel.buffers[4],
			  
			  xylab.margins.right = xylab.margins[4],
			  meta.buffers.right.in = meta.buffers[4],
			  meta.margins.right = meta.margins[4],
			  meta.buffers.right.out = meta.buffers[4],
			  outer.margins.right = outer.margins[4])
		types = c("npc", "inch", "npc", "inch", "inch", 
				  "inch", "inch", "inch",
				  
				  {if (o$ncols > 1) rep(c("inch", "null", "inch", "inch"), o$ncols - 1) else NULL},
				  "inch", "null", "inch",
				  
				  "inch", "inch", "inch",
				  "inch", "inch", "npc", "inch", "npc")
		
		u = unit(x, types)
		
		u2 = convertWidth(u, "npc")
		
		#isplot = which(types == "null")
		
		#u2[isplot] = (unit(1,"npc") - sum(u2)) / length(isplot)
		
		names(u2) = names(x)
		u2
	})
	
	cols_facet_ids = 1:o$ncols * 4 + 6
	rows_facet_ids = 1:o$ncols * 4 + 6
	
	
	sum(rows[rows_facet_ids])
	
	#cols = convertWidth(cols, "inch")
	#rows = convertHeight(rows, "inch")
	
	sum(cols)
	sum(rows)
	
	names(rows)
	
	prows = as.numeric(rows)
	pcols = as.numeric(cols)
	
	fasp = ((1-sum(pcols)) / (1-sum(prows))) * dasp / o$ncols * o$nrows
	gasp = ((1-sum(pcols)) / (1-sum(prows))) * dasp
	
	
	if (o$asp == 0) {
		# follow device
		o$asp = fasp
	}
	print("fasp", fasp)
	
	gasp2 = o$asp * o$ncols / o$nrows

	
	print("gasp")
	print(gasp)
	print("gasp2")
	print(gasp2)
	


	if (gasp2 > gasp) {
		extra.height =   (1 - ((1 - sum(pcols))/(gasp2/dasp))) - sum(prows)
		rows[c(1, length(rows))] = rows[c(1, length(rows))] + unit(extra.height / 2, "npc")
	} else if (gasp2 < gasp) {
		extra.width =   (1 - ((1 - sum(prows)) * (gasp2/dasp))) - sum(pcols)
		cols[c(1, length(cols))] = cols[c(1, length(cols))] + unit(extra.width / 2, "npc")
	}
	cols[cols_facet_ids] = (unit(1, "npc") - sum(cols)) / o$ncols
	rows[rows_facet_ids] = (unit(1, "npc") - sum(rows)) / o$nrows
	
	
	#rows[rows_facet_ids] = unit()
	
	
	
	vp_tree = grid::vpStack(grid::viewport(width = grid::unit(cw, "snpc"), height = grid::unit(ch, "snpc"), name = "vp_asp"),
							#grid::viewport(layout = grid::grid.layout(nrow = length(rows), ncol = length(cols)), name = "vp_main")
							grid::viewport(layout = grid::grid.layout(nrow = length(rows), ncol = length(cols), widths = cols, heights = rows), name = "vp_main")
	)
	
	#gt = grobTree(rectGrob(gp=gpar(fill="red")), vp = vp_tree)
	
	gt = grid::grobTree(grid::grobTree(name = "gt_main"), 
						grid::rectGrob(gp=grid::gpar(col="red", lwd = 1, fill = NA), name = "red_frame"),
						vp = vp_tree, name = "tmap_grob_tree")
	
	e = environment()
	
	add_to_gt = function(grb, row, col) {
		vp = viewport(layout.pos.col = col, layout.pos.row = row)
		gtr = grobTree(grb, vp = vp)
		
		gt = grid::addGrob(gt, gtr, gPath = grid::gPath("gt_main"))
		assign("gt", gt, envir = e)
		
	}
	
	length(rows)
	length(cols)
	
	prows = as.numeric(rows)
	pcols = as.numeric(cols)
	
	names(prows) = names(rows)
	names(pcols) = names(cols)
	
	cat("rows:\n")
	print(prows)
	cat("cols:\n")
	print(pcols)
	
	#scales::show_col(pals::brewer.paired(12))
	p = rep(pals::brewer.paired(12), 3)
	
	nr = length(rows)
	nc = length(cols)
	
	add_to_gt(rectGrob(gp=gpar(fill = p[1])), row = 1:(nr), col = 1:(nc)) # outer
	add_to_gt(rectGrob(gp=gpar(fill = p[2])), row = 2:(nr-1), col = 2:(nc-1)) # meta buffer out
	add_to_gt(rectGrob(gp=gpar(fill = p[3])), row = 3:(nr-2), col = 3:(nc-2)) # meta margins
	add_to_gt(rectGrob(gp=gpar(fill = p[2])), row = 4:(nr-3), col = 4:(nc-3)) # meta buffer in
	
	add_to_gt(rectGrob(gp=gpar(fill = p[4])), row = 5:(nr-4), col = 5:(nc-4)) # xyab
	if (o$panel.type == "xtab") {
		#add_to_gt(rectGrob(gp=gpar(fill = p[5])), row = 6:(nr-5), col = 6:(nc-5)) # panel buffer
		add_to_gt(rectGrob(gp=gpar(fill = p[5])), row = 6:(nr-5), col = 6:(nc-5)) # panel
	}
	
	add_to_gt(rectGrob(gp=gpar(fill = p[6])), row = 7:(nr-6), col = 7:(nc-6)) # grid buffer
	add_to_gt(rectGrob(gp=gpar(fill = p[7])), row = 8:(nr-7), col = 8:(nc-7)) # grid
	
	
	for (i in 1:o$nrows) {
		for (j in 1:o$ncols) {
			add_to_gt(rectGrob(gp=gpar(fill = p[11])), row = i * 4 + 6, col = j * 4 + 6)
			if (o$panel.type == "wrap") {
				if (o$panel.wrap.pos == "top") {
					add_to_gt(rectGrob(gp=gpar(fill = p[5])), row = i * 4 + 5, col = j * 4 + 5:7)
				} else if (o$panel.wrap.pos == "bottom") {
					add_to_gt(rectGrob(gp=gpar(fill = p[5])), row = i * 4 + 7, col = j * 4 + 5:7)
				}  else if (o$panel.wrap.pos == "left") {
					add_to_gt(rectGrob(gp=gpar(fill = p[5])), row = i * 4 + 5:7, col = j * 4 + 5)
				}  else {
					add_to_gt(rectGrob(gp=gpar(fill = p[5])), row = i * 4 + 5:7, col = j * 4 + 7)
				}
			}
			
		}
	}
	
	
	grid.newpage()
	grid.draw(gt)
	
}




tmapGridInit = function(o) {
	if (!requireNamespace("grid")) stop("grid package required but not installed yet.")
	#grid.newpage()
	
	devsize = dev.size()
	dasp = devsize[1] / devsize[2]
	
	
	fasp = dasp * o$nrows / o$ncols
	
	if (dasp > 1) {
		cw <- dasp
		ch <- 1
	} else {
		ch <- 1/dasp
		cw <- 1
	}
	
	gts = lapply(1L:o$npages, function(ip) {
		vp_tree = grid::vpStack(grid::viewport(width = grid::unit(cw, "snpc"), height = grid::unit(ch, "snpc"), name = "vp_container"),
								grid::viewport(layout = grid::grid.layout(nrow = o$nrows, ncol = o$ncols), name = "vp_facets"))
		
		#gt = grobTree(rectGrob(gp=gpar(fill="red")), vp = vp_tree)
		
		grid::grobTree(grid::grobTree(name = "gt_facets"), 
							grid::rectGrob(gp=grid::gpar(col="red", lwd = 4, fill = NA), name = "red_frame"),
							vp = vp_tree, name = "tmap_grob_tree")
	})
	
	# gt = grid::grobTree(grid::grobTree(name = "gt_map"), 
	# 					grid::rectGrob(gp=gpar(col="red", lwd = 4, fill = NA), name = "red_frame"),
	# 					vp = vp_tree, name = "tmap_grob_tree")
	
	assign("devsize", devsize, envir = .TMAP_GRID)
	assign("dasp", dasp, envir = .TMAP_GRID)
	assign("fasp", fasp, envir = .TMAP_GRID)
	assign("gts", gts, envir = .TMAP_GRID)
	#assign("bbx", bbx, envir = .TMAP_GRID)

	NULL
}

frc = function(row, col) paste0(sprintf("%02d", row), "_", sprintf("%02d", col))


tmapGridShape = function(bbx, facet_row, facet_col, facet_page) {
	gts = get("gts", .TMAP_GRID)
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
	
	gts[[facet_page]] = grid::addGrob(gts[[facet_page]], gtmap, gPath = grid::gPath("gt_facets"))
	
	#assign("devsize", devsize, envir = .TMAP_GRID)
	#assign("dasp", dasp, envir = .TMAP_GRID)
	assign("gts", gts, envir = .TMAP_GRID)
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

tmapGridPolygons = function(shpTM, dt, facet_row, facet_col, facet_page) {
	
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	fill = if ("fill" %in% names(dt)) dt$fill else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	gp = grid::gpar(fill = fill, col = color) # lwd=border.lwd, lty=border.lty)
	#grb = gTree(sf::st_as_grob(shpSel, gp = gp), name = "polygons")
	
	grb = sf::st_as_grob(shp, gp = gp, name = "polygons")
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	gt$children$gt_facets$children[[gt_name]]$children = appendGlist(gt$children$gt_facets$children[[gt_name]]$children, grb)
	gt$children$gt_facets$children[[gt_name]]$childrenOrder = names(gt$children$gt_facets$children[[gt_name]]$children)

	gts[[facet_page]] = gt
		
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
}

appendGlist = function(glist, x) {
	glist = grid::gList(glist, x)
	names(glist)[length(glist)] = x$name
	glist
}


tmapGridSymbols = function(shpTM, dt, facet_row, facet_col, facet_page) {
	rc_text = frc(facet_row, facet_col)
	
	res = select_sf(shpTM, dt)
	shp = res$shp
	dt = res$dt
	
	size = if ("size" %in% names(dt)) dt$size else rep(NA, nrow(dt))
	shape = if ("shape" %in% names(dt)) dt$shape else rep(NA, nrow(dt))
	color = if ("color" %in% names(dt)) dt$color else rep(NA, nrow(dt))
	
	
	coords = sf::st_coordinates(shp)
	
	gp = grid::gpar(fill = color, col = "green")
	grb = grid::pointsGrob(x = grid::unit(coords[,1], "native"), y = grid::unit(coords[,2], "native"), pch = shape, size = grid::unit(size, "lines"), gp = gp, name = "symbols")
	
	gts = get("gts", .TMAP_GRID)
	gt = gts[[facet_page]]
	
	gt_name = paste0("gt_facet_", rc_text)
	gt$children$gt_facets$children[[gt_name]]$children = appendGlist(gt$children$gt_facets$children[[gt_name]]$children, grb)
	gt$children$gt_facets$children[[gt_name]]$childrenOrder = names(gt$children$gt_facets$children[[gt_name]]$children)
	
	
	#gt = grid::addGrob(gt, grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
	gts[[facet_page]] = gt
	assign("gts", gts, envir = .TMAP_GRID)
	NULL	
	
}


tmapGridRaster <- function(shpTM, dt, facet_row, facet_col, facet_page) {
	gts = get("gts", .TMAP_GRID)
	bbx = get("bbx", .TMAP_GRID)

	rc_text = frc(facet_row, facet_col)
	
	
	bb_target <- bbx #attr(shp, "bbox")
	bb_real <-  stm_bbox_all(shpTM)
	
	shp = shpTM$shp
	tmapID = shpTM$tmapID
	
	if (is_regular_grid(shp)) {
		
		tid = intersect(tmapID, dt$tmapID__)
		
		color = rep(NA, length(tmapID)) #"#FFFFFF"
		
		sel = which(tmapID %in% tid)
		tid2 = tmapID[sel]
		
		color[sel] = dt$color[match(tid2, dt$tmapID__)]
		
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
		m[is.na(m)] = NA #"#0000FF"
		grb = grid::rasterGrob(m, x=cx, y=cy, width=width, height=height, interpolate = FALSE) #gpl$raster.misc$interpolate
		gt = grid::addGrob(gts[[facet_page]], grb, gPath = grid::gPath(paste0("gt_facet_", rc_text)))
		gts[[facet_page]] = gt
		assign("gts", gts, envir = .TMAP_GRID)
	} else {
		shp[[1]][tmapID] = tmapID
		shpTM <- shapeTM(sf::st_geometry(sf::st_as_sf(shp)), tmapID)
		tmapGridPolygons(shpTM, dt, facet_row, facet_col, facet_page)
		#grid.shape(s, gp=gpar(fill=color, col=NA), bg.col=NA, i, k)
	}
	NULL
} 




tmapGridRun = function(o) {
	gts = get("gts", .TMAP_GRID)
	lapply(gts, function(gt) {
		grid::grid.newpage()
		grid::grid.draw(gt)
	})
}
