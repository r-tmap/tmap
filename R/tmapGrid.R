tmapGridInit = function(bbx) {
	if (!requireNamespace("grid")) stop("grid package required but not installed yet.")
	#grid.newpage()
	
	devsize = dev.size()
	dasp = devsize[1] / devsize[2]
	
	if (dasp > 1) {
		cw <- dasp
		ch <- 1
	} else {
		ch <- 1/dasp
		cw <- 1
	}
	
	sasp = (bbx[3] - bbx[1]) / (bbx[4] - bbx[2])
	
	if (sasp > dasp) {
		width = 1
		height = dasp / sasp
	} else {
		height = 1
		width = sasp / dasp
	}
	
	vp_tree = grid::vpStack(grid::viewport(width = grid::unit(cw, "snpc"), height = grid::unit(ch, "snpc"), name = "vp_container"),
							grid::viewport(width = width, height = height, xscale = bbx[c(1,3)], yscale = bbx[c(2,4)], name = "vp_map", clip = TRUE))
	
	#gt = grobTree(rectGrob(gp=gpar(fill="red")), vp = vp_tree)
	
	gt = grid::grobTree(grid::grobTree(name = "gt_map"), 
						grid::rectGrob(gp=gpar(col="red", lwd = 4, fill = NA), name = "red_frame"),
						vp = vp_tree, name = "tmap_grob_tree")
	
	assign("devsize", devsize, envir = .TMAP_GRID)
	assign("dasp", dasp, envir = .TMAP_GRID)
	assign("gt", gt, envir = .TMAP_GRID)
	
	NULL
}

tmapGridPolygons = function(x, fill, color) {
	geoms = sf::st_geometry(x)
	gp = grid::gpar(fill = fill, col = color) # lwd=border.lwd, lty=border.lty)
	grb = sf::st_as_grob(geoms, gp = gp, name = "polygons")
	
	
	gt = get("gt", .TMAP_GRID)
	
	gt = grid::addGrob(gt, grb, gPath = grid::gPath("gt_map"))

	assign("gt", gt, envir = .TMAP_GRID)
	NULL	
}

tmapGridRun = function() {
	gt = get("gt", .TMAP_GRID)
	grid.newpage()
	grid::grid.draw(gt)
}
