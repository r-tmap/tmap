library(grid)

o = tmap_options()

# calculate margins (using grid system)
o = process_margins(o)

#o$nby = get_nby(o$fl)
tf = tm_facets_wrap()
o = c(o, tf[[1]])


o$n = 9#prod(o$nby)
o$asp = NA
o$ncols = NA
o$nrows = NA
o$sasp = 2
o$npages = 1
o = how_many_rows(o)
#o$x
#o$nrows = 6
#o$meta.margins = c(0.2,0.1,0,0)
#o$panel.type = "wrap"

tmapGridInit2(o)

gts = get("gts", envir = .TMAP_GRID)
g = get("g", envir = .TMAP_GRID)

bbx = sf::st_bbox(World)


g$fasp






tmapGridShape = function(bbx, facet_row, facet_col, facet_page) {
	gts = get("gts", envir = .TMAP_GRID)
	g = get("g", envir = .TMAP_GRID)


	basp = (bbx[3] - bbx[1]) / (bbx[4] - bbx[2])
	
	fbbx = bb_asp(bbx, g$fasp)
	
	rc_text = frc(facet_row, facet_col)
	
	rowid = g$rows_facet_ids[facet_row]
	colid = g$cols_facet_ids[facet_col]
	
	
	gtmap = grid::grobTree(grid::rectGrob(gp=grid::gpar(col="blue", lwd = 4, fill = NA), name = paste0("blue_rect_", rc_text)),
						   vp = grid::vpStack(grid::viewport(layout.pos.col = colid, layout.pos.row = rowid, name = paste0("vp_facet_", rc_text)),
						   				   grid::viewport(xscale = fbbx[c(1,3)], yscale = fbbx[c(2,4)], name = paste0("vp_map_", rc_text), clip = TRUE)), name = paste0("gt_facet_", rc_text))
	
	gts[[facet_page]] = grid::addGrob(gts[[facet_page]], gtmap, gPath = grid::gPath("gt_facets"))
	
	#assign("devsize", devsize, envir = .TMAP_GRID)
	#assign("dasp", dasp, envir = .TMAP_GRID)
	assign("gts", gts, envir = .TMAP_GRID)
	#assign("bbx", bbx, envir = .TMAP_GRID)
	
		
}
