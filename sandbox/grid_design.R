library(grid)

o = list(nrows = 3,
		 ncols = 3,
		 outer.margins = c(0.02, 0.02, 0.02, 0.02),
		 inner.margins = NA,
		 meta.margins = c(0, 0.1, 0, 0.1),
		 between.margins = 0.5,
		 panel.label.height = 1.25,
		 xylab.height = 1.25,
		 coords.height = 1.25
		 )
		 
		 

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





library(grid)

rows = with(o, {
	x = c(outer.margins.top = outer.margins[3],
		  meta.margins.top = meta.margins[3],
		  panel.label.top = panel.label.height,
		  xylab.top = xylab.height,
		  coords.top = coords.height,
		  facets = 1,
		  outer.margins.bottom = outer.margins[3],
		  meta.margins.bottom = meta.margins[3],
		  panel.label.bottom = panel.label.height,
		  xylab.bottom = xylab.height,
		  coords.bottom = coords.height)
	u = unit(x, c("npc", "npc", "lines", "lines", "lines", "null", "lines", "lines", "lines", "npc", "npc"))
	names(u) = names(x)
	u
})

cols = with(o, {
	x = c(outer.margins.left = outer.margins[3],
		  meta.margins.left = meta.margins[3],
		  panel.label.left = panel.label.height,
		  xylab.left = xylab.height,
		  coords.left = coords.height,
		  facets = 1,
		  outer.margins.right = outer.margins[3],
		  meta.margins.right = meta.margins[3],
		  panel.label.right = panel.label.height,
		  xylab.right = xylab.height,
		  coords.right = coords.height)
	u = unit(x, c("npc", "npc", "lines", "lines", "lines", "null", "lines", "lines", "lines", "npc", "npc"))
	names(u) = names(x)
	u
})



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

vp_tree = grid::vpStack(grid::viewport(width = grid::unit(cw, "snpc"), height = grid::unit(ch, "snpc"), name = "vp_asp"),
							grid::viewport(layout = grid::grid.layout(nrow = length(rows), ncol = length(cols), widths = cols, heights = rows), name = "vp_main"))
	
	#gt = grobTree(rectGrob(gp=gpar(fill="red")), vp = vp_tree)
	
gt = grid::grobTree(grid::grobTree(name = "gt_facets"), 
			   grid::rectGrob(gp=grid::gpar(col="red", lwd = 4, fill = NA), name = "red_frame"),
			   vp = vp_tree, name = "tmap_grob_tree")



grid.draw(gt)




