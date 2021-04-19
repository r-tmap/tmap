library(grid)

o = list(nrows = 3,
		 ncols = 3,
		 outer.margins = c(0.02, 0.02, 0.02, 0.02),
		 inner.margins = NA,
		 meta.margins = c(0, 0, 0, 0.2),
		 between.margins = 0.5,
		 panel.label.height = 1.25,
		 grid.mark.height = 2,
		 xylab.height = 1.25,
		 coords.height = 1.25,
		 xlab.show = TRUE,
		 ylab.show = TRUE,
		 xlab.pos = "bottom",
		 ylab.pos = "right",
		 grid.show = TRUE,
		 grid.label.pos = c("right", "bottom"),
		 panel.type = "none", # or "wrap" or "xtab",
		 panel.wrap.pos = "top", # or "left", "right", "bottom"
		 panel.xtab.pos = c("left", "top")
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


lineH = convertHeight(unit(1, "lines"), unitTo = "inch")

buffer = lineH / 2

meta.buffers = sign(o$meta.margins) * buffer # outside and inside
panel.buffers = if (o$panel.type == "wrap") {
	as.integer(o$panel.wrap.pos == c("bottom", "left", "top", "right")) * buffer
} else if (o$panel.type == "xtab") {
	as.integer(c("bottom", "left", "top", "right") %in% o$panel.xtab.pos) * buffer
} else rep(0, 4) # outside only


panel.xtab.size = if (o$panel.type == "xtab") {
	c(ifelse("bottom" %in% o$panel.xtab.pos, panel.label.height * lineH, 0),
	  ifelse("left" %in% o$panel.xtab.pos, panel.label.height * lineH, 0),
	  ifelse("top" %in% o$panel.xtab.pos, panel.label.height * lineH, 0),
	  ifelse("right" %in% o$panel.xtab.pos, panel.label.height * lineH, 0))
} else c(0, 0, 0, 0)

panel.wrap.size = if (o$panel.type == "wrap") {
	c(ifelse(o$panel.wrap.pos == "bottom", panel.label.height * lineH, 0),
	  ifelse(o$panel.wrap.pos == "left", panel.label.height * lineH, 0),
	  ifelse(o$panel.wrap.pos == "top", panel.label.height * lineH, 0),
	  ifelse(o$panel.wrap.pos == "right", panel.label.height * lineH, 0))
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
		  
		  panel.buffers.top = panel.buffers[3],
		  panel.xtab.top = panel.xtab.size[3],
		  grid.buffers.top = grid.buffers[3],
		  grid.margins.top = grid.margins[3],

		  {if (o$nrows > 1) rep(c(panel.wrap.size[3], 1, panel.wrap.size[3], between.margin), o$nrows -1) else NULL},
		  panel.wrap.size[3], 1, panel.wrap.size[3],

		  grid.margins.top = grid.margins[1],
		  grid.buffers.top = grid.buffers[1],
		  panel.xtab.bottom = panel.xtab.size[1],
		  panel.buffers.bottom = panel.buffers[1],
		  
		  xylab.margins.bottom = xylab.margins[1],
		  meta.buffers.bottom.in = meta.buffers[1],
		  meta.margins.bottom = meta.margins[1],
		  meta.buffers.bottom.out = meta.buffers[1],
		  outer.margins.bottom = outer.margins[1])
	types = c("npc", "inch", "npc", "inch", "inch", 
			"inch", "inch", "inch", "inch",
			
			{if (o$nrows > 1) rep(c("inch", "null", "inch", "inch"), o$nrows - 1) else NULL},
			"inch", "null", "inch",
			
			"inch", "inch", "inch", "inch",
			"inch", "inch", "npc", "inch", "npc")
	
	u = unit(x, types)
	
	u2 = convertHeight(u, "npc")
	
	
	isplot = which(types == "null")

	u2[isplot] = (unit(1,"npc") - sum(u2)) / length(isplot)
	
	names(u2) = names(x)
	u2
})


cols = with(o, {
	x = c(outer.margins.left = outer.margins[2],
		  meta.buffers.left.out = meta.buffers[2],
		  meta.margins.left = meta.margins[2],
		  meta.buffers.left.in = meta.buffers[2],
		  xylab.margins.left = xylab.margins[2],
		  
		  panel.buffers.left = panel.buffers[2],
		  panel.xtab.left = panel.xtab.size[2],
		  grid.buffers.left = grid.buffers[2],
		  grid.margins.left = grid.margins[2],
		  
		  {if (o$nrows > 1) rep(c(panel.wrap.size[2], 1, panel.wrap.size[2], between.margin), o$nrows -1) else NULL},
		  panel.wrap.size[2], 1, panel.wrap.size[2],
		  
		  grid.margins.left = grid.margins[4],
		  grid.buffers.left = grid.buffers[4],
		  panel.xtab.right = panel.xtab.size[4],
		  panel.buffers.right = panel.buffers[4],
		  
		  xylab.margins.right = xylab.margins[4],
		  meta.buffers.right.in = meta.buffers[4],
		  meta.margins.right = meta.margins[4],
		  meta.buffers.right.out = meta.buffers[4],
		  outer.margins.right = outer.margins[4])
	types = c("npc", "inch", "npc", "inch", "inch", 
			  "inch", "inch", "inch", "inch",
			  
			  {if (o$nrows > 1) rep(c("inch", "null", "inch", "inch"), o$nrows - 1) else NULL},
			  "inch", "null", "inch",
			  
			  "inch", "inch", "inch", "inch",
			  "inch", "inch", "npc", "inch", "npc")
	
	u = unit(x, types)
	
	u2 = convertWidth(u, "npc")
	
	
	isplot = which(types == "null")
	
	u2[isplot] = (unit(1,"npc") - sum(u2)) / length(isplot)
	
	names(u2) = names(x)
	u2
})



names(rows)


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

rows
cols
names(rows)

add_to_gt(rectGrob(gp=gpar(fill = "blue")), row = 1:29, col = 1:29)
add_to_gt(rectGrob(gp=gpar(fill = "yellow")), row = 2:28, col = 2:28)
add_to_gt(rectGrob(gp=gpar(fill = "red")), row = 3:27, col = 3:27)
add_to_gt(rectGrob(gp=gpar(fill = "yellow")), row = 4:26, col = 4:26)


add_to_gt(rectGrob(gp=gpar(fill = "blue")), row = 1, col = 1:29)
add_to_gt(rectGrob(gp=gpar(fill = "blue")), row = 29, col = 1:29)

add_to_gt(rectGrob(gp=gpar(fill = "blue")), row = 1:29, col = 1)
add_to_gt(rectGrob(gp=gpar(fill = "blue")), row = 1:29, col = 29)

add_to_gt(rectGrob(gp=gpar(fill = "red")), row = 3, col = 3:27)
add_to_gt(rectGrob(gp=gpar(fill = "red")), row = 27, col = 3:27)

add_to_gt(rectGrob(gp=gpar(fill = "red")), row = 3:27, col = 3)
add_to_gt(rectGrob(gp=gpar(fill = "red")), row = 3:27, col = 27)

grid.newpage()
grid.draw(gt)

