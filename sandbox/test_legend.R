legs = list(list(title = "economy", labels = levels(World$economy), palette = hcl.colors(7)),
			list(title = "test1", labels = letters[1:7], palette = hcl.colors(7)),
			list(title = "", labels = letters[1:5], palette = hcl.colors(5)))

o = tmap_options()
o$legend.title.size = 3
o$legend.text.size = 2

gridCell = function(rows, cols, e) {
	vp = grid::viewport(layout.pos.row = rows, layout.pos.col = cols)
	#grid::gTree(children=grid::gList(e), vp=vp)
	#browser()
	grid::grobTree(e, vp = vp)
}


legend.stack = "vertical"
legend.stack = "horizontal"


leg_standard = list(
	fun_height = function(leg) {
		inch = grid::convertHeight(grid::unit(1, "lines"), "inches", valueOnly = TRUE)
		
		tH = ifelse(leg$title == "", 0, inch * o$legend.title.size * 1.375)
		iH = inch * (length(leg$labels) + 0.8) * o$legend.text.size
		tH + iH
	},
	fun_width = function(leg) {
		inch = grid::convertHeight(grid::unit(1, "lines"), "inches", valueOnly = TRUE)
		
		tW = ifelse(leg$title == "", 0, inch * o$legend.title.size * grid::convertWidth(grid::stringWidth(leg$title), unitTo = "lines", valueOnly = TRUE))
		
		
		iW = inch * o$legend.text.size * grid::unit(grid::convertWidth(grid::stringWidth(leg$labels), unitTo = "lines", valueOnly = TRUE) + 1.65, "lines")
		max(c(tW, iW)) + (inch * o$legend.text.size * 0.75)
	},
	fun_plot = function(leg, W, H) {
		nlev = length(leg$labels)
		lH = grid::convertHeight(grid::unit(1, "lines"), "inches", valueOnly = TRUE)
		
		if (leg$title == "") o$legend.title.size = 0
		
		vp = grid::viewport(layout = grid::grid.layout(ncol = 4, nrow = nlev + 4, 
													   widths = grid::unit(c(lH * o$legend.text.size * 0.4, lH * o$legend.text.size, lH * o$legend.text.size * 0.25, 1), units = c("inches", "inches", "inches", "null")),
													   heights = grid::unit(
													   	c(lH * o$legend.title.size * c(0.25, 1),
													   	  lH * o$legend.title.size * .125 + lH * o$legend.text.size * .4,
													   	  rep(lH * o$legend.text.size, nlev), 1), units = c(rep("inches", nlev + 3), "null"))))
		
		grTitle = gridCell(1:3, 2, grid::textGrob(leg$title, x = 0, just = "left", gp = grid::gpar(cex = o$legend.title.size)))
		grText = lapply(1:nlev, function(i) gridCell(i+3, 4, grid::textGrob(leg$labels[i], x = 0, just = "left", gp = grid::gpar(cex = o$legend.text.size))))
		grItems = lapply(1:nlev, function(i) gridCell(i+3, 2, grid::rectGrob(gp = grid::gpar(fill = leg$palette[i]))))
		
		g = do.call(grid::grobTree, c(list(grTitle), grText, grItems, list(vp = vp)))
		
		#g = grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="red")))
		
		g
		
		# nlev = length(leg$labels)
		# nlns = nlev + 2
		# 
		# ys = c(nlns - 0.75, seq(nlns - 2.25, 0.75, by = -1))
		# xs = c(0.25, rep(1.5, length(leg$labels)))
		# g = list(grid::rectGrob(gp=grid::gpar(fill="grey90")),
		# 		 grid::rectGrob(x = grid::unit(.75, "lines"), y = grid::unit(ys[-1], "lines"), width = grid::unit(1, "lines"), height = grid::unit(1, "lines"), gp=grid::gpar(fill = leg$palette)),
		# 		 grid::textGrob(c(leg$title, leg$labels), x = grid::unit(xs, "lines"), y = grid::unit(ys, "lines"), just = "left", gp=grid::gpar(cex = 0.7)))
		# totalH = grid::unit(nlns, "lines")
		# totalW = grid::unit(max(grid::convertWidth(grid::stringWidth(leg$labels), unitTo = "lines", valueOnly = TRUE)) + 1.75, "lines")
		# list(g=g, totalH=totalH, totalW=totalW)
	}
)

legWin = vapply(legs, leg_standard$fun_width, FUN.VALUE = numeric(1))
legHin = vapply(legs, leg_standard$fun_height, FUN.VALUE = numeric(1))
if (o$legend.justified) {
	if (legend.stack == "vertical") {
		legWin = rep(max(legWin), length(legs))		
	} else {
		#legHin = rep(max(legHin), length(legs))
	}
} 

legW = grid::unit(legWin, "inches")
legH = grid::unit(legHin, "inches")

legGrobs = lapply(legs, leg_standard$fun_plot)




if (length(legs) == 1) {
	legY = list(grid::unit(1, "npc"))
	legX = list(grid::unit(0, "npc"))
} else {
	legY = c(list(grid::unit(1, "npc")), lapply(1:(length(legs)-1), function(i) {
		u = grid::unit(1, "npc")
		for (j in 1:i) {
			u = u - legH[[j]]
		}
		u
	}))
	legX = c(list(grid::unit(0, "npc")), lapply(1:(length(legs)-1), function(i) {
		u = legW[[1]]
		if (i>1) for (j in 2:i) {
			u = u + legW[[j]]
		}
		u
	}))
}




# 
# 
# legY = 
# 
# print(legH)

#legW = 


# grbs = do.call(grid::gList, mapply(function(lG, lH) {
# 	grbs = do.call(grid::grobTree, c(lG$g, list(vp = grid::viewport(x = lG$totalW/2, width = lG$totalW, y = lH - lG$totalH/2, height = lG$totalH))))
# }, legGrobs, legY, SIMPLIFY = FALSE))

grbs = do.call(grid::gList, mapply(function(lG, lX, lY, lH, lW) {
	frame = grid::rectGrob(gp=grid::gpar(fill = o$legend.bg.color, col = "black"))
	if (legend.stack == "vertical") {
		grid::grobTree(frame, lG, vp = grid::viewport(x = lW/2, width = lW, y = lY - lH/2, height = lH))
	} else {
		grid::grobTree(frame, lG, vp = grid::viewport(x = lX + lW/2, width = lW, y = legY[[1]] - lH/2, height = lH))
	}
	
	
}, legGrobs, legX, legY, legH, legW, SIMPLIFY = FALSE))


grid::grid.newpage()

grid::grid.draw(grbs)


#g = grid::grobTree(grid::rectGrob(gp=grid::gpar(fill="red")))
#grid::grid.draw(g)


