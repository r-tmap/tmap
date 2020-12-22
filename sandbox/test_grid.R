library(grid)


vpA = viewport(w=0.8, h=0.8)
vpB = viewport(w=0.8, h=0.8, x = .3, y = .3)


vpS = vpStack(vpA, vpB)

g = rectGrob(gp=gpar(fill="red"), vp = vpS)

grobTree


library(gtable)



grid.newpage()
grid.draw(g)



tree <- vpTree(viewport(w=0.8, h=0.8, name="A"),
			   vpList(vpStack(viewport(x=0.1, y=0.1, w=0.5, h=0.5,
			   						just=c("left", "bottom"), name="B"),
			   			   viewport(x=0.1, y=0.1, w=0.5, h=0.5,
			   			   		 just=c("left", "bottom"), name="C"),
			   			   viewport(x=0.1, y=0.1, w=0.5, h=0.5,
			   			   		 just=c("left", "bottom"), name="D")),
			   	   viewport(x=0.5, w=0.4, h=0.9,
			   	   		 just="left", name="E")))
pushViewport(tree)




gLsts = lapply(LETTERS[1:5], function(i) {
	seekViewport(i)
	gList(grid::rectGrob(),
		  grid::textGrob(current.vpTree(FALSE),
			  x=unit(1, "mm"), y=unit(1, "npc") - unit(1, "mm"),
			  just=c("left", "top"),
			  gp=gpar(fontsize=8)))
	
})


g = do.call(grid::gList, gLsts)


grid.draw(g)
