
npc.w <- .5
npc.h <- 1



ws <- unit.c(unit(1, "null"), unit(npc.w, "snpc"), unit(1, "null"))
hs <- unit.c(unit(1, "null"), unit(npc.h, "snpc"), unit(1, "null"))


grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=3, ncol=3, 
										 widths=ws,
										 heights=hs)))
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))	
grid.rect(gp=gpar(fill="blue"))
