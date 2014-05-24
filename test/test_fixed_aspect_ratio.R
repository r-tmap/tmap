library(grid)
library(gridBase)
library(sp)
opar <- par(no.readonly=TRUE)


npc.w <- .7
npc.h <- .5

data(Europe)


ws <- unit.c(unit(1, "null"), unit(npc.w, "snpc"), unit(1, "null"))
hs <- unit.c(unit(1, "null"), unit(npc.h, "snpc"), unit(1, "null"))


grid.newpage()
pushViewport(viewport(layout=grid.layout(nrow=3, ncol=3, 
										 widths=ws,
										 heights=hs)))
pushViewport(viewport(layout.pos.col=2, layout.pos.row=2))	
grid.rect(gp=gpar(fill="green"))
#par(fig=gridFIG())
par(mai=c(0,0,0,0), oma=c(0,0,0,0))
par(plt=gridPLT())
par(new=TRUE)
plot(Europe)
grid.rect(gp=gpar(col="blue", fill=NA))

par(opar)


library(grid)
opar <- par(no.readonly=TRUE)
# gridFIG
grid.newpage()
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lty="dashed"))
par(fig=gridFIG())
par(new=TRUE)
plot(1:10)
# multiple plots
# NOTE the use of par(mfg)
# gridOMI
par(opar)
grid.newpage()
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lty="dashed"))
par(omi=gridOMI())
par(mfrow=c(2, 2), mfg=c(1, 1), mar=c(3, 3, 1, 0))
for (i in 1:4) {
	plot(i)
}
# gridPLT
par(opar)
grid.newpage()
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lwd=5))
par(plt=gridPLT())
par(new=TRUE)
plot(1:10)
# gridFIG with par(omi) set
par(opar)
grid.newpage()
par(omi=rep(1, 4))
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lwd=5))
par(fig=gridFIG())
par(new=TRUE)
plot(1:10)
# gridPLT with par(omi) set
par(opar)
grid.newpage()
par(omi=rep(1, 4))
pushViewport(viewport(width=0.5, height=0.5))
grid.rect(gp=gpar(col="grey", lwd=5))
par(plt=gridPLT())
par(new=TRUE)
plot(1:10)
# gridPAR
par(opar)
grid.newpage()
pushViewport(viewport(width=0.5, height=0.5,
					  gp=gpar(col="red", lwd=3, lty="dotted")))
grid.rect(gp=gpar(col="grey", lwd=5))
par(fig=gridFIG())
par(gridPAR())
par(new=TRUE)
plot(1:10, type="b")


## working example (testing)

library('grid')
xlim <- c(0, 1000)
ylim <- c(0, 500)
w <- min(1,diff(xlim)/diff(ylim))
h <- min(1,diff(ylim)/diff(xlim))

if (w>h) {
	w <- unit(1, "snpc")
	h <- unit(h, "snpc")
} else  {
	w <- unit(w, "snpc")
	h <- unit(1, "snpc")
}


grid.newpage() # like plot.new()
pushViewport(viewport( # like plot.window()
	x=0.5, y=0.5, # a centered viewport
	width=w, 
	height=h,
	xscale=xlim, # cf. xlim
	yscale=ylim  # cf. ylim
))
# some drawings:
grid.rect(xlim[1], ylim[1], xlim[2], ylim[2], just=c(0, 0), default.units="native")
grid.lines(xlim, ylim, default.units="native")
grid.lines(xlim, rev(ylim), default.units="native")





## working example

library('grid')
xlim <- c(0, 1000)
ylim <- c(0, 500)
grid.newpage() # like plot.new()
pushViewport(viewport( # like plot.window()
	x=0.5, y=0.5, # a centered viewport
	width=unit(min(1,diff(xlim)/diff(ylim)), "snpc"), # aspect ratio preserved
	height=unit(min(1,diff(ylim)/diff(xlim)), "snpc"),
	xscale=xlim, # cf. xlim
	yscale=ylim  # cf. ylim
))
# some drawings:
grid.rect(xlim[1], ylim[1], xlim[2], ylim[2], just=c(0, 0), default.units="native")
grid.lines(xlim, ylim, default.units="native")
grid.lines(xlim, rev(ylim), default.units="native")




## experiment with grid polygon
require(grid)
shp <- Europe

plot(shp)


data(World)

system.time({
	plot(World)
})
system.time({
	grid.shape(World)
})




