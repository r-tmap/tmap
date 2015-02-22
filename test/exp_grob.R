library(grid)


grid.newpage()

grid.rect(width=.5, height=.5, gp=gpar(fill="steelblue"))
g1 <- rectGrob(width=.5, height=.5, gp=gpar(fill="steelblue"))

vp <- viewport(width=.5, height=.5)

pushViewport(vp)

grid.rect(width=.5, height=.5, x=.7, y=.7, gp=gpar(fill="gold"))
g2 <- rectGrob(width=.5, height=.5, x=.7, y=.7, gp=gpar(fill="gold"))
grid.lines(x=c(.1,.9), y=c(.3,.7), gp=gpar(lwd=3, col="purple"))
g3 <- linesGrob(x=c(.1,.9), y=c(.3,.7), gp=gpar(lwd=3, col="purple"))


gTree

gt1 <- gTree(children=gList(g1))

gt2 <- gTree(children=gList(g2, g3), vp=vp)

gt <- gTree(children=gList(gt1, gt2, NULL))

grid.newpage()
grid.draw(gt1)
grid.draw(gt2)


grid.newpage()
grid.draw(gt)


grid.newpage()
vp <- viewport(width=.5, height=.5)

rm(x)
pushViewport(viewport(layout = grid.layout(3,3)))

x <- cellplot(x=1,y=1, e={
	3
})

grid.draw(gt)

gold <- function() grid.rect(gp=gpar(fill="gold"))
blue <- function() grid.rect(gp=gpar(fill="steelblue"))
frm <- function() grid.rect(gp=gpar(col="purple", fill=NA, lwd=3))


## test

data(NLD_prov)

tm_shape(NLD_prov) +
	tm_fill("population") +
	tm_borders() +
	tm_bubbles(col = "name", size="population") +
	tm_grid() + tm_layout_NLD(title = "test")