library(grid)

grid.newpage()
vp <- viewport(width=0.5, height=0.5, gp=gpar(alpha=1), default.units = "native")
pushViewport(vp)


str(current.viewport())

grid.rect(gp=gpar(fill="lightgreen"))


grid.polyline(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(col="#0000EE77", lwd=4))


grid.polygon(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(fill="blue"))


# tm_borders <- function(col="grey40", lwd=1, lty="solid", alpha=1) {
# 	col <- do.call("rgb", c(as.list(col2rgb(col)), list(alpha=alpha*255, maxColorValue=255)))
# 	alpha <- NULL
# 	g <- list(tm_borders=as.list(environment()))
# 	class(g) <- "tmap"
# 	g
# }


library(ggplot2)
x <- data.frame(x=c(1,2), y=c(20,30))
gg <- qplot(x=x,y=y, data=x, geom="line", colour=I("#0000EE77"), size=4)
grob <- ggplotGrob(gg)

# simplify
grob2 <- grob
grob2$grobs <- grob2$grobs[4]
grob2$layout <- grob2$layout[4, ]

gr <- polylineGrob(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(col="#0000EE77", lwd=4))

grob2$grobs <- gList(gr)

## good:
grid.newpage(); grid.draw(grob2)



## create simple gtable object
library(gtable)

g <- gtable(widths=unit(1,"npc"), heights=unit(1,"npc"))
g <- gtable_add_grob(x = g, grobs = gr, 1, 1)


## bad:
grid.newpage(); grid.draw(g)



getTree <- function(x) {
	children_vps <- mapply(gtable:::child_vp, vp_name = gtable:::vpname(x$layout), 
						   t = x$layout$t, r = x$layout$r, b = x$layout$b, l = x$layout$l, 
						   clip = x$layout$clip, SIMPLIFY = FALSE)
	x$grobs <- mapply(gtable:::wrap_gtableChild, x$grobs, children_vps, 
					  SIMPLIFY = FALSE)
	gt <- gTree(children = do.call("gList", x$grobs[order(x$layout$z)]), 
				cl = c("gTableParent"), vp = x$vp, layoutvp = viewport(layout = gtable:::gtable_layout(x), 
																	   name = x$name))
}


gt <- getTree(grob2)
gt2 <- getTree(g)

# good
grid.newpage(); grid.draw(gt)

# bad
grid.newpage(); grid.draw(gt2)


# what's the difference?
names(gt)
names(gt2)

str(gt$layoutvp)
str(gt2$layoutvp)

lvp <- gt2$layoutvp

lvp$layout <- grid.layout(2,2, widths=unit(1/2, "npc"), heights=unit(1/2, "npc"))

gt3 <- gt2
gt3$layoutvp <- lvp

str(gt$layoutvp)

# good:
grid.newpage(); grid.draw(gt3)

gt4 <- gt3
gt4$children[[1]]$vp <- gt4$children[[1]]$wrapvp
gt4$children[[1]]$wrapvp <- NULL
class(gt4$children[[1]]) <- class(gt4$children[[1]])[-1]

## good:
grid.newpage(); grid.draw(gt4)

gt5 <- gt4
gt5$vp <- gt5$layoutvp
gt5$layoutvp <- NULL
class(gt5) <- class(gt4)[-1]

## good:
grid.newpage(); grid.draw(gt5)

## try to build from stratch
gr2 <- gr
gr2$vp <- viewport(layout.pos.row = 1, layout.pos.col = 1)

gt6 <- gTree(children = gList(gr2), vp=viewport(layout=grid.layout(1,1, widths=unit(.99999, "npc"), heights=unit(.99999, "npc"))))

## bad:
grid.newpage(); grid.draw(gt6)

str(gt5$children[[1]])

str(gt6$children[[1]])

gt7 <- gt6

gt7$children[[1]]$vp$clip <- TRUE
grid.newpage(); grid.draw(gt7)




## test

data(NLD_prov)
qtm(NLD_prov)

gr <- rectGrob(width = .5, height=.5, gp=gpar(col="#00FF0033"))
gr <- polylineGrob(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(col="#0000EE77", lwd=4), vp=viewport(width=.99999, height=.999999, clip=TRUE))
gr <- polylineGrob(x=c(0.2, 0.7, 0.8, 0.5, 0.3, 0.2), y=c(0.1, 0.2, 0.9, 0.8, 0.2, 0.1), gp=gpar(col="#0000EE77", lwd=4), vp=viewport(layout.pos.row = 1, layout.pos.col = 1, clip=TRUE))


## new test
vp <- viewport(layout=grid.layout(3, 3, 
								  heights=unit(c(.3, .4, .3), 
								  			 c("npc", "npc", "npc")), 
								  widths=unit(c(.3, .4, .3), 
								  			c("npc", "npc", "npc"))),
			   name="maingrid")

vp <- viewport(layout=grid.layout(1,1,widths=unit(.99999, "npc"), heights=unit(.99999, "npc")))

#t <- gTree(children=gList(gTree(children=gList(gr))), vp=vp)
t <- gTree(children=gList(gr), vp=vp)
grid.newpage()
grid.draw(t)


