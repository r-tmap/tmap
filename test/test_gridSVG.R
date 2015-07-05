library(gridSVG)
library(grid)

data(NLD_prov)

tm_shape(NLD_prov) +
	tm_fill("population", convert2density = TRUE)


data(World)

system.time({
	print(tm_shape(World) + 
		tm_polygons("pop_est"))
})


grid.ls(fullNames = TRUE)

# http://sachsmc.github.io/UseR2015-Talk/#1

grid.garnish("tm_polygons_28", title="test1344", 
			 onmouseover="setAttribute('opacity', '0.5');",
			 onmouseout="setAttribute('opacity','1)');", group=TRUE)


grid.export("../test/test.svg")


## R Journal paper

library(grid)
text <- sample(c("goat", "goat", "car"))
cols <- hcl(c(0, 120, 240), 80, 80)
MontyHall <- function() {
	grid.newpage()
	grid.text(text, 1:3/4, gp = gpar(cex = 2), name = "prizes")
	for (i in 1:3) {
		grid.rect(i/4 - .1, width=.2, height=.8, just = "left",
				  gp = gpar(fill = cols[i]), name = paste0("door", i))
	}
}
MontyHall()

grid.ls(fullNames = TRUE)

library(gridSVG)
links <- c("http://www.google.com/search?q=car&tbm=isch",
			 "http://www.google.com/search?q=goat&tbm=isch")
for (i in 1:3) {
	grid.hyperlink(paste0("door", i),
				   href = links[match(text[i], c("car", "goat"))],
				   show = "new")
}
grid.export("../test/montyhall-hyper.svg")



MontyHall()
goatDoor <- grep("goat", text)[1]
grid.animate(paste0("door", goatDoor), width = c(.2, 0), duration = 2)
grid.export("../test/montyhall-anim.svg")



circleMask <- gTree(children = gList(rectGrob(gp = gpar(col = NA, fill = "white")),
									 circleGrob(x = goatDoor/4, r=.15,
									 		   gp = gpar(col = NA, fill = "grey")),
									 polylineGrob(c(0, 1, .5, .5),
									 			 c(.5, .5, 0, 1),
									 			 id = rep(1:2, each = 2),
									 			 gp = gpar(lwd = 10, col = "white"))))

MontyHall()
grid.mask(paste0("door", goatDoor), gridSVG::mask(circleMask))
grid.export("../test/montyhall-masked.svg")


MontyHall()
for (i in 1:3) {
	grid.garnish(paste0("door", i), title = text[i])
}
grid.export("../test/montyhall-tooltip.svg")
