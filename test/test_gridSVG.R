library(tmap)
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

# htmltools makes it easy for svg to appear in RStudio Viewer
library(htmltools)
# will also use XML saveXML
library(XML)

tm_shape(World) + 
	tm_polygons("pop_est")

# let's add country name as title for all the polygons
lapply(
	seq.int(1,length(World@data$name))
	,function(n){
		grid.garnish(
			paste0("tm_polygons_",n)
			, title=as.character(World@data$name)[n]
			, onmouseover="this.setAttribute('opacity', '0.5');"
			, onmouseout="this.setAttribute('opacity', '1');"
			, group = TRUE
		)
		grid.get(paste0("tm_polygons_",n))
	}
)

browsable(
	HTML(
		saveXML(
			# name = "" or NULL gives us the output; ...$svg is the svg
	  		grid.export(name = NULL)$svg
	  		#,prefix = ""
		)
	)
)


# add pan zoom with svgPanZoom htmlwidget
library(svgPanZoom)

svgPanZoom( grid.export(name = NULL)$svg, controlIconsEnabled = TRUE )

# restrict zoom to just the mapElements
#  for now pan up/down is reversed, but can be fixed
svgPanZoom(
	grid.export(name = NULL)$svg
	, viewportSelector = "#mapElements\\.1"
	, controlIconsEnabled = TRUE
)


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
