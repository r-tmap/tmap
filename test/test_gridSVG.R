############################
### install/load packages
############################


devtools::load_all("./pkg")
#library(tmap)
library(gridSVG)
library(grid)
library(htmltools)
library(XML) # will also use XML saveXML

#devtools::install_github("timelyportfolio/svgPanZoom")
library(svgPanZoom)

############################
### usefull links
# http://sachsmc.github.io/UseR2015-Talk/#1
# http://www.buildingwidgets.com/blog/2015/1/15/week-02-
############################

############################

data(World)

tm_shape(World) + tm_polygons("pop_est")

grid.ls(fullNames = TRUE)

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

grid.export("../test/test.svg")

# shows static SVG file (is this normal?)
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
svgPanZoom( grid.export(name = NULL)$svg, controlIconsEnabled = TRUE )

# restrict zoom to just the mapElements
#  for now pan up/down is reversed, but can be fixed
svgPanZoom(
	grid.export(name = NULL)$svg
	, viewportSelector = "#mapElements\\.1"
	, controlIconsEnabled = TRUE
)




