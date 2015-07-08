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
### example 1
############################

data(World)

tm_shape(World) + tm_polygons("pop_est")



grid.ls(fullNames = TRUE)

# let's add country name as title for all the polygons
hover_text <- paste(World$name, "\nPopulation=", World$pop_est)

lapply(
	seq.int(1,length(World@data$name))
	,function(n){
		grid.garnish(
			paste0("tm_polygons_1_",n)
			, title=hover_text[n]
			, onmouseover="this.setAttribute('opacity', '0.5');"
			, onmouseout="this.setAttribute('opacity', '1');"
			, group = TRUE
			, global = FALSE
		)
		# return this to see effect
		grid.get(
			grid.grep(paste0("tm_polygons_1_",n),global=TRUE)[[1]]
		)[c("name","groupAttributes")]
	}
)

# result from above should return a list with the name of polygon and groupAttributes
#   if NULL then something did not work


# title in RStudio does not appear because of this
#   http://stackoverflow.com/questions/12222345/tooltips-not-showing-when-hovering-over-svg-polygons
# 	regardless, title is a crude way for tooltip
#   work on a better tooltip mechanism
#   Paul Murrell's tootlip.js might be the easiest start without introducing dependencies

# for now we can amend our svg to get nested titles so will work in RStudio
tmap_svg <- grid.export(name = NULL)$svg

xpathApply(
	tmap_svg
	,"//*[local-name() = 'g' and starts-with(@id, 'tm_polygon')]"
	,function(g_el){
		addChildren(
			g_el
			, newXMLNode("title",htmlEscape(xmlAttrs(g_el)[["title"]]))
		)
	}
)

#grid.export("../test/test.svg")
cat( saveXML(tmap_svg), file = "../test/test.svg")

# ###### hopefully, this is now fixed where a title appears on hover and opacity changes
# shows static SVG file (is this normal?)
#
browsable(
	HTML(
		saveXML(
			# name = "" or NULL gives us the output; ...$svg is the svg
			# grid.export(name = NULL)$svg
			#,prefix = ""
			tmap_svg
		)
	)
)


############################
### example 2
############################


data(metro)
metro$growth <- (metro$pop2020 - metro$pop2010) / (metro$pop2010 * 10) * 100

tm_shape(World) +
	tm_polygons("income_grp", palette="-Blues", contrast=.7) +
	tm_shape(metro) +
	tm_bubbles("pop2010", col = "growth", 
			   border.col = "black", border.alpha = .5, 
			   style="fixed", breaks=c(-Inf, seq(0, 6, by=2), Inf),
			   palette="-RdYlBu", contrast=1, 
			   title.size="Metro population", 
			   title.col="Growth rate (%)") + 
	tm_layout_World()
grid.ls(fullNames = TRUE)

hover_text <- paste(World$name, "\nPopulation=", World$pop_est)

lapply(
	seq.int(1,length(World@data$name))
	,function(n){
		grid.garnish(
			paste0("tm_polygons_1_",n)
			, title=hover_text[n]
			, onmouseover="this.setAttribute('opacity', '0.5');"
			, onmouseout="this.setAttribute('opacity', '1');"
			, group = TRUE
		)
		grid.get(paste0("tm_polygons_",n))
	}
)

grid.garnish("tm_bubbles_2_1", title=c("test123", "test321"), group=FALSE)

grid.export("../test/test2.svg")






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




