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
hover_text <- paste0(World$name, " Population=", World$pop_est)

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
			, newXMLNode("title",xmlAttrs(g_el)[["title"]])
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

# let's add country name as title for all the polygons
hover_text <- paste0(World$name, " Population=", World$pop_est)

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


# test bubble interaction but title won't work in RStudio as discussed above
#  bubbles won't work yet as shown, because grid.ls() shows only one circle tm_bubbles
#  bubbles will need to work like polygons with a separate grid element for each
#  alternately, we could manage with SVG/XML; will include example below
grid.garnish("tm_bubbles_2_1", title=c("test123", "test321"), group=FALSE)



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
			, newXMLNode("title",xmlAttrs(g_el)[["title"]])
		)
	}
)


# now do the title for each bubble with SVG/XML
#  some discussion above 
#  note title as attribute won't appear in RStudio
mapply(
	function(el,title){
		xmlAttrs(el) <-  c("title" = title)
		el
	}
	, getNodeSet( tmap_svg, "//*[local-name() = 'circle' and starts-with(@id, 'tm_bubble')]" )
	, metro$name[order(metro$pop2010,decreasing=T)]
)

# more invasive way to get working in RStudio
mapply(
	function(el,title){
		xmlAttrs(el) <-  c("title" = title)
		copy_attrs <- xmlAttrs(el)
		copy_circle <- newXMLNode("circle")
		xmlAttrs(copy_circle) <- copy_attrs
		title_node <- newXMLNode("title",copy_attrs[["title"]])
		newg <- replaceNodes(
			el
			,newXMLNode("g",.children = list(copy_circle,title_node))
		)
		newg
	}
	, getNodeSet( tmap_svg, "//*[local-name() = 'circle' and starts-with(@id, 'tm_bubble')]" )
	, metro$name[order(metro$pop2010,decreasing=T)]
)

#grid.export("../test/test2.svg")
cat( saveXML(tmap_svg), file = "../test/test2.svg")






# shows static SVG file (is this normal?)
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


# add pan zoom with svgPanZoom htmlwidget
svgPanZoom(
	tmap_svg  #grid.export(name = NULL)$svg #works but no interactivity from above
	, controlIconsEnabled = TRUE
)

# restrict zoom to just the mapElements
#  for now pan up/down is reversed, but can be fixed
svgPanZoom(
	tmap_svg #grid.export(name = NULL)$svg #works but no interactivity from above
	, viewportSelector = "#mapElements\\.1"
	, controlIconsEnabled = TRUE
)
