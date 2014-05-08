## first example: US cities with mapbox

devtools::install_github("leaflet-shiny", username="jcheng5")
devtools::install_github("ShinyDash", "trestletech")

runApp("../test/leaflet/")


## second example simple polygon in OSM

library(shiny)
library(leaflet)

runApp(
	list(
		server=function(input, output, session) {
			
			# create the map
			map <- createLeafletMap(session, 'map')
			
			observe({ 
				
				if(input$drawPoints == 0) {
					return(NULL)
				} else {
					
					map$clearShapes()
					
					opts=list(color='#4A9')
					
					map$addPolygon(
						c(48.99831, 49.08815, 49.08815, 48.99831, 48.99831),
						c(13.42666, 13.42666, 13.56383, 13.56358, 13.42666),
						layerId=c("1"),
						options=opts,
						defaultOptions=opts)
					
				}
			})
		},
		ui=basicPage(
			mainPanel(
				leafletMap("map", "100%", 550, initialTileLayer = "http://{s}.tile.openstreetmap.org/{z}/{x}/{y}.png",
						   initialTileLayerAttribution = HTML('&copy; <a href="http://openstreetmap.org">OpenStreetMap</a> contributors, <a href="http://creativecommons.org/licenses/by-sa/2.0/">CC-BY-SA</a>'),
						   options = list(center = c(51.01, 8.68), zoom = 6, maxZoom = 10, minZoom=4, maxBounds = list(list(30,-20),list(68,90)))),
				actionButton("drawPoints", "Draw"))
		))
)