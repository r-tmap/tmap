


library(shiny)
library(leaflet)



data(NLD_prov)
data(World)
shp <- World#NLD_prov
library(sp)
shp <- spTransform(shp, CRS("+proj=longlat +datum=WGS84"))

# http://leafletjs.com/examples/choropleth.html

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
					
					opts=list(color='white', fillColor='red', fillOpacity=input$opacity)
					
# 					co <- coordinates(shp)
# 					map$addCircle(
# 						co[,1],
# 						co[,2],
# 						20 / max(5, input$map_zoom)^2,
# 						shp$name,
# 						list(
# 							weight=1.2,
# 							fill=TRUE,
# 							color='#4A9'
# 						)
# 					)
					
# 					map$addPolygon(
# 						c(50.835317, 51.835317, 51.835317, 50.835317, 50.835317),
# 						c(5.673065,  5.673065, 6.673065, 6.673065, 5.673065),
# 						layerId=c("1"),
# 						options=opts,
# 						defaultOptions=opts)
# 
					i <- 0
					for (p in shp@polygons) {
						for (pp in p@Polygons) {
							co <- pp@coords
							co <- rbind(co, co[1,])
							i <- i + 1
							map$addPolygon(
								c(co[,2]),
								c(co[,1]),
								layerId=list(as.character(i)),
								options=opts,
								defaultOptions=opts)
						}
					}


					
				}
			})
		},
		ui=fluidPage(
			leafletMap(
				"map", "100%", 600,
				initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
				initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
				options=list(
					center = c(51.01, 8.68),
					zoom = 4,
					maxBounds = list(list(17, -180), list(59, 180))
				)
			),
			fluidRow(
				column(4, offset=2, 
					   h2("Choropleth"),
					   actionButton("drawPoints", "Draw"),
					   sliderInput("opacity", "Opacity", min=0, max=1, value=.7, step=.05)))
		))
)




















