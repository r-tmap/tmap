library(leaflet)


shinyUI(fluidPage(
	tags$head(tags$link(rel='stylesheet', type='text/css', href='styles.css')),
	fixedRow(
		column(8, 
			   leafletMap(
			   	"map", "100%", 650,
			   	initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
			   	initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
			   	options=list(
			   		center = c(mean(bbox[2,]), mean(bbox[1,])),
			   		zoom = zoomlevel(bbox),
			   		maxBounds = list(list(bbox[2,1], 
			   							  bbox[1,1]), 
			   						 list(bbox[2,2],
			   						 	 bbox[1,2])))
			   )),
		column(4, 
			   h4('Visible regions'),
			   dataTableOutput('data')
		)
	),
	fluidRow(
		column(4, offset=1,
			   plotOutput("legendPlot", "100%", 650)
		),
		column(4, 
			   h4("Settings"),
			   sliderInput("opacity", "Opacity", 
			   			min=0, max=1, value=.5, step=.05)
		)
	)
))