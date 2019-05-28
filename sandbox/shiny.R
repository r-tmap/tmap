library(shiny)
library(leaflet)
library(sf)

devtools::load_all()
#library(tmap)
# import data
data(World)

library(leafsync)



# Define UI for application that draws static and slippy maps
ui <- fluidPage(
	
	# Application title
	titlePanel("Example maps"),
	# Sidebar with a slider input for year of interest
	sidebarLayout(
		sidebarPanel(
			selectInput("color", "Color", c("blue", "red", "purple"))
		),
		# Show two maps
		mainPanel(
			h2("tmap"),
			tmapOutput("tmap"),
			h2("leaflet"),
			leafletOutput("lmap")
		)
	)
)

# Define server logic
server <- function(input, output) {
	# single panel map
	
	
	
	output$tmap <- renderTmap({
		tm_shape(World) +
			tm_polygons("gdp_cap_est")
	})
	
	output$lmap <- renderLeaflet({
		leaflet() %>% 
			addPolygons(data = World %>% st_transform(4326), layerId = as.character(World$iso_a3))
	})
	
	tm_clear_polygons
	

	observe({
		col <- input$color
		print("----")
		# tm_proxy("tmap") %>% 
		# 	removeShape(as.character(World$iso_a3[101:177])) %>% 
		# 	addPolygons(data = World[101:177,] %>% st_transform(4326), layerId = as.character(World$iso_a3)[101:177], fillColor = col)
		# tm_proxy("tmap") +
		# 	tm_clear_polygons() +
		# 	tm_shape(World) +
		# 	tm_polygons(col)
		
		
		tm_proxy("tmap")$tm_proxy %>%
			removeShape(as.character(World$iso_a3[101:177])) %>%
			addPolygons(data = World[101:177,] %>% st_transform(4326), layerId = as.character(World$iso_a3)[101:177], fillColor = col)


	})
	
	# tm_proxy("tmap") +
	# tm_remove_layer(3) + 
	
	
	# leafletProxy("tmap") %>% 
	# 	clearShapes() %>% 
	# 	addPolygons(data = World %>% st_transform(4326), layerId = as.character(World$iso_a3), fillColor = col)
	
		
	observe({
		col <- input$color

		leafletProxy("lmap") %>% 
			removeShape(as.character(World$iso_a3[101:177])) %>% 
			addPolygons(data = World[101:177,] %>% st_transform(4326), layerId = as.character(World$iso_a3)[101:177], fillColor = col)
	})
	
	
}

# Run the application 
shinyApp(ui = ui, server = server)
