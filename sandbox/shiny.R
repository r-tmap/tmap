library(shiny)
library(leaflet)
library(sf)

devtools::load_all()
#library(tmap)
# import data
data(World)
data(metro)


library(leafsync)

world_vars <- setdiff(names(World), c("iso_a3", "name", "sovereignt", "geometry"))

years <- paste0("pop", seq(1950, 2030, by = 10))

# Define UI for application that draws static and slippy maps
ui <- fluidPage(
	# Application title
	titlePanel("Example maps"),
	# Sidebar with a slider input for year of interest
	sidebarLayout(
		sidebarPanel(
			selectInput("variable", "Variable", world_vars),
			sliderInput("year", "Year", min = 1950, max = 2030, step = 10, value = 1950),
			sliderInput("range", "Population range", min = 0, max = 40e6, step = 1e6, value = c(0, 40e6)),
			selectInput("letter", "Remove countries starting with the letter", letters),
			shiny::actionButton("button", "Remove them")
		),
		# Show two maps
		mainPanel(
			h2("tmap"),
			tmapOutput("tmap", height = "1000px")
		)
	)
)

# Define server logic
server <- function(input, output, session) {
	# single panel map
	
	
	
	output$tmap <- renderTmap({
		tm_shape(World) +
			tm_polygons(world_vars[1]) + #, zindex = 402
		tm_shape(metro) +
			tm_symbols(col = "gold", size = years[1]) #, zindex = 403
	})

	observe({
		var <- input$variable
		
		# leafletProxy("tmap", session) %>% 
		# 	removeShape(levels(World$iso_a3))

		# tm_proxy("tmap", session) + tm_remove_layer(402)
				
		tmapProxy("tmap", session, {
			tm_remove_layer(402) +
			tm_shape(World) +
				tm_polygons(var, zindex = 402)
		})
	})
	
	

	# observe({
	# 	col <- input$color
	# 	print("----")
	# 	# tm_proxy("tmap") %>% 
	# 	# 	removeShape(as.character(World$iso_a3[101:177])) %>% 
	# 	# 	addPolygons(data = World[101:177,] %>% st_transform(4326), layerId = as.character(World$iso_a3)[101:177], fillColor = col)
	# 
	# 	tm_proxy("tmap") +
	# 		tm_remove_features() +
	# 		tm_remove_layer()
	# 		tm_shape(World) +
	# 		tm_polygons(col)
	# 	
	# 	
	# 	tm_proxy("tmap")$tm_proxy %>%
	# 		removeShape(as.character(World$iso_a3[101:177])) %>%
	# 		addPolygons(data = World[101:177,] %>% st_transform(4326), layerId = as.character(World$iso_a3)[101:177], fillColor = col)
	# 
	# 
	# })
	# 
	# # tm_proxy("tmap") +
	# # tm_remove_layer(3) + 
	# 
	# 
	# # leafletProxy("tmap") %>% 
	# # 	clearShapes() %>% 
	# # 	addPolygons(data = World %>% st_transform(4326), layerId = as.character(World$iso_a3), fillColor = col)
	# 
	# 	
	# observe({
	# 	col <- input$color
	# 
	# 	leafletProxy("lmap") %>% 
	# 		removeShape(as.character(World$iso_a3[101:177])) %>% 
	# 		addPolygons(data = World[101:177,] %>% st_transform(4326), layerId = as.character(World$iso_a3)[101:177], fillColor = col)
	# })
	
	
}

# Run the application 
shinyApp(ui = ui, server = server)
