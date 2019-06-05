library(shiny)
library(leaflet)
library(sf)

devtools::load_all()
#library(tmap)
# import data
data(World)
data(metro)
data(rivers)
data(land)


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
			selectInput("rastervar", "Raster variable", names(land)),
			selectInput("polyvar", "Polygons variable", world_vars),
			sliderInput("alpha", "Polygon alpha", min = 0, max = 1, value = .5),
			sliderInput("width", "River width", min = 1, max = 10, value = 3),
			sliderInput("year", "Population year", min = 1950, max = 2030, step = 10, value = 1950)
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
		tm_shape(land) +
			tm_raster(names(land)[1], zindex = 401) +
		tm_shape(World) +
			tm_polygons(world_vars[1], zindex = 402) + #, zindex = 402
		tm_shape(rivers) +
			tm_lines(lwd = "strokelwd", scale = 3, zindex = 403) +
		tm_shape(metro) +
			tm_symbols(col = "gold", size = years[1], zindex = 404) #, zindex = 403
	})


	
	observe({
		var <- input$rastervar
		tmapProxy("tmap", session, {
			tm_remove_layer(401) +
			tm_shape(land) +
				tm_raster(var, zindex = 401)
		})
	})
	
	observe({
		var <- input$polyvar
		alpha <- input$alpha
		tmapProxy("tmap", session, {
			tm_remove_layer(402) +
				tm_shape(World) +
				tm_polygons(var, alpha = alpha, zindex = 402)
		})
	})
	
	observe({
		width <- input$width
		tmapProxy("tmap", session, {
			tm_remove_layer(403) +
			tm_shape(rivers) +
				tm_lines(lwd = "strokelwd", scale = width, zindex = 403)
		})
	})
	
	observe({
		year <- paste0("pop", input$year)
		
		tmapProxy("tmap", session, {
			tm_remove_layer(404) +
				tm_shape(metro) +
				tm_symbols(col = "gold", size = year, zindex = 404)
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
