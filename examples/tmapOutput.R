if (require("shiny")) {
	
	
	data(World)
	world_vars <- setdiff(names(World), c("iso_a3", "name", "sovereignt", "geometry"))
	
	
	ui <- fluidPage(
		tmapOutput("map"),
		selectInput("var", "Variable", world_vars)
	)
	
	server <- function(input, output, session) {
		output$map <- renderTmap({
			tm_shape(World) +
				tm_polygons(world_vars[1], zindex = 401)
		})
		
		observe({
			var <- input$var
			tmapProxy("map", session, {
				tm_remove_layer(401) +
				tm_shape(World) +
					tm_polygons(var, zindex = 401)
			})
		})
	}	
	
	
	app <- shinyApp(ui, server)
	if (interactive()) app
}
