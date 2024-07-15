if (interactive() && require("shiny")) {
	
	data(World)
	world_vars <- setdiff(names(World), c("iso_a3", "name", "sovereignt", "geometry"))
	
	tmap_mode("plot")

	shinyApp(
		ui = fluidPage(
			tmapOutput("map", height = "600px"),
			selectInput("var", "Variable", world_vars)
		),
		server <- function(input, output, session) {
			output$map <- renderTmap({
				tm_shape(World) +
					tm_polygons(input$var, zindex = 401)
			})
		}
	)
	
	tmap_mode("view")

	shinyApp(
		ui = fluidPage(
			tmapOutput("map", height = "600px"),
			selectInput("var", "Variable", world_vars)
		),
		server <- function(input, output, session) {
			output$map <- renderTmap({
				tm_shape(World[1:3,], id = "iso_a3") +
					tm_polygons(world_vars[1], zindex = 401)
			})
			observe({
				var <- input$var
				tmapProxy("map", session, {
					tm_remove_layer(401) +
						tm_shape(World[1:3,], id = "iso_a3") +
						tm_polygons(var, zindex = 401)
				})
			})
		},options = list(launch.browser=TRUE)
	)
	
	shinyApp(
		ui = fluidPage(
			tmapOutput("map", height = "600px"),
			selectInput("var", "Variable", world_vars)
		),
		server <- function(input, output, session) {
			output$map <- renderTmap({
				tm_shape(World[1:3,], id = "iso_a3") +
					tm_symbols(fill = world_vars[1], zindex = 401)
			})
			observe({
				var <- input$var
				tmapProxy("map", session, {
					tm_remove_layer(401) +
						tm_shape(World[1:3,], id = "iso_a3") +
						tm_symbols(fill = var, zindex = 401)
				})
			})
		},options = list(launch.browser=TRUE)
	)
	
	shinyApp(
		ui = fluidPage(
			tmapOutput("map", height = "600px"),
			selectInput("var", "Variable", world_vars)
		),
		server <- function(input, output, session) {
			output$map <- renderTmap({
				tm_shape(World[1:3,], id = "iso_a3") +
					tm_text(world_vars[1], zindex = 401)
			})
			observe({
				var <- input$var
				tmapProxy("map", session, {
					tm_remove_layer(401) +
						tm_shape(World[1:3,], id = "iso_a3") +
						tm_text(var, zindex = 401)
				})
			})
		},options = list(launch.browser=TRUE)
	)
	
}
	

	