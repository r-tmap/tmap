if (interactive() && require("shiny")) {

	data(World)
	world_vars <- setdiff(names(World), c("iso_a3", "name", "sovereignt", "geometry"))

	current.mode <- tmap_mode("plot")

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
				tm_shape(World, id = "iso_a3") +
					tm_polygons(fill = world_vars[1], zindex = 401)
			})
			observe({
				var <- input$var
				tmapProxy("map", session, {
					tm_remove_layer(401) +
						tm_shape(World, id = "iso_a3") +
						tm_polygons(fill = var, zindex = 401)
				})
			})
		},options = list(launch.browser=TRUE)
	)

	tmap_mode(current.mode)
}
