library(shiny)
library(leaflet)

devtools::load_all()
#library(tmap)
# import data
data(World)

library(mapview)
library(leafsync)



# Define UI for application that draws static and slippy maps
ui <- fluidPage(
	
	# Application title
	titlePanel("Example maps"),
	# Sidebar with a slider input for year of interest
	sidebarLayout(
		sidebarPanel(),
		# Show two maps
		mainPanel(
			h2("facets"),
			leafletOutput("twoPanels")
		)
	)
)

# Define server logic
server <- function(input, output) {
	# single panel map

	output$twoPanels <- renderTmap({

		tm_shape(World) +
		#tm_polygons("gdp_cap_est") +
		tm_layout(title = "Test", bg.color = "black") +
		tm_polygons(c("gdp_cap_est", "pop_est_dens")) +
		tm_facets(ncol = 2, as.layers = TRUE)
	})
}

# Run the application 
shinyApp(ui = ui, server = server)
