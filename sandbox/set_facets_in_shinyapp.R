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

	output$twoPanels <- renderMapview({
		leafsync::latticeview(mapview(World), mapview(World))
		# 
		# tmFacet <- 
		# 	# render gdp per capita + pop density, synced
		# 	tm_shape(World) +
		# 	tm_polygons(c("gdp_cap_est", "pop_est_dens")) +
		# 	tm_facets(ncol = 2)
		# # default mode is "view"
		# tmap_leaflet(tmFacet)
	})
}

# Run the application 
shinyApp(ui = ui, server = server)
