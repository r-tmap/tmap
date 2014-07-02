library(shiny)




runApp(list(
	ui= pageWithSidebar(
		
		# Application title
		headerPanel("Prototype interactieve CBS visualizatie verkeersindex"),
		
		# Sidebar with controls to select the variable to plot against mpg
		# and to specify whether outliers should be included
		sidebarPanel(
			checkboxInput("rijrichting", "Rijrichting", FALSE),
			radioButtons("dagdelen", "Dagdelen", c("Alle", "Ochtend", "Middag", "Avond", "Nacht"), selected="Alle"),
			sliderInput(inputId="dagen", "Dag in mei:", min=5, max=11, value=5)
		),
		mainPanel(
			plotOutput("map", height="800px")
		)
	),
	server = function(input, output) {
		
		output$map <- renderPlot({
			cat(input$dagen, file="testfile.txt")

			nm <- paste("2014-05-", sprintf("%02d", input$dagen), "_alle", sep="")
			
			cat(nm, file="testfile.txt")
			rr <- input$rijrichting
			geo_shape(corop) +
				geo_fill("gray70") +
				geo_borders("white") +
				#geo_shape(gm) +
				#geo_borders("white") +
				geo_shape(rwb_cr) +	
				geo_lines(nm, lwd=3, max.categories=46) +
				geo_theme(legend.show=TRUE)
		})
	}
))