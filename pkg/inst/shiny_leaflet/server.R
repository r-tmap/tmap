library(leaflet)

# From a future version of Shiny
bindEvent <- function(eventExpr, callback, env=parent.frame(), quoted=FALSE) {
	eventFunc <- exprToFunction(eventExpr, env, quoted)
	
	initialized <- FALSE
	invisible(observe({
		eventVal <- eventFunc()
		if (!initialized)
			initialized <<- TRUE
		else
			isolate(callback())
	}))
}




shinyServer(function(input, output, session) {
	makeReactiveBinding('selectedPoly')
	
	# create the map
	map <- createLeafletMap(session, 'map')
	
	polyInBounds <- reactive({
		if (is.null(input$map_bounds))
			return(data[FALSE,])
		bounds <- input$map_bounds
		latRng <- range(bounds$north, bounds$south)
		lngRng <- range(bounds$east, bounds$west)
		
		coords <- get_polygon_ranges(shp, key="name")$polygon.ranges
		sel <- ((coords[,4] >= latRng[1] & coords[,3] <= latRng[2]) |
					(coords[,3] <= latRng[2] & coords[,4] >= latRng[2])) &
			((coords[,2] >= lngRng[1] & coords[,1] <= lngRng[2]) |
			 	(coords[,1] <= lngRng[2] & coords[,2] >= lngRng[2]))
		
		cat("sum", sum(sel), "\n")
		data[sel, ]
	})
	
	observe({ 
		if (is.null(input$map_zoom)) return()
		defaultOpts=list(color=col2hex(gp$geoLayer1$col), 
						 weight=gp$geoLayer1$lwd * sqrt(input$map_zoom),
						 fillOpacity=input$opacity,
						 stroke=FALSE)
		
		# 					if(input$drawPoints == 0) {
		# 						return(NULL)
		# 					} else {
		
		map$clearShapes()
		
		map$addPolygon(
			inf$coords[, 2],
			inf$coords[, 1],
			layerId=inf$ids,
			options=inf$opts,
			defaultOptions=defaultOpts)
		# 					}
	})
	
	
	observe({
		if (is.null(input$map_click))
			return()
		selectedPoly <<- NULL
	})
	
	observe({
		event <- input$map_shape_click
		if (is.null(event))
			return()
		map$clearPopups()
		
		isolate({
			poly <- get_polyID(event$id)
			selectedPoly <<- poly
			content <- as.character(tagList(
				tags$strong(poly),
				tags$br()
			))
			center <- coordinates(shp)[poly, ]
			map$showPopup(center[2], center[1], content, event$id)
		})
	})
	
	output$legendPlot <- renderPlot({
		leg <- legend_prepare(gp, gt)
		
		if (is.null(leg)) return()
		legend_plot(gt, leg)
	})
	output$data <- renderDataTable({
		datasel <- polyInBounds()
		if (nrow(datasel) == 0)
			return(NULL)
		datasel
	}, options = list(bSortClasses = TRUE, 
					  aLengthMenu = c(5, 10, 20), 
					  iDisplayLength = 10))
	
	# 				options=list(scrollY="200px",
	# 								scrollCollapse=TRUE,
	# 								paginate=FALSE))
})