# data(NLD_prov)
# g <- geo_shape(NLD_prov) + geo_choropleth("pop") + geo_borders()
# library(shiny)
# library(leaflet)

#' Interactive geo maps
#' 
#' Interactive choropleth and bubble maps
#' 
#' @param g geo object
#' @import leaflet
#' @import shiny
#' 
igeo <- function(g) {
	shape.id <- which(names(g)=="geo_shape")[1]
	
	g[[shape.id]]$projection <- "longlat"
	
	result <- process_geo(g)
	gmeta <- result$gmeta
	shps <- result$gps$shps
	multi <- result$gps$multiples
	nx <- result$nx
	
	
	bbox <- shps[[1]]@bbox
	
	gp <- multi[[1]]
	shp <- shps[[1]]
	ids <- get_IDs(shp)
	
	inf <- get_polygon_info(shp, fill=gp$geoLayer1$fill)

	gt <- gp$geo_theme
	
	gp[c("geo_theme")] <- NULL
	
	data <- shp@data
	
	runApp(
		list(
			server=function(input, output, session) {
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

			},
			ui=fluidPage(
				fluidRow(
					h2("Choropleth")
				),
				fixedRow(
					column(6, 
						   leafletMap(
						   	"map", "100%", 650,
						   	initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
						   	initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
						   	options=list(
						   		center = c(mean(bbox[2,]), mean(bbox[1,])),
						   		zoom = zoomlevel(bbox),
						   		maxBounds = list(list(bbox[2,1], 
						   							  bbox[1,1]), 
						   						 list(bbox[2,2],
						   						 	 bbox[1,2])))
						   )),
					column(6, 
						   h4('Visible regions'),
						   dataTableOutput('data')
					)
				),
				fluidRow(
					column(4, offset=1,
						   plotOutput("legendPlot", "100%", 650)
					),
					column(4, 
						   h4("Settings"),
						   sliderInput("opacity", "Opacity", 
						   			min=0, max=1, value=.5, step=.05)
					)
				)
			))
	)
	
	
	
}
