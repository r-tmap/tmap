#' Print geo object
#' 
#' Print geo object
#' 
#' @param x geo object
#' @param ... not used
#' @import sp
#' @import RColorBrewer
#' @import grid
#' @import gridBase
#' @import classInt
#' @export
print.geo <- function(x, ...) {
	result <- process_geo(x)
	gmeta <- result$gmeta
	gps <- result$gps
	nx <- result$nx
	
	# backup par settings
	#opar <- par("mai", "xaxs", "yaxs")
	opar <- par(no.readonly=TRUE)
	
	par(mai=c(0,0,0,0), oma=c(0,0,0,0))
	plot.new()
	#grid.newpage()
	gridplot(gmeta$geo_grid$nrow, gmeta$geo_grid$ncol, "plot_all", nx, gps$shps, gps$multiples)
	do.call("par", opar)
	
}

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
	
	col2hex <- function(x) do.call(rgb, c(as.list(col2rgb(x)), list(maxColorValue=255)))
	
	bbox <- shps[[1]]@bbox
	
	m <- multi[[1]]
	shp <- shps[[1]]
	ids <- get_IDs(shp)
	
	runApp(
		list(
			server=function(input, output, session) {
				makeReactiveBinding('selectedPoly')
				
				# create the map
				map <- createLeafletMap(session, 'map')
				
				observe({
					if (is.null(input$map_click))
						return()
					selectedPoly <<- NULL
				})
				
				observe({
					event <- input$map_shape_click
					cat(event$id, "\n")
					if (is.null(event))
						return()
					map$clearPopups()
					
					isolate({
						poly <- paste0("Poly: ", event$id)
						selectedPoly <<- poly
						content <- as.character(tagList(
							tags$strong(poly),
							tags$br()
						))
						map$showPopup(event$lat, event$lng, content, event$id)
					})
				})
				
				observe({ 
					
					if(input$drawPoints == 0) {
						return(NULL)
					} else {
						
						map$clearShapes()
						
						
						
						# 					co <- coordinates(shp)
						# 					map$addCircle(
						# 						co[,1],
						# 						co[,2],
						# 						20 / max(5, input$map_zoom)^2,
						# 						shp$name,
						# 						list(
						# 							weight=1.2,
						# 							fill=TRUE,
						# 							color='#4A9'
						# 						)
						# 					)
						
						# 					map$addPolygon(
						# 						c(50.835317, 51.835317, 51.835317, 50.835317, 50.835317),
						# 						c(5.673065,  5.673065, 6.673065, 6.673065, 5.673065),
						# 						layerId=c("1"),
						# 						options=opts,
						# 						defaultOptions=opts)
						# 
						borderCol <-col2hex(m$geoLayer1$col)
						for (pi in 1:length(shp)) {
							p <- shp@polygons[[pi]]
							opts=list(color=borderCol, 
									  weigth=m$geoLayer1$lwd,
									  fillColor=m$geoLayer1$fill[pi], 
									  fillOpacity=input$opacity)
							i <- 0
							for (pp in p@Polygons) {
								co <- pp@coords
								co <- rbind(co, co[1,])
								i <- i + 1
								map$addPolygon(
									c(co[,2]),
									c(co[,1]),
									layerId=list(paste0(ids[pi], "_", i)),
									options=opts,
									defaultOptions=opts)
							}
						}
						
						
						
					}
				})
			},
			ui=fluidPage(
				leafletMap(
					"map", "100%", 600,
					initialTileLayer = "//{s}.tiles.mapbox.com/v3/jcheng.map-5ebohr46/{z}/{x}/{y}.png",
					initialTileLayerAttribution = HTML('Maps by <a href="http://www.mapbox.com/">Mapbox</a>'),
					options=list(
						center = c(mean(bbox[1,]), mean(bbox[2,])),
						zoom = 1,
						maxBounds = list(list(bbox[2,1], bbox[1,1]), list(bbox[2,2], bbox[1,2]))
					)
				),
				fluidRow(
					column(4, offset=2, 
						   h2("Choropleth"),
						   actionButton("drawPoints", "Draw"),
						   sliderInput("opacity", "Opacity", min=0, max=1, value=.7, step=.05)))
			))
	)
	
	
	
}
