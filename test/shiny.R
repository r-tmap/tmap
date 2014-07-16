data(Europe)
data(World)
data(NLD_muni)
data(NLD_prov)

shps <- ls(envir=.GlobalEnv)
is.spat <- sapply(shps, function(s)inherits(get(s, .GlobalEnv), "SpatialPolygonsDataFrame"))
shps <- shps[is.spat]


allvars <- lapply(shps, function(s) {
	names(get(s, .GlobalEnv))
})
names(allvars) <- shps

numvars <- lapply(shps, function(s) {
	x <- sapply(get(s, .GlobalEnv)@data, is.numeric)
	names(x)[x]
})
names(numvars) <- shps

library(shiny)



runApp(list(
	ui= fluidPage(title="geo",
				  titlePanel("geo maps"),
				  
				  # Application title
				  sidebarPanel(width=6,
				  			 
				  	uiOutput("uishp1"),
				  	helpText("This GUI only accepts SpatialPolygonDataFrames."),
					wellPanel(
				  		uiOutput("uifill1"),
				  		checkboxInput("density1", "Convert to densities", FALSE),
				  		selectInput("style1", label="Style", choices=c("equal", "pretty", "quantile", "kmeans"), selected="kmeans")),
					wellPanel(
						selectInput("borders1", label="Borders", choices=c("No borders", "gray, 1 lwd", "gray, 2 lwd", "gray, 3 lwd", "black, 1 lwd", "black, 2 lwd", "black, 3 lwd"), selected="gray, 1lwd")),
					wellPanel(
				  		uiOutput("uibubblesCol1"),
				  		uiOutput("uibubblesSize1"),
						sliderInput("bubblesScale1", "Scale (bubbles)", min=.1, max=3, value=1, step=.1)),
					wellPanel(
						uiOutput("uitext1"),
				  		uiOutput("uitextSize1"),
						sliderInput("textScale1", "Scale (text)", min=.1, max=3, value=1, step=.1)),
					sliderInput("scale1", "Scale (overall)", min=.1, max=3, value=1, step=.1)),
				  mainPanel(
				  		   plotOutput("map", height="800px")
				  )
	),
	server = function(input, output) {
		
		output$uishp1 <- renderUI({
			selectInput("shp1", label="Shape", choices=shps, selected=shps[1])
		})
		
		output$uifill1 <- renderUI({
			shpname <- input$shp1
			if (!is.null(shpname)) {
				opts <- c("No fill", "gray60", "gray70", "gray80", "gray90", allvars[[shpname]])
				
				selectInput("fill1", label="Fill", choices=opts, selected="gray80")
			}
		})
		
		output$uibubblesCol1 <- renderUI({
			shpname <- input$shp1
			if (!is.null(shpname)) {
				opts <- c("No bubbles", "purple", "red", "blue", allvars[[shpname]])
				
				selectInput("bubblesCol1", label="Bubbles color", choices=opts, selected="No bubbles")
			}
		})

		output$uibubblesSize1 <- renderUI({
			shpname <- input$shp1
			if (!is.null(shpname)) {
				opts <- c("0.5", "1.0", "1.5", "2.0", numvars[[shpname]])
				
				selectInput("bubblesSize1", label="Bubbles size", choices=opts, selected="1")
			}
		})
		
		output$uitext1 <- renderUI({
			shpname <- input$shp1
			if (!is.null(shpname)) {
				shp <- get(input$shp1, .GlobalEnv)
				opts <- c("No text", names(shp))
				
				selectInput("text1", label="Text", choices=opts, selected="No text")
			}
		})
		
		output$uitextSize1 <- renderUI({
			shpname <- input$shp1
			if (!is.null(shpname)) {
				opts <- c("0.5", "1.0", "1.5", "2.0", numvars[[shpname]])
				
				selectInput("textSize1", label="Text size", choices=opts, selected="1")
			}
		})
		
		
		output$map <- renderPlot({
			shpname <- input$shp1
			fill <- input$fill1
			density <- input$density1
			borders <- input$borders1
			bubblesCol <- input$bubblesCol1
			bubblesSize <- input$bubblesSize1
			bubblesScale <- input$bubblesScale1
			text <- input$text1
			textSize <- input$textSize1
			textScale <- input$textScale1
			scale <- input$scale1
			
			if (is.null(shpname) || is.null(fill) || is.null(density) || is.null(borders) ||
					is.null(bubblesCol) || is.null(bubblesSize) || is.null(bubblesScale) || is.null(text) || is.null(textSize) || is.null(textScale) || is.null(scale)) return()
			
			if (borders=="No borders") {
				bordersCol <- bordersLwd <- NA
			} else {
				blist <- strsplit(borders, split = ", ", fixed=TRUE)[[1]]
				bordersCol <- blist[1]
				bordersLwd <- as.integer(substr(blist[2], 1, 1))
			}
			
			suppressWarnings({
				if (!is.na(as.numeric(bubblesSize))) {
					bubblesSize <- as.numeric(bubblesSize)
				}
			})
			suppressWarnings({
				if (!is.na(as.numeric(textSize))) {
					textSize <- as.numeric(textSize)
				}
			})

			shp <- get(input$shp1, .GlobalEnv)
			g <- geo_shape(shp) + geo_borders(col=bordersCol, lwd = bordersLwd)
			if (fill != "No fill") {
				g <- g + geo_fill(col = fill, convert2density = density)
			}
			if (bubblesCol != "No bubbles") {
				g <- g + geo_bubbles(col = bubblesCol, size=bubblesSize, scale=bubblesScale)
			}
			if (text != "No text") {
				g <- g + geo_text(text = text, cex = textSize, scale=textScale)
			}
			g + geo_theme(scale=scale)
		})
		
	}
))