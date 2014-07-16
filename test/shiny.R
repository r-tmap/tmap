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
				  fluidRow(
				  	column(width=4,
				  		   uiOutput("uishp1"),
				  		   uiOutput("uifill1"),
				  		   selectInput("borders1", label="Borders", choices=c("No borders", "gray, 1lwd", "gray, 2 lwd", "gray, 3 lwd", "black, 1 lwd", "black, 2 lwd", "black, 3 lwd"), selected="gray, 1lwd"),
				  		   uiOutput("uibubblesCol1"),
				  		   uiOutput("uibubblesSize1"),
				  		   uiOutput("uitext1"),
				  		   uiOutput("uitextSize1")
				  	),
				  	column(width=6,
				  		   plotOutput("map", height="800px")
				  	)
				  )
	),
	server = function(input, output) {
		
		output$uishp1 <- renderUI({
			selectInput("shp1", label="", choices=shps, selected=shps[1])
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
			borders <- input$borders1
			bubblesCol <- input$bubblesCol1
			bubblesSize <- input$bubblesSize1
			text <- input$text1
			textSize <- input$textSize1

			if (is.null(shpname) || is.null(fill) || is.null(borders) ||
					is.null(bubblesCol) || is.null(bubblesSize) || is.null(text) || is.null(textSize)) return()
			
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
				g <- g + geo_fill(col = fill)
			}
			if (bubblesCol != "No bubbles") {
				g <- g + geo_bubbles(col = bubblesCol, size=bubblesSize)
			}
			if (text != "No text") {
				g <- g + geo_text(text = text, cex = textSize)
			}
			g
		})
		
	}
))