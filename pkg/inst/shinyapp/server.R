library(shiny)
library(geoNL)

obs <- ls(envir=.GlobalEnv)
shps <- obs[sapply(obs, function(x)inherits(get(x, envir=.GlobalEnv), c("SpatialPolygonsDataFrame")))]

numvars <- lapply(shps, function(shpname){
    shp <- get(shpname)
    names(shp@data)[sapply(shp@data, is.numeric)]})
names(numvars) <- shps

vars <- lapply(shps, function(shpname){
    shp <- get(shpname)
    names(shp@data)})
names(vars) <- shps

bbox <- lapply(shps, function(shpname){
    shp <- get(shpname)
    shp@bbox})
names(bbox) <- shps

createPlot <- function(shpname, input, plotMap=TRUE){
	shp <- get(shpname)
	method <- input$method
	
	
	## process bounding box info
	xlim <- input$xlim
	ylim <- input$ylim
	if (is.null(xlim) || is.null(ylim)) {
		return()
	}
	
	## process border info
	showBorders <- input$showBorders
	borderLwd <- round(input$borderLwd, 1)
	borderLwd2 <- round(input$addBorderLwd, 1)
	if (!input$addBorders) {
		shp2 <- NULL
		rcode_shp2 <- ""
	} else {
		shp2 <- get(input$shpBorders)
		rcode_shp2 <- paste(", shp2=", input$shpBorders, ", lwd2=", borderLwd2)
	}
	
	## process text info
	textvar <- input$textvar
	if (is.null(textvar) | !input$showText) {
		rcode_text <- ""
		textvar <- NULL
	} else {
		rcode_text <- paste0(", labels=", shpname, "$", textvar)
		textvar <- shp@data[[textvar]]
	}
	
	## process font sizes
	title.cex <- input$titleCex
	labels.cex <- input$textCex
	
	if (plotMap) plot.new()
	
	
	## general args
	args <- list(xlim=xlim, ylim=ylim, 
				 labels=textvar, labels.cex=labels.cex, units="abs",
				 plot.borders=showBorders, lwd=borderLwd, 
				 shp2=shp2, lwd2=borderLwd2)
	rcode_args <- paste0(", xlim=c(", 
					paste(xlim, collapse=","), "), ylim=c(", paste(ylim, collapse=","),
					")", rcode_text, ", units=\"abs\", title.cex=", title.cex,
					", plot.borders=", showBorders, ", lwd=", 
						 borderLwd, rcode_shp2, ")")
	
	rcode <- ""
	if (method=="Plain") {
		if (plotMap) {
			do.call(cartoMap, c(list(shp=shp), args))
			#plot(shp)
		} else {
			rcode <- paste0("cartoMap(", shpname, rcode_args)
		}
	} else if (method=="Choropleth") {
		var <- input$chorovar
		if (!(var %in% numvars[[shpname]])) return()
		style <- input$style
		n <- input$n
		quandens <- input$quandens
		show.legend.text <- input$showLegend
		legend.cex <- input$legendSize
		if (quandens=="quantity") {
			shp$temp_density_variable <- densities(shp, var=var)
			title <- paste(var, "per km2")
			rcode <- paste0("temp_density_variable <- densities(", 
							shpname, ", var=", var, "); ")
			rcode_title <- paste0(", title=", title)
			var <- "temp_density_variable"
		} else {
			title <- var
			rcode_title <- ""
		}
		
		if (plotMap) {
			do.call(choropleth, c(list(shp=shp, x=var, n=n, style=style, show.legend.text=show.legend.text, legend.cex=legend.cex, title=title, title.cex=title.cex), args))
		} else {
			rcode <- paste0(rcode, "choropleth(", shpname, ", \"", var, "\", n=", n, ", style=\"", style, ", title.cex=", title.cex, ", show.legend.text=", show.legend.text, ", legend.cex=", legend.cex, rcode_title, rcode_args)
		}
	} else if (method=="Bubble map") {
		var1 <- input$bubblevar1
		if (!(var1 %in% numvars[[shpname]])) return()
			
		scale <- input$scale
		var2 <- input$bubblevar2
		pal <- input$bubblepal
		show.legend <- input$showLegend
		legend.cex <- input$legendSize
		if (plotMap) {
			do.call(bubbleMap, c(list(shp=shp, x=var1, col=var2, scale=scale, 
									  palette=pal, title.cex=title.cex,
									  show.legend.colors=show.legend, show.legend.sizes=show.legend, legend.cex=legend.cex), args))
		} else {
			rcode <- paste0("bubbleMap(", shpname, ", \"", var1, "\", col=\"", var2,
							", scale=", scale, ", palette=", pal, 
							", title.cex=", title.cex,
							"\", show.legend.colors=", show.legend, ", show.legend.sizes=", show.legend, ", legend.cex=", legend.cex, rcode_args)
		}
	}
	
	
	if (input$drawBB) {
		if (plotMap) {
			lines(c(xlim[1], rep(xlim[2],4), rep(xlim[1],3)), 
				  c(rep(ylim[1],3), rep(ylim[2],4), ylim[1]))
		} else {
			rcode <- paste(rcode,
						   paste("lines(c(", xlim[1], ", rep(", xlim[2], ",4), rep(", xlim[1], ", 3)), c(rep(", ylim[1], ",3), rep(", ylim[2], ",4), ", ylim[1], "))"), sep="; ")
		}
	}    	
	if (!plotMap) rcode
}

shinyServer(function(input, output) {
	

    shapeName <- reactive({
        ifelse(is.null(input$shp), shps[1], input$shp)
    })
    
    
    getBBX <- reactive({
    	bbSel <- input$bb
#     	relBB <- input$relBB
#     	if (is.null(relBB)) return()
     	shapeN <- shapeName()
     	bb <- bbox[[shapeN]]
    	
    	xrange <- switch(bbSel, asis=bb[1,], 
    		   nl=c(13565.4, 277992.8), 
    		   randstad=c(68751, 150195), 
    		   eindhoven=c(139618, 181926), 
    		   zlimburg=c(172407, 205196))
        c(max(bb[1,1], xrange[1]), min(bb[1,2], xrange[2]))
    })
    
    getBBY <- reactive({
    	bbSel <- input$bb
    	#     	relBB <- input$relBB
    	#     	if (is.null(relBB)) return()
    	shapeN <- shapeName()
    	bb <- bbox[[shapeN]]
    	
    	yrange <- switch(bbSel, asis=bb[2,], 
    		   nl=c(306846.9, 619217.0), 
    		   randstad=c(427443, 501184), 
    		   eindhoven=c(367451, 399947), 
    		   zlimburg=c(306846.9, 344954))
    	c(max(bb[2,1], yrange[1]), min(bb[2,2], yrange[2]))
    })    
    

    
    output$shape <- renderUI({
		#shp <- shape()
		#vars <- colnames(shp@data)
		selectInput("shp", label="Shape object:", choices=shps)
	})
    
    output$chorovar <- renderUI({
        shapeN <- shapeName()
        vs <- numvars[[shapeN]]
        selectInput("chorovar", label="Variable:", choices=vs)
    })
    
    output$bubblevar1 <- renderUI({
        shapeN <- shapeName()
		vs <- numvars[[shapeN]]
		selectInput("bubblevar1", label="Size variable:", choices=vs)
	})

    output$bubblevar2 <- renderUI({
        shapeN <- shapeName()
        vs <- c("red", "blue", vars[[shapeN]])
        names(vs) <- c("<red>", "<blue>", vars[[shapeN]])
        selectInput("bubblevar2", label="Colour (variable):", choices=vs)
    })

    output$bubblepal <- renderUI({
    	shapeN <- shapeName()
    	var2 <- input$bubblevar2
    	
    	value <- ifelse(var2 %in% numvars[[shapeN]], "Blues", "Set3")
    	
    	selectInput("bubblepal", label="Palette:", choices=c("BrBG", "PiYG", "PRGn", "PuOr", "RdBu", "RdGy", "RdYlBu", "RdYlGn", "Spectral", "Accent", "Dark2", "Paired", "Pastel1", "Pastel2", "Set1", "Set2", "Set3", "Blues", "BuGn", "BuPu", "GnBu", "Greens", "Greys", "Oranges", "OrRd", "PuBu", "PuBuGn", "PuRd", "Purples", "RdPu", "Reds", "YlGn", "YlGnBu", "YlOrBr", "YlOrRd"), selected=value)})
    
    
    output$textvar <- renderUI({
        shapeN <- shapeName()
        vs <- vars[[shapeN]]
        selectInput("textvar", label="Label variable:", choices=vs)
    })

    
    output$xlim <- renderUI({
    	#cat("xlim\n")
    	shapeN <- shapeName()
    	bb <- bbox[[shapeN]]
    	sliderInput("xlim", label="x limits:", 
        			min=bb[1,1], max=bb[1,2], value=getBBX(), format="#0.0")
    })
        
        
    output$ylim <- renderUI({
    	#cat("ylim\n")
    	shapeN <- shapeName()
    	bb <- bbox[[shapeN]]
    	sliderInput("ylim", label="y limits:", 
    				min=bb[2,1], max=bb[2,2], value=getBBY(), format="#0.0")
    })

    
    output$shpBorders <- renderUI({
    	#shp <- shape()
    	#vars <- colnames(shp@data)
    	shapeN <- shapeName()
    	selectInput("shpBorders", label="Shape object:", choices=shps, selected=shapeN)
    })
    
    
    output$pdf <- downloadHandler(
    	filename = function() { paste(input$expname, input$expformat, sep=".")},
    	content = function(file) {
    		width <- input$expwidth
    		height <- input$expheight
    		
    		file2 <- paste(file, input$expformat, sep=".")
    		saveMap(createPlot(shapeName(), input), file=file2, width=width, height=height)
    		file.rename(file2, file)
    		
    		
#     		if (input$expformat=="pdf") {
#     			pdf(file=file, width = width, height = height)
#     		} else {
#     			do.call(input$expformat, list(file = file, width = width, height = height, units="in", res=200))
#     		}
#     		
#     		dev.off()
    	},
    	contentType = paste("image", input$expformat, sep="/")
    )
    
    output$caption <- renderText({
    	createPlot(shapeName(), input, plotMap=FALSE)
    })
    
	output$plot <- renderPlot({
		createPlot(shapeName(), input)
	}, height=800)
    
    output$plotinfo <- renderTable({ 
    	shp <- get(shapeName())
    	
    	if (!is.null(input$click)) {
	    	points <- SpatialPoints(input$click)
	    	x <- over(points, shp)[1,]
	    	
	    	out <- input$textvar
	    	
	    	method <- input$method
	    	#shp@data[[textvar]]
	    	
	    	if (method=="Choropleth") {
	    		out <- c(out, input$chorovar)
	    	} else if (method=="Bubble map") {
	    		out <- c(out, input$bubblevar1, input$bubblevar2)
	    	}
	    	out <- intersect(out, names(x))
	    	x[1, out, drop=FALSE]
    	}
    }, include.rownames=FALSE)
    
})