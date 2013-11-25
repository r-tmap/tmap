library(shiny)

shinyUI(pageWithSidebar(
	
	# Application title
	headerPanel("NL cartography"),
	
	sidebarPanel(
		
	    tabsetPanel(
        tabPanel("Map",
    		uiOutput("shape"),
    		helpText("Only objects of the class SpatialPolygonsDataFrame",
    		         "are accepted."),
        	 uiOutput("textvar"),
        		 wellPanel(
                radioButtons("bb", label="Bounding Box:",
                             list("Original"="asis",
                                  "NL"="nl",
                                  "Randstand"="randstad",
                                  "Eindhoven"="eindhoven",
                                  "Zuid-Limburg"="zlimburg")),
                br(),
                uiOutput("xlim"),
                uiOutput("ylim"),
                checkboxInput("drawBB", label="Draw bounding box", value=FALSE))),
        
        
        
	    tabPanel("Method",
	        radioButtons("method", label="Method:", 
                         list("Plain"="Plain", "Choropleth"="Choropleth", 
                              "Bubble map"="Bubble map")),
        		helpText("Plain: just the shapes, ",
        		         "Choropleth: coloured shapes represent densities,",
        		         "Bubble map: bubbles represent absolute values"),
        		wellPanel(
                    conditionalPanel(
        			    condition = "input.method == 'Choropleth'",
        			        uiOutput("chorovar"),
        			    	radioButtons("quandens", 
        			    				 label="What type of variable is it?",
        			    				 list("quantity"="quantity",
        			    				 	 "density"="density")),
        			    	helpText("For choropleths, densities are required.",
        			    		 "When a quantity variable is chosen, this variable is",
        			    		 "temporarily transposed into a density variable"),
        		            selectInput("style", label="Clustering style:", 
        		            			choices=c("quantile", "pretty", 
        		            					  "equal", "kmeans")),
        			        numericInput("n", label="Number of clusters:", 
        			        			 value=5, min=2, max=10, step=1)),
        		    conditionalPanel(
                        condition = "input.method == 'Bubble map'",
                            uiOutput("bubblevar1"),
        		            sliderInput("scale", label="Scale factor:", 
        		            			min=0.1, max=10, value=1, format="#0.0"),
                        	uiOutput("bubblevar2"),
                        	conditionalPanel(
                        	condition = "input.bubblevar2 != 'red' & 
                        	input.bubblevar2 != 'blue'", uiOutput("bubblepal"))))),
        
        
        
        tabPanel("Layout",
        	wellPanel(
        		sliderInput("titleCex", label="Title font size:", 
        					min=0.1, max=4, value=1.5, format="#0.0"),
        		checkboxInput("showBorders", label="Show borders", value=TRUE),
        		conditionalPanel(
        			condition = "input.showBorders == true",
        			sliderInput("borderLwd", label="Line width:", 
        						min=0.1, max=5, value=1, format="#0.0")),
        		checkboxInput("addBorders", label="Additional borders", value=FALSE),
        		conditionalPanel(
        			condition = "input.addBorders == true",
        			uiOutput("shpBorders"),
        			sliderInput("addBorderLwd", label="Line width:", 
        						min=0.1, max=5, value=2, format="#0.0"))),
    		 wellPanel(
    		 	checkboxInput("showText", label="Show labels", value=FALSE),
    		 	conditionalPanel(
    		 		condition = "input.showText == true",
    		 		sliderInput("textCex", label="Text font size:", 
    		 					min=0.1, max=4, value=1, format="#0.0"))),
    		 wellPanel(
    		 	conditionalPanel(
    		 		condition = "(input.method == 'Bubble map' & 
    		 		input.bubblevar2 != 'red' & input.bubblevar2 != 'blue') | 
    		 		input.method == 'Choropleth'",
    		 		checkboxInput("showLegend", label="Show legend", value=TRUE),
    		 		sliderInput("legendSize", label="Font size legend:", 
    		 					min=0.1, max=3, value=1, format="#0.0")))
        ),
        
        
        
        tabPanel("Export",
        	wellPanel(
        		p("R code:"),
        		p(textOutput("caption")),
        		helpText("This R code may not be styled nicely.",
        				 "For completeness, all interactive arguments are",
        				 "specified.")),
        	wellPanel(
        	 	textInput("expname", label="Name:", value="map"),
        		radioButtons("expformat", label="Format:",
        		 			 list("pdf"="pdf",
        		 			 	 "png"="png",
        		 			 	 "bmp"="bmp",
        		 			 	 "jpeg"="jpeg")),
        		sliderInput("expwidth", label="Width (inches):", 
        				min=0.1, max=12, value=7, format="#0.0"),
        		sliderInput("expheight", label="Height (inches):", 
        	 			min=0.1, max=12, value=7, format="#0.0"),
        		downloadButton("pdf", "Download map"))
        ))),
	
	
	
	mainPanel(
		plotOutput("plot", height="auto", clickId="click", hoverId="hov"),
		tableOutput("plotinfo")
	)
))
