library(shiny)

corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

gm <- get_shape("../shapes/gm_2012.shp")
gm <- set_projection(gm, current.projection="rd")

rwb_cr <- get_shape("../test/NDW_example/rw2013_doorgaand_cr.shp")
loops <- get_shape("../test/NDW_example/loops2013_classified.shp")

rwb_cr <- rwb_cr[!is.na(rwb_cr$ID), ]

rwb_cr_list <- double_line(rwb_cr, width=1000)

rwb_crL <- rwb_cr_list[[1]]
rwb_crL <- split_lines(rwb_crL, dist=100, include.last=FALSE)
rwb_crL <- fit_polylines(rwb_crL, id="ID",na.rm=FALSE)

rwb_crR <- rwb_cr_list[[2]]
rwb_crR <- split_lines(rwb_crR, dist=100, include.last=FALSE)
rwb_crR <- fit_polylines(rwb_crR, id="ID",na.rm=FALSE)


k <- length(rwb_cr)


#### generate random data
set.seed(20140703)
nms <- paste(rep(paste("2014-05-", sprintf("%02d", 5:11), sep=""), each=5), 
			 rep(c("alle", "ochtend", "middag", "avond", "nacht"), times=7), sep="_")

n <- length(nms)
tots <- seq(1, n, by=5)
n2 <- n-length(tots) 

baseline <- rep(1, n)
baseline[-tots] <- c(1.3, 1.1, 1.2, 0.5) * rlnorm(n2, meanlog=0, sdlog=.2)
baseline[tots] <- apply(matrix(baseline[-tots], ncol=4, byrow=TRUE), MARGIN=1,mean)

m <- matrix(baseline, ncol=n, nrow=k, byrow=TRUE)

baserw <- rnorm(46, mean=100, sd=4)[as.integer(rwb_cr$ID)]
basecr <- rlnorm(k, meanlog=0, sdlog=.2)

m <- m * baserw * basecr
	

mL <- m * rlnorm(length(m), meanlog=0, sdlog=.1) * .5
mR <- m - mL

m <- as.data.frame(m)
names(m) <- nms
mL <- as.data.frame(mL)
names(mL) <- nms
mR <- as.data.frame(mR)
names(mR) <- nms


rwb_cr <- append_data(rwb_cr, data=m, fixed.order=TRUE)

rwb_crL <- split_lines_poly(rwb_crL, corop)
rwb_crR <- split_lines_poly(rwb_crR, corop)

key <- paste(rwb_cr$ID, rwb_cr$CR2013)
keyL <- paste(rwb_crL$ID, rwb_crL$CR2013)
keyR <- paste(rwb_crR$ID, rwb_crR$CR2013)

rwb_crL <- rwb_crL[keyL %in% key, ]
rwb_crR <- rwb_crR[keyR %in% key, ]

rwb_crL <- append_data(rwb_crL, data=mL, fixed.order=TRUE)
rwb_crR <- append_data(rwb_crR, data=mR, fixed.order=TRUE)




runApp(list(
	ui= pageWithSidebar(
		
		# Application title
		headerPanel("Prototype interactieve CBS visualizatie verkeersindex"),
		
		# Sidebar with controls to select the variable to plot against mpg
		# and to specify whether outliers should be included
		sidebarPanel(
			checkboxInput("rijrichting", "Rijrichting", FALSE),
			radioButtons("dagdelen", "Dagdelen", c("Alle", "Ochtend", "Middag", "Avond", "Nacht", "Gemiddeld"), selected="Alle"),
			conditionalPanel(condition="input.dagdelen != 'Alle'",
				sliderInput(inputId="dagen", "Dag in mei:", min=5, max=11, value=5, animate=TRUE)),
			conditionalPanel(condition="input.dagdelen == 'Alle'",
				sliderInput(inputId="dagen2", "Dag in mei:", min=5, max=11, value=5, step=.25,animate=TRUE))
		),
		mainPanel(
			plotOutput("map", height="800px")
		)
	),
	server = function(input, output) {
		
		output$map <- renderPlot({
			cat(input$dagen, file="testfile.txt")
			
			alle <- (input$dagdelen=="Alle")
			dag <- if (alle) input$dagen else input$dagen2

			if (alle) {
				dagdeel <- "alle"
			} else {
				rest <- dag - floor(dag)
				dag <- floor(dag)
				if (input$dagdelen)
				dagdeel <- c("ochtend", "middag", "avond", "nacht")[(rest+.25)*4] 
			}
			nm <- paste("2014-05-", sprintf("%02d", dag), "_", dagdeel, sep="")
			

			
			cat(nm, file="testfile.txt")
			rr <- input$rijrichting
			
			
			if (rr) {
				geo_shape(corop) +
					geo_fill("gray70") +
					geo_borders("white") +
					geo_shape(rwb_crL) +	
					geo_lines(nm, lwd=1, max.categories=46, n=7, style="fixed", breaks=c(0, 12, 25, 37, 50, 75, 100, 250)) +
					geo_shape(rwb_crR) +	
					geo_lines(nm, lwd=1, max.categories=46, n=7, style="fixed", breaks=c(0, 12, 25, 37, 50, 75, 100, 250)) +
					geo_theme(legend.show=TRUE, scale=2)
			} else {
				geo_shape(corop) +
					geo_fill("gray70") +
					geo_borders("white") +
					#geo_shape(gm) +
					#geo_borders("white") +
					geo_shape(rwb_cr) +	
					geo_lines(nm, lwd=2, max.categories=46, n=7, style="fixed", breaks=c(0, 25, 50, 75, 100, 150, 200, 500)) +
					geo_theme(legend.show=TRUE, scale=2)
			}
		})
	}
))