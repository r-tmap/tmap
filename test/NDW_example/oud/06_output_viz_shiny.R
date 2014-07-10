corop <- get_shape("../test/NDW_example/cr_2013.shp")
corop <- set_projection(corop, current.projection="rd")

corop2 <- get_shape("../test/NDW_example/corop_grenzen.shp")
corop2 <- set_projection(corop2, current.projection="rd")


geo_shape(corop) +
	geo_fill() +
	geo_borders("red") +
	geo_shape(corop2) +
	geo_lines(col="blue")


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

nuts <- read.csv2("../test/NDW_example/COROP-NUTS.csv", stringsAsFactor=FALSE)
nuts$NUTS1 <- as.integer(substr(nuts$NUTS3, 3, 3))
nuts$NUTS2 <- as.integer(substr(nuts$NUTS3, 3, 4))

corop <- append_data(corop, data=nuts, key.shp="CR2013", key.data="COROP")

# corop without islands (for borders version)
corop_borders <- corop
corop_borders <- corop_borders[!corop_borders$NUTS2 %in% c(23,34),]
corop_borders@polygons[[18]]@Polygons <- corop_borders@polygons[[18]]@Polygons[5]
corop_borders@polygons[[18]]@plotOrder <- 1L

corop_borders@polygons[[4]]@Polygons <- corop_borders@polygons[[4]]@Polygons[13]
corop_borders@polygons[[4]]@plotOrder <- 1L

corop_borders@polygons[[3]]@Polygons <- corop_borders@polygons[[3]]@Polygons[7]
corop_borders@polygons[[3]]@plotOrder <- 1L

gIsValid(corop_borders, reason = TRUE)


library(maptools)
nuts2 <- unionSpatialPolygons(corop_borders, corop_borders$NUTS2)
nuts1 <- unionSpatialPolygons(corop_borders, corop_borders$NUTS1)


save(corop, corop_borders, rwb_cr, rwb_crL, rwb_crR, nuts1, nuts2, file="../test/NDW_example/vis_demo_files.Rdata")
load("../test/NDW_example/vis_demo_files.Rdata")


########### run demo
library(shiny)
library(ggplot2)
library(scales)

df <- data.frame(date=as.POSIXct(substr(names(rwb_cr)[7:41], 1, 10)), mean=colMeans(rwb_cr@data[,7:41]))
df <- df[-seq(1, 35, by=5),]
df$date <- df$date + c(7*60*60, 13*60*60, 19*60*60,25*60*60)

item <- 


g <- qplot(date, mean, data=df, geom="line") + scale_x_datetime(breaks=date_breaks("day"))


palette <- "YlOrRd"
bg <- "gray65"


geo_shape(corop) +
	geo_fill(bg) +
	geo_shape(corop_borders) +
	geo_borders("white", lwd=.5) +
	geo_shape(nuts2) +
	geo_borders("white", lwd=1) +
	geo_shape(rwb_crL) +	
	geo_lines("2014-05-05_avond", lwd=1, max.categories=46, n=7, style="fixed", breaks=c(0, 12, 25, 37, 50, 75, 100, 250), palette=palette) +
	geo_shape(rwb_crR) +	
	geo_lines("2014-05-05_avond", lwd=1, max.categories=46, n=7, style="fixed", breaks=c(0, 12, 25, 37, 50, 75, 100, 250), palette=palette) +
	geo_theme(legend.show=TRUE, scale=2, legend.bg.color=bg)




runApp(list(
	ui= fluidPage(title="Verkeersindex",
		
		titlePanel("Prototype interactieve CBS visualizatie verkeersindex"),
		# Application title
		fluidRow(
			column(width=4,
				checkboxInput("rijrichting", "Rijrichting", FALSE),
				radioButtons("dagdelen", "Dagdelen", c("Alle", "Ochtend", "Middag", "Avond", "Nacht", "Gemiddeld"), selected="Alle"),
				conditionalPanel(condition="input.dagdelen != 'Alle'",
					sliderInput(inputId="dagen", "Dag in mei:", min=5, max=11, value=5, animate=TRUE)),
				conditionalPanel(condition="input.dagdelen == 'Alle'",
					sliderInput(inputId="dagen2", "Dag in mei:", min=5, max=11, value=5, step=.25,animate=TRUE)),
				plotOutput("graph", height="400px")
			),
			column(width=6,
				plotOutput("map", height="800px")
			)
		)
	),
	server = function(input, output) {
		
		output$graph <- renderPlot({
			
			alle <- (input$dagdelen=="Alle")
			dag <- if (alle) input$dagen2 else input$dagen
			
			if (alle) {
				rest <- dag - floor(dag)
				dag <- floor(dag)
				dagdeel <- c("ochtend", "middag", "avond", "nacht")[(rest+.25)*4] 
			} else {
				dagdeel <- c(Ochtend="ochtend", Middag="middag", Avond="avond", Nacht="nacht", Gemiddeld="alle")[input$dagdelen]
			}
			nm <- as.POSIXct(paste("2014-05-", sprintf("%02d", dag)))
			nm <- nm + ifelse(dagdeel=="ochtend",7*60*60, 
					   ifelse(dagdeel=="middag", 13*60*60, 
					   ifelse(dagdeel=="avond", 19*60*60,
					   ifelse(dagdeel=="nacht", 25*60*60, 0))))
			
			g2 <- g + geom_vline(xintercept=as.numeric(nm))
			
			print(g2)
		})
		
		output$map <- renderPlot({
			cat(input$dagen, file="testfile.txt")
			
			alle <- (input$dagdelen=="Alle")
			dag <- if (alle) input$dagen2 else input$dagen

			if (alle) {
				rest <- dag - floor(dag)
				dag <- floor(dag)
				dagdeel <- c("ochtend", "middag", "avond", "nacht")[(rest+.25)*4] 
			} else {
				dagdeel <- c(Ochtend="ochtend", Middag="middag", Avond="avond", Nacht="nacht", Gemiddeld="alle")[input$dagdelen]
			}
			nm <- paste("2014-05-", sprintf("%02d", dag), "_", dagdeel, sep="")
			

			
			cat(nm, file="testfile.txt")
			rr <- input$rijrichting
			
			
			if (rr) {
				geo_shape(corop) +
					geo_fill(bg) +
					geo_shape(corop_borders) +
					geo_borders("white", lwd=.5) +
					geo_shape(nuts2) +
					geo_borders("white", lwd=1) +
					geo_shape(rwb_crL) +	
					geo_lines(nm, lwd=1, max.categories=46, n=7, style="fixed", breaks=c(0, 12, 25, 37, 50, 75, 100, 250), palette=palette) +
					geo_shape(rwb_crR) +	
					geo_lines(nm, lwd=1, max.categories=46, n=7, style="fixed", breaks=c(0, 12, 25, 37, 50, 75, 100, 250), palette=palette) +
					geo_theme(legend.show=TRUE, scale=2, legend.bg.color=bg)
				
			} else {
				geo_shape(corop) +
					geo_fill(bg) +
					geo_shape(corop_borders) +
					geo_borders("white", lwd=.5) +
					geo_shape(nuts2) +
					geo_borders("white", lwd=1) +
					geo_shape(rwb_cr) +	
					geo_lines(nm, lwd=2, max.categories=46, n=7, style="fixed", breaks=c(0, 25, 50, 75, 100, 150, 200, 500), palette=palette) +
					geo_theme(legend.show=TRUE, scale=2, legend.bg.color=bg)
			}
		})
	}
))