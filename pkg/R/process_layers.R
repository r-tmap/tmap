process_layers <- function(g, free.scales.choro, free.scales.bubble.size, 
						   free.scales.bubble.col, legend.digits, legend.NA.text) {
	data <- g$geo_shape$data
	
	# determine plotting order 
	bubbleID <- which(names(g) == "geo_bubbles")
	textID <- which(names(g) == "geo_text")
	text.on.bubbles <-  if(!length(textID) || !length(bubbleID)) TRUE else (textID[1] > bubbleID[1])

	
	# border info
	gborders <- if (is.null(g$geo_borders)) {
		list(col=NA, lwd=1, lty="blank")
	} else g$geo_borders
	
	# fill info
	geofill <- if (is.null(g$geo_fil)) geo_fill(col=NA)$geo_fill else g$geo_fill
	gfill <- process_fill(data, geofill, free.scales.choro, legend.digits, legend.NA.text)

	# bubble info
	geobubbles <- if (is.null(g$geo_bubbles)) geo_bubbles(size=NULL)$geo_bubbles else g$geo_bubbles
	gbubble <- process_bubblemap(data, geobubbles, free.scales.bubble.size, free.scales.bubble.col, 
								 legend.digits, legend.NA.text)
	
	# lines info
	glines <- if (is.null(g$geo_lines)) geo_lines(col=NA)$geo_lines else g$geo_lines
	
	
	# text info
	gtext <- if (is.null(g$geo_text)) list(text=NULL) else g$geo_text
	c(list(varnames=list(choro.fill=gfill$xfill, bubble.size=gbubble$xsize, bubble.col=gbubble$xcol), text.on.bubbles=text.on.bubbles), gborders, gfill, glines, gbubble, gtext)
}