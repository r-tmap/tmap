process_layers <- function(g, free.scales.fill, free.scales.bubble.size, 
						   free.scales.bubble.col, free.scales.line.col, legend.digits, legend.NA.text) {
	data <- g$geo_shape$data
	
	# determine plotting order 
	fillID <- which(names(g) == "geo_fill")
	lineID <- which(names(g) == "geo_lines")
	bubbleID <- which(names(g) == "geo_bubbles")
	textID <- which(names(g) == "geo_text")
	
	plot.order <- names(g)[names(g) %in% c("geo_fill", "geo_borders", "geo_text", "geo_bubbles", "geo_lines")]
	plot.order[plot.order=="geo_borders"] <- "geo_fill"
	plot.order <- unique(plot.order)
	
	# border info
	gborders <- if (is.null(g$geo_borders)) {
		list(col=NA, lwd=1, lty="blank")
	} else g$geo_borders
	
	# fill info
	geofill <- if (is.null(g$geo_fil)) geo_fill(col=NA)$geo_fill else g$geo_fill
	gfill <- process_fill(data, geofill, free.scales.fill, legend.digits, legend.NA.text)

	# bubble info
	geobubbles <- if (is.null(g$geo_bubbles)) geo_bubbles(size=NULL)$geo_bubbles else g$geo_bubbles
	gbubble <- process_bubbles(data, geobubbles, free.scales.bubble.size, free.scales.bubble.col, legend.digits, legend.NA.text)
	
	# lines info
	glines <- if (is.null(g$geo_lines)) list(line.col=NA, xline=NA) else process_lines(data, g$geo_lines, free.scales.line.col, legend.digits, legend.NA.text)
	
	# text info
	gtext <- if (is.null(g$geo_text)) list(text=NULL) else process_text(data, g$geo_text, gfill$fill)
	
	c(list(npol=nrow(data), varnames=list(choro.fill=gfill$xfill, bubble.size=gbubble$xsize, bubble.col=gbubble$xcol, line.col=glines$xline), plot.order=plot.order), gborders, gfill, glines, gbubble, gtext)
}