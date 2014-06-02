process_layers <- function(g, gt) {
	facets_defined <- ("geo_facets" %in% names(g))
	if (!facets_defined) g <- c(g, geo_facets())
	if (dupl <- anyDuplicated(names(g))) g <- g[-dupl]
	
	data <- g$geo_shape$data
	
	gby <- g$geo_facets
	
	
	data$GROUP_BY <- if (!is.null(gby$by)) {
		 as.factor(data[[gby$by]])
	} else {
		factor("_NA_")
	}
	facets <- levels(data$GROUP_BY)
	
	# determine plotting order 
	plot.order <- names(g)[names(g) %in% c("geo_fill", "geo_borders", "geo_text", "geo_bubbles", "geo_lines")]
	plot.order[plot.order=="geo_borders"] <- "geo_fill"
	plot.order <- unique(plot.order)
	
	# border info
	gborders <- if (is.null(g$geo_borders)) {
		list(col=NA, lwd=1, lty="blank")
	} else g$geo_borders
	
	# fill info
	geofill <- if (is.null(g$geo_fil)) geo_fill(col=NA)$geo_fill else g$geo_fill
	gfill <- process_fill(data, geofill, gt, gby)

	# bubble info
	geobubbles <- if (is.null(g$geo_bubbles)) geo_bubbles(size=NULL)$geo_bubbles else g$geo_bubbles
	gbubble <- process_bubbles(data, geobubbles, free.scales.bubble.size, free.scales.bubble.col, legend.digits, legend.NA.text)
	
	# lines info
	glines <- if (is.null(g$geo_lines)) list(line.col=NA, xline=NA, xlinelwd=NA) else process_lines(data, g$geo_lines, free.scales.line.col, legend.digits, legend.NA.text)
	
	# text info
	gtext <- if (is.null(g$geo_text)) list(text=NULL) else process_text(data, g$geo_text, gfill$fill)

	c(list(npol=nrow(data), varnames=list(fill=gfill$xfill, bubble.size=gbubble$xsize, bubble.col=gbubble$xcol, line.col=glines$xline, line.lwd=glines$xlinelwd), plot.order=plot.order, facets_defined=facets_defined), gborders, gfill, glines, gbubble, gtext)
}