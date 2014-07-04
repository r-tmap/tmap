process_layers <- function(g, gt, gf) {
	if (dupl <- anyDuplicated(names(g))) g <- g[-dupl]
	
	data <- g$geo_shape$data
	
	scale <- gt$scale
	
	if (!is.null(gf$by)) {
		data$GROUP_BY <- as.factor(data[[gf$by]])
		by <- levels(data$GROUP_BY)
	} else {
		data$GROUP_BY <- factor("_NA_")
		by <- NA
	}
	
	# determine plotting order 
	plot.order <- names(g)[names(g) %in% c("geo_fill", "geo_borders", "geo_text", "geo_bubbles", "geo_lines")]
	plot.order[plot.order=="geo_borders"] <- "geo_fill"
	plot.order <- unique(plot.order)
	
	# border info
	gborders <- if (is.null(g$geo_borders)) {
		list(col=NA, lwd=1, lty="blank")
	} else g$geo_borders
# 	gborders$lwd <- gborders$lwd * scale
	
	
	# fill info
	if (is.null(g$geo_fil)) {
		gfill <- list(fill=NULL, xfill=NA) 
	} else {
		gfill <- process_fill(data, g$geo_fill, gborders, gt, gf)
	}
	# bubble info
	if (is.null(g$geo_bubbles)) {
		gbubble <- list(bubble.size=NULL, xsize=NA, xcol=NA)
	} else {
		gbubble <- process_bubbles(data, g$geo_bubbles, gt, gf)
	}

# lines info
	if (is.null(g$geo_lines)) {
		glines <- list(line.lwd=NULL, xline=NA, xlinelwd=NA) 
	} else {
		glines <- process_lines(data, g$geo_lines, gt, gf)	
	} 
	
	
	# text info
	if (is.null(g$geo_text)) {
		gtext <- list(text=NULL)
	}  else {
		gtext <- process_text(data, g$geo_text, if (is.null(gfill$fill)) NA else gfill$fill)
	}

	c(list(npol=nrow(data), varnames=list(by=by, fill=gfill$xfill, bubble.size=gbubble$xsize, bubble.col=gbubble$xcol, line.col=glines$xline, line.lwd=glines$xlinelwd), plot.order=plot.order), gborders, gfill, glines, gbubble, gtext)
}