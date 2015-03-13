process_layers <- function(g, gt, gf) {
	if (dupl <- anyDuplicated(names(g))) {
		warning(paste("One tm layer has duplicated drawing elements, which are omitted. To draw multiple of the same drawing element, use multiple layers (i.e. specify tm_shape prior to each of them)."))
		g <- g[-dupl]	
	} 
	
	data <- g$tm_shape$data
	
	scale <- gt$scale
	
	if (!is.null(gf$by) && gf$shp_name==g$tm_shape$shp_name) {
		data$GROUP_BY <- as.factor(data[[gf$by]])
		by <- levels(data$GROUP_BY)
	} else {
		data$GROUP_BY <- factor("_NA_")
		by <- NA
	}
	
	# determine plotting order 
	plot.order <- names(g)[names(g) %in% c("tm_fill", "tm_borders", "tm_text", "tm_bubbles", "tm_lines")]
	plot.order[plot.order=="tm_borders"] <- "tm_fill"
	plot.order <- unique(plot.order)
	
	# border info
	gborders <- if (is.null(g$tm_borders)) {
		list(col=NA, lwd=1, lty="blank", alpha=NA)
	} else g$tm_borders
# 	gborders$lwd <- gborders$lwd * scale
	
	
	# fill info
	if (is.null(g$tm_fil)) {
		gfill <- list(fill=NULL, xfill=NA) 
	} else {
		gfill <- process_fill(data, g$tm_fill, gborders, gt, gf)
	}
	# bubble info
	if (is.null(g$tm_bubbles)) {
		gbubble <- list(bubble.size=NULL, xsize=NA, xcol=NA)
	} else {
		gbubble <- process_bubbles(data, g$tm_bubbles, gt, gf)
	}

# lines info
	if (is.null(g$tm_lines)) {
		glines <- list(line.lwd=NULL, xline=NA, xlinelwd=NA) 
	} else {
		glines <- process_lines(data, g$tm_lines, gt, gf)	
	} 
	
	
	# text info
	if (is.null(g$tm_text)) {
		gtext <- list(text=NULL)
	}  else {
		gtext <- process_text(data, g$tm_text, if (is.null(gfill$fill)) NA else gfill$fill)
	}

	c(list(npol=nrow(data), varnames=list(by=by, fill=gfill$xfill, bubble.size=gbubble$xsize, bubble.col=gbubble$xcol, line.col=glines$xline, line.lwd=glines$xlinelwd), data_by=data$GROUP_BY, plot.order=plot.order), gborders, gfill, glines, gbubble, gtext)
}