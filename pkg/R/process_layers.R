process_layers <- function(g, z, gt, gf) {
	if (dupl <- anyDuplicated(names(g))) {
		warning(paste("One tm layer group has duplicated layer types, which are omitted. To draw multiple layers of the same type, use multiple layer groups (i.e. specify tm_shape prior to each of them)."))
		g <- g[-dupl]	
	} 
	
	data <- g$tm_shape$data
	
	scale <- gt$scale
		
	if (g$tm_shape$by=="") {
		data$GROUP_BY <- factor("_NA_")
		by <- NA
	} else {
		d <- data[[g$tm_shape$by]]
		data$GROUP_BY <- if (is.factor(d)) {
			factor(as.character(d), levels=levels(d)[table(d)>0])
		} else {
			factor(d)
		}
		by <- levels(data$GROUP_BY)
	}
	
	
	# determine plotting order 
	plot.order <- names(g)[names(g) %in% c("tm_fill", "tm_borders", "tm_text", "tm_bubbles", "tm_lines", "tm_raster")]
	plot.order[plot.order=="tm_borders"] <- "tm_fill"
	plot.order <- unique(plot.order)
	
	# border info
	gborders <- if (is.null(g$tm_borders)) {
		list(col=NA, lwd=1, lty="blank", alpha=NA)
	} else g$tm_borders
# 	gborders$lwd <- gborders$lwd * scale
	
	
	# fill info
	if (is.null(g$tm_fil)) {
		gfill <- list(fill=NULL, xfill=NA, fill.legend.title=NA) 
	} else {
		gfill <- process_fill(data, g$tm_fill, gborders, gt, gf, z=z+which(plot.order=="tm_fill"))
	}
	# bubble info
	if (is.null(g$tm_bubbles)) {
		gbubble <- list(bubble.size=NULL, xsize=NA, xcol=NA, bubble.size.legend.title=NA, bubble.col.legend.title=NA)
	} else {
		gbubble <- process_bubbles(data, g$tm_bubbles, gt, gf, z=z+which(plot.order=="tm_bubbles"))
	}

	# lines info
	if (is.null(g$tm_lines)) {
		glines <- list(line.lwd=NULL, xline=NA, xlinelwd=NA, line.col.legend.title=NA, line.lwd.legend.title=NA) 
	} else {
		glines <- process_lines(data, g$tm_lines, gt, gf, z=z+which(plot.order=="tm_lines"))	
	} 

	# raster info
	if (is.null(g$tm_raster)) {
		graster <- list(raster=NULL, xraster=NA, raster.legend.title=NA) 
	} else {
		graster <- process_raster(data, g$tm_raster, gt, gf, z=z+which(plot.order=="tm_raster"))
	}	
	
	
	# text info
	if (is.null(g$tm_text)) {
		gtext <- list(text=NULL)
	}  else {
		gtext <- process_text(data, g$tm_text, if (is.null(gfill$fill)) NA else gfill$fill)
	}

	c(list(npol=nrow(data), varnames=list(by=by, fill=gfill$xfill, bubble.size=gbubble$xsize, bubble.col=gbubble$xcol, line.col=glines$xline, line.lwd=glines$xlinelwd, raster=graster$xraster), data_by=data$GROUP_BY, plot.order=plot.order), gborders, gfill, glines, gbubble, gtext, graster)
}