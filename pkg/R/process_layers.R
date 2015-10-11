process_layers <- function(g, z, gt, gst, gf) {
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
	
	aes.color.light <- sum(col2rgb(gt$aes.color) * c(.299, .587, .114)) >= 128
	
	# determine plotting order 
	plot.order <- names(g)[names(g) %in% c("tm_fill", "tm_borders", "tm_text", "tm_bubbles", "tm_lines", "tm_raster")]
	plot.order[plot.order=="tm_borders"] <- "tm_fill"
	plot.order <- unique(plot.order)
	
	# border info
	gborders <- if (is.null(g$tm_borders)) {
		list(col=NULL, lwd=1, lty="blank", alpha=NA)
	} else g$tm_borders
	if (!is.null(gborders$col)) {
		if (is.na(gborders$col)) {
			gborders$col <- ifelse(aes.color.light, darker(gt$aes.color, .4), lighter(gt$aes.color, .4))
		}
	} else {
		gborders$col <- NA
	}
	gborders$col <- do.call("process_color", c(list(col=gborders$col, alpha=gborders$alpha), gst))
	
# 	gborders$lwd <- gborders$lwd * scale
	
	
	# fill info
	if (is.null(g$tm_fil)) {
		gfill <- list(fill=NULL, xfill=NA, fill.legend.title=NA, fill.id=NA) 
	} else {
		gfill <- process_fill(data, g$tm_fill, gborders, gt, gst, gf, z=z+which(plot.order=="tm_fill"))
	}
	# bubble info
	if (is.null(g$tm_bubbles)) {
		gbubble <- list(bubble.size=NULL, xsize=NA, xcol=NA, bubble.size.legend.title=NA, bubble.col.legend.title=NA, bubble.id=NA)
	} else {
		gbubble <- process_bubbles(data, g$tm_bubbles, gt, gst, gf, z=z+which(plot.order=="tm_bubbles"))
	}

	# lines info
	if (is.null(g$tm_lines)) {
		glines <- list(line.lwd=NULL, xline=NA, xlinelwd=NA, line.col.legend.title=NA, line.lwd.legend.title=NA, line.id=NA) 
	} else {
		glines <- process_lines(data, g$tm_lines, gt, gst, gf, z=z+which(plot.order=="tm_lines"))	
	} 

	# raster info
	if (is.null(g$tm_raster)) {
		graster <- list(raster=NULL, xraster=NA, raster.legend.title=NA) 
	} else {
		graster <- process_raster(data, g$tm_raster, gt, gst, gf, z=z+which(plot.order=="tm_raster"))
	}	
	
	
	# text info
	if (is.null(g$tm_text)) {
		gtext <- list(text=NULL)
	}  else {
		gtext <- process_text(data, g$tm_text, if (is.null(gfill$fill)) NA else gfill$fill, gt, gst)
	}

	c(list(npol=nrow(data), varnames=list(by=by, fill=gfill$xfill, bubble.size=gbubble$xsize, bubble.col=gbubble$xcol, line.col=glines$xline, line.lwd=glines$xlinelwd, raster=graster$xraster), idnames=list(fill=gfill$fill.id, bubble=gbubble$bubble.id, line=glines$line.id), data_by=data$GROUP_BY, plot.order=plot.order), gborders, gfill, glines, gbubble, gtext, graster)
}