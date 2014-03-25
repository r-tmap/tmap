process_layers <- function(g, free.scales, proj) {
	shp <- get(g$geo_shape$shp)

	# check proj4 string
	shp.proj <- proj4string(shp)
	if (!is.null(proj$projection)) {
		if (is.na(shp.proj)) {
			warning("Currect projection of shape object unknown. Long-lat (WGS84) is assumed.")
			shp.proj <- CRS("+proj=longlat +datum=WGS84")
			proj4string(shp) <- shp.proj
		}
		spTransform(shp, proj$projection)
	}	
	
	# set bounding box
	shp.bbox <- bbox(shp)
	if (!is.null(proj$bbox)) {
		bbox <- proj$bbox
	} else {
		if (proj$relative) {
			steps <- shp.bbox[, 2] - shp.bbox[, 1]
			xlim <- shp.bbox[1,1] + proj$xlim * steps[1]
			ylim <- shp.bbox[2,1] + proj$ylim * steps[2]
		}
		bbox <- matrix(c(xlim, ylim), ncol = 2, byrow=TRUE, 
						dimnames=list(c("x", "y"), c("min", "max")))
	}
	shp@bbox <- bbox
	
	# border info
	if (is.null(g$geo_borders)) {
		col <- NA
		lwd <- 1
		lty <- "blank"
	} else {
		col <- g$geo_borders$col
		lwd <- g$geo_borders$lwd
		lty <- g$geo_borders$lty
	}
	
	# fill info
	if (is.null(g$geo_choropleth)) {
		if (is.null(g$geo_fil)) {
			fill <- NA
		} else {
			fill <- g$geo_fill$col
		}
		choro.values <- NA
		choro.legend.labels <- NA
		choro.legend.palette <- NA
		choro.breaks <- NA
		xfill <- NA
	} else {
		chorores <- process_choro(shp, g$geo_choropleth, free.scales)

		fill <- chorores$fill
		choro.values <- chorores$choro.values
		choro.legend.labels <- chorores$choro.legend.labels
		choro.legend.palette <- chorores$choro.legend.palette
		choro.breaks <- chorores$choro.breaks
		xfill <- chorores$xfill
	}
	
	# bubble info
	if (is.null(g$geo_bubblemap)) {
		if (is.null(g$geo_bubbles)) {
			bubble.size <- NA
			bubble.col <- NA
			bubble.border <- NA
			bubble.scale <- NA
		} else {
			bubble.size <- g$geo_bubbles$bubble.size
			bubble.col <- g$geo_bubbles$bubble.col
			bubble.border <- g$geo_bubbles$bubble.border
			bubble.scale <- g$geo_bubbles$bubble.scale
		}
		bubble.legend.labels <- NA
		bubble.legend.palette <- NA
		bubble.legend.sizes <- NA
		bubble.legend.size_labels <- NA
		xsize <- NA
		xcol <- NA
	} else {
		bubbleres <- process_bubblemap(shp, g$geo_bubblemap, free.scales)
		bubble.size <- bubbleres$bubble.size
		bubble.col <- bubbleres$bubble.col
		bubble.border <- bubbleres$bubble.border
		bubble.scale <- bubbleres$bubble.scale
		bubble.legend.labels <- bubbleres$bubble.legend.labels
		bubble.legend.palette <- bubbleres$bubble.legend.palette
		bubble.legend.sizes <- bubbleres$bubble.legend.sizes
		bubble.legend.size_labels <- bubbleres$bubble.legend.size_labels
		xsize <- bubbleres$xsize
		xcol <- bubbleres$xcol
	}

	# text info
	if (is.null(g$geo_text)) {
		text <- NA
		cex <- NA
	} else {
		text <- g$geo_text$text
		cex <- g$geo_text$cex
	}
	
	
	gp <- list(shp=shp,
			   fill=fill, col=col, 
			   varnames=list(choro.fill=xfill, bubble.size=xsize, bubble.col=xcol),
			   lwd=lwd, lty=lty, 
			   bubble.size=bubble.size, bubble.col=bubble.col, 
			   bubble.border=bubble.border, 
			   bubble.scale=bubble.scale,
			   choro.values=choro.values,choro.legend.labels=choro.legend.labels, 
			   choro.legend.palette=choro.legend.palette, 
			   choro.breaks=choro.breaks, 
			   bubble.legend.labels=bubble.legend.labels, 
			   bubble.legend.palette=bubble.legend.palette,
			   bubble.legend.sizes=bubble.legend.sizes,
			   bubble.legend.size_labels=bubble.legend.size_labels,
			   text=text, cex=cex)
	gp
}