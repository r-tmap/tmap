process_text <- function(data, g, fill) {
	npol <- nrow(data)
	
	within(g, {
		text <- data[[text]]
		
		if (is.character(text.cex)) {
			if (substr(text.cex, 1, 4)=="AREA") {
				nc <- nchar(text.cex)
				p <- if (nc>4) as.numeric(substr(text.cex, 5, nc)) else 2
				text.cex <- (data$SHAPE_AREAS / max(data$SHAPE_AREAS, na.rm=TRUE)) ^ (1/p)
				rm(nc, p)
			} else {
				text.cex <- data[[text.cex]]
				text.cex <- text.cex / max(text.cex, na.rm=TRUE)
			}
		} else text.cex <- rep(text.cex, lenght.out=npol)
		text.fontcolor <- if (is.na(text.fontcolor[1])) {
			if (is.matrix(fill)) {
				apply(fill, MARGIN=2, function(f) {
					fillrgb <- col2rgb(f)
					light <- apply(fillrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
					rep(ifelse(light, "black", "white"), length.out=npol)
				})
			} else {
				fillrgb <- col2rgb(fill)
				light <- apply(fillrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
				rep(ifelse(light, "black", "white"), length.out=npol)
			}
		} else rep(text.fontcolor, length.out=npol)
		if (is.character(text.xmod)) text.xmod <- data[[text.xmod]]
		if (is.character(text.ymod)) text.ymod <- data[[text.ymod]]
		text_sel <- (text.cex >= text.cex.lowerbound)
		text_empty <- is.na(text)
		
		if (g$text.print.tiny) {
			text.cex[!text_sel & !text_empty] <- text.cex.lowerbound
			text_sel <- !text_empty
		} else {
			text_sel <- text_sel & !text_empty
		}
		rm(text_empty)
		text.cex <- rep(text.cex * text.scale, length.out=npol)
		
		if (!is.na(text.bg.color)) {
			bgcols <- col2rgb(text.bg.color)
			bgcols <- rgb(bgcols[1,], bgcols[2,], bgcols[3,], 
						  alpha=text.bg.alpha, maxColorValue=255)
			text.bg.color <- bgcols
			rm(bgcols)
		}
	})
}