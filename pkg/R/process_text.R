process_text <- function(data, g, fill) {
	root <- NULL; text.cex.lowerbound <- NULL; text.scale <- NULL; text.bg.alpha <- NULL; text.case <- NULL; text.alpha <- NULL
	
	npol <- nrow(data)
	
	within(g, {
		#xtext <- text
		nx <- length(text)
		
		text <- if (nx > 1) matrix(unlist(lapply(data[, text]), as.character), ncol=nx) else as.character(data[[text]])
		if (!is.na(text.case)) text <- if(text.case=="upper") toupper(text) else tolower(text)
		rm(nx)
		
		if (is.character(text.cex)) {
			if (text.cex=="AREA") {
				text.cex <- data$SHAPE_AREAS
			} else {
				text.cex <- data[[text.cex]]
			}
			text.cex <- (text.cex / max(text.cex, na.rm=TRUE)) ^ (1/root)
		} else text.cex <- rep(text.cex, length.out=npol)
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
		text.xmod <- if (is.character(text.xmod)) data[[text.xmod]] else rep(text.xmod, length.out=npol)
		text.ymod <-  if (is.character(text.ymod)) data[[text.ymod]] else rep(text.ymod, length.out=npol)
		
		text_sel <- (text.cex >= text.cex.lowerbound)
		text_empty <- is.na(text) | is.na(text.cex)
		
		if (g$text.print.tiny) {
			text.cex[!text_sel & !text_empty] <- text.cex.lowerbound
			text_sel <- !text_empty
		} else {
			text_sel <- text_sel & !text_empty
		}
		rm(text_empty)
		text.cex <- rep(text.cex * text.scale, length.out=npol)
		
		text.fontcolor <- if (is.matrix(text.fontcolor)) {
			apply(text.fontcolor, MARGIN=2, function(col) {
				get_alpha_col(col, text.alpha)
			})
		} else get_alpha_col(text.fontcolor, text.alpha)
		if (!is.na(text.bg.color)) {
			text.bg.color <- get_alpha_col(text.bg.color, text.bg.alpha)
		}
	})
	
}