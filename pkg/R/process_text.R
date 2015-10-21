process_text <- function(data, g, fill, gt) {
	root <- NULL; text.size.lowerbound <- NULL; text.scale <- NULL; text.bg.alpha <- NULL; text.case <- NULL; text.alpha <- NULL
	text.shadow <- NULL
	
	npol <- nrow(data)

	if (gt$aes.colors.light["text"]) {
		collight <- gt$aes.colors["text"]
		coldark <- "black"
	} else {
		collight <- "white"
		coldark <- gt$aes.colors["text"]
	}
	
	if (is.na(fill[1])) fill <- ifelse(gt$aes.colors.light["text"], "black", "white")
	
	within(g, {
		#xtext <- text
		nx <- length(text)
		
		text <- if (nx > 1) matrix(unlist(lapply(data[, text]), as.character), ncol=nx) else as.character(data[[text]])
		if (!is.na(text.case)) text <- if(text.case=="upper") toupper(text) else tolower(text)
		rm(nx)
		if (is.character(text.size)) {
			if (text.size=="AREA") {
				text.size <- data$SHAPE_AREAS
			} else {
				text.size <- data[[text.size]]
			}
			text.size <- (text.size / max(text.size, na.rm=TRUE)) ^ (1/root)
		} else {
			text.size <- rep(text.size, length.out=npol)
			text.size.lowerbound <- 0
		}
		text.fontcolor <- if (is.na(text.fontcolor[1])) {
			if (is.matrix(fill)) {
				apply(fill, MARGIN=2, function(f) {
					fillrgb <- col2rgb(f)
					light <- apply(fillrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
					rep(ifelse(light, coldark, collight), length.out=npol)
				})
			} else {
				fillrgb <- col2rgb(fill)
				light <- apply(fillrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
				rep(ifelse(light, coldark, collight), length.out=npol)
			}
		} else rep(text.fontcolor, length.out=npol)
		
		if (text.shadow) {
			text.shadowcol <- if (is.matrix(text.fontcolor)) {
				apply(text.fontcolor, MARGIN=2, function(f) {
					fillrgb <- col2rgb(f)
					light <- apply(fillrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
					rep(ifelse(light, coldark, collight), length.out=npol)
				})
			} else {
				fillrgb <- col2rgb(text.fontcolor)
				light <- apply(fillrgb * c(.299, .587, .114), MARGIN=2, sum) >= 128
				rep(ifelse(light, coldark, collight), length.out=npol)
			}
		}
		
		text.xmod <- if (is.character(text.xmod)) data[[text.xmod]] else rep(text.xmod, length.out=npol)
		text.ymod <-  if (is.character(text.ymod)) data[[text.ymod]] else rep(text.ymod, length.out=npol)
		
		text_sel <- (text.size >= text.size.lowerbound)
		text_empty <- is.na(text) | is.na(text.size)
		
		if (g$text.print.tiny) {
			text.size[!text_sel & !text_empty] <- text.size.lowerbound
			text_sel <- !text_empty
		} else {
			text_sel <- text_sel & !text_empty
		}
		rm(text_empty)
		text.size <- rep(text.size * text.scale, length.out=npol)
		
		text.fontcolor <- if (is.matrix(text.fontcolor)) {
			apply(text.fontcolor, MARGIN=2, function(col) {
				do.call("process_color", c(list(col=col, alpha=text.alpha), gt$pc))
			})
		} else do.call("process_color", c(list(col=text.fontcolor, alpha=text.alpha), gt$pc))
		if (!is.na(text.bg.color)) {
			text.bg.color <- do.call("process_color", c(list(col=text.bg.color, alpha=text.bg.alpha), gt$pc))
		}
		
		if (is.na(text.fontface)) text.fontface <- gt$fontface
		if (is.na(text.fontfamily)) text.fontfamily <- gt$fontfamily
	})
	
}