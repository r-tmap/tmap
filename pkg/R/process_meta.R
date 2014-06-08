process_meta <- function(gt, gf, nx, varnames) {
	
	gf <- within(gf, {
		by <- NULL
		if (is.null(ncol) && is.null(nrow)) {
			## default setting: place next to each other, or in grid
			if (nx <= 3) {
				ncol <- nx
				nrow <- 1
			} else {
				ncol <- ceiling(sqrt(nx))
				nrow <- ceiling(nx / ncol)
			}
		} else {
			if (is.null(ncol)) ncol <- ceiling(nx / nrow)
			if (is.null(nrow)) nrow <- ceiling(nx / ncol)
		}
	})
	
	m <- gf$ncol * gf$nrow
	
	gt <- within(gt, {
		if (is.na(title[1])) {
			id <- which(as.logical(sapply(varnames, function(x)sum(!is.na(x[1])))))[1]
		} else id <- switch(title[1],
							by=1,
							fill=2,
							bubble.size=3,
							bubble.col=4,
							line.col=5,
							line.lwd=6,
							0)
		if (is.na(id)) {
			title <- rep("", nx)
		} else if (id!=0) {
			legend.titles <- lapply(legend.titles, function(x) {
				idx <- substitute(x)[[3]] + 1
				if (is.na(x) && id!=idx) varnames[[idx]] else x
			})
			title <- rep(varnames[[id]], length.out=nx)
		}
		rm(id)
		legend.titles <- lapply(legend.titles, function(x) if (is.na(x[1])) "" else x)
		legend.titles <- lapply(legend.titles, function(x) rep(x, length.out=nx))
	
		scale <- scale / m
		
		title.cex <- title.cex * scale
		legend.title.cex <- legend.title.cex * scale
		legend.text.cex <- legend.text.cex * scale
		legend.hist.cex <- legend.hist.cex * scale
				
		if (is.null(bg.color)) bg.color <- ifelse(is.na(varnames$fill[1]), "white", "grey85")
		
		if (identical(title.bg.color, TRUE)) title.bg.color <- bg.color
		if (identical(legend.bg.color, TRUE)) legend.bg.color <- bg.color
	})	
	
	c(gt, gf)
}
