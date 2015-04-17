legend_prepare <- function(gp, gt, scaleFactor) {
	varnames <- c("fill", "bubble.size", "bubble.col", "line.col", "line.lwd", "raster")
	
	ids <- lapply(varnames, function(v) {
		which(sapply(gp, function(x)!is.na(x$varnames[[v]][1])))[1]
	})
	names(ids) <- varnames
	
	varnames2 <- c("fill", "fill", "bubble.size", "bubble.col", "line.col", "line.lwd", "raster")
	
	varnames3 <- c("fill", "fill_hist", "bubble.size", "bubble.col", "line.col", "line.lwd", "raster")
	ids2 <- ids[varnames2]
	
	if (!gt$legend.show) gt$legend.config <- ""
	if (!gt$legend.hist.show) gt$legend.config <- setdiff(gt$legend.config, "fill_hist")
	
	if (is.null(names(gt$legend.is.portrait))) {
		legend.is.portrait <- rep(gt$legend.is.portrait, length.out=length(varnames2))
		names(legend.is.portrait) <- varnames2
	} else {
		legend.is.portrait <- gt$legend.is.portrait
	}
	
	
	legelem <- mapply(function(v3, v2, i) {
		if (!is.na(i) && v3 %in% gt$legend.config) {
			g <- gp[[i]]
			legend.labels <- paste(v2, "legend.labels", sep=".")
			legend.palette <- paste(v2, "legend.palette", sep=".")
			legend.misc <- paste(v2, "legend.misc", sep=".")
			if (is.na(g[[legend.labels]][1])) NULL else {
				c(list(legend.type=v3,
					   legend.is.portrait=legend.is.portrait[v2],
					   legend.labels=g[[legend.labels]],
					   legend.palette=g[[legend.palette]]),
				  g[[legend.misc]])
			} 
		} else NULL
	}, varnames3, varnames2, ids2)
	
	if (!is.null(legelem$bubble.size)) {
		legelem$bubble.size$legend.sizes <- legelem$bubble.size$legend.sizes * scaleFactor
	}
	if (!is.null(legelem$bubble.col)) {
		legelem$bubble.col$bubble.max.size <- legelem$bubble.col$bubble.max.size * scaleFactor
	}
	
	if (all(sapply(legelem, is.null)) && gt$title=="") {
		return(NULL)
	} else {
		legelem
	}
}