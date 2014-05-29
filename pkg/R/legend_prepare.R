legend_prepare <- function(gp, gt, scaleFactor) {
	choroID <- which(sapply(gp, function(x)!is.na(x$varnames$choro.fill[1])))[1]
	bubbleSizeID <- which(sapply(gp, function(x)!is.na(x$varnames$bubble.size[1])))[1]
	bubbleColID <- which(sapply(gp, function(x)!is.na(x$varnames$bubble.col[1])))[1]
	lineColID <- which(sapply(gp, function(x)!is.na(x$varnames$line.col[1])))[1]
	lineLwdID <- which(sapply(gp, function(x)!is.na(x$varnames$line.lwd[1])))[1]
	
	varnames <- c("choro.fill", "bubble.size", "bubble.col", "line.col", "line.lwd")
	
	ids <- lapply(varnames, function(v) {
		which(sapply(gp, function(x)!is.na(x$varnames[[v]][1])))[1]
	})
	names(ids) <- varnames
	
	varnames2 <- c("choro.fill", "choro.fill", "bubble.size", "bubble.col", "line.col", "line.lwd")
	varnames3 <- c("choro", "hist", "bubble.size", "bubble.col", "line.col", "line.lwd")
	ids2 <- ids[varnames2]
	
	legelem <- mapply(function(v3, v2, i) {
		if (!is.na(i) && v3 %in% gt$legend.config) {
			g <- gp[[i]]
			legend.labels <- paste(v2, "legend.labels", sep=".")
			legend.palette <- paste(v2, "legend.palette", sep=".")
			legend.misc <- paste(v2, "legend.misc", sep=".")
			if (is.na(g[[legend.labels]][1])) NULL else {
				c(list(legend.type=v3,
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