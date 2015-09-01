legend_prepare <- function(gp, gt, scaleFactor) {

	varnames <- c("fill", "bubble.size", "bubble.col", "line.col", "line.lwd", "raster")
	varnames_hist <- c("fill", "bubble.col", "line.col", "raster")
	
	# todo hist: "fill_hist"
	# is.portrait
	
	if (gt$legend.show) {
		x <- lapply(gp, function(gpl) {
			y <- lapply(varnames, function(v) {
				if (!is.na(gpl$varnames[[v]][1])) {
					if (gpl[[paste(v, "legend.show", sep=".")]]) {
						legend.labels <- paste(v, "legend.labels", sep=".")
						legend.palette <- paste(v, "legend.palette", sep=".")
						legend.title <- paste(v, "legend.title", sep=".")
						legend.is.portrait <- paste(v, "legend.is.portrait", sep=".")
						legend.z <- paste(v, "legend.z", sep=".")
						legend.misc <- paste(v, "legend.misc", sep=".")
						list_misc <- gpl[[legend.misc]]
						if (v=="bubble.size") list_misc$legend.sizes <- list_misc$legend.size * scaleFactor / 2
						if (v=="bubble.col") list_misc$bubble.max.size <- list_misc$bubble.max.size * scaleFactor / 2
						
						
						c(list(legend.type=v,
							   legend.title=gpl[[legend.title]],
							   legend.is.portrait=gpl[[legend.is.portrait]],
							   legend.z=gpl[[legend.z]],
							   legend.labels=gpl[[legend.labels]],
							   legend.palette=gpl[[legend.palette]]),
						  list_misc)
					}
				}
			})
			
			yhist <- lapply(varnames_hist, function(v) {
				vh <- paste(v, "hist", sep="_")
				legend.hist <- paste(v, "legend.hist", sep=".")
				
				if (!is.na(gpl$varnames[[v]][1])) {
					if (gpl[[legend.hist]]) {
						legend.labels <- paste(v, "legend.labels", sep=".")
						legend.palette <- paste(v, "legend.palette", sep=".")
						legend.title <- paste(v, "legend.hist.title", sep=".")
						legend.hist.z <- paste(v, "legend.hist.z", sep=".")
						legend.hist.misc <- paste(v, "legend.hist.misc", sep=".")
						list_hist_misc <- gpl[[legend.hist.misc]]
						c(list(legend.type="hist",
							   legend.title=gpl[[legend.title]],
							   legend.is.portrait=TRUE,
							 legend.z=gpl[[legend.hist.z]],
							 legend.labels=gpl[[legend.labels]],
							 legend.palette=gpl[[legend.palette]]),
						  list_hist_misc)
					} else NULL
				} else NULL
			})
			
			c(y[!sapply(y, is.null)], yhist[!sapply(yhist, is.null)])
		})
		legelem <- do.call("c", x)
	} else legelem <- list(NULL)
	
	
# 	ids <- lapply(varnames, function(v) {
# 		which(sapply(gp, function(x)!is.na(x$varnames[[v]][1])))[1]
# 	})
# 	names(ids) <- varnames
# 	
# 	varnames2 <- c("fill", "fill", "bubble.size", "bubble.col", "line.col", "line.lwd", "raster")
# 	
# 	varnames3 <- c("fill", "fill_hist", "bubble.size", "bubble.col", "line.col", "line.lwd", "raster")
# 	ids2 <- ids[varnames2]
# 	
# 	if (!gt$legend.show) gt$legend.config <- ""
# 	if (!gt$legend.hist.show) gt$legend.config <- setdiff(gt$legend.config, "fill_hist")
# 	
# 	if (is.null(names(gt$legend.is.portrait))) {
# 		legend.is.portrait <- rep(gt$legend.is.portrait, length.out=length(varnames2))
# 		names(legend.is.portrait) <- varnames2
# 	} else {
# 		legend.is.portrait <- gt$legend.is.portrait
# 	}
# 	
# 	
# 	legelem <- mapply(function(v3, v2, i) {
# 		if (!is.na(i) && v3 %in% gt$legend.config) {
# 			g <- gp[[i]]
# 			legend.labels <- paste(v2, "legend.labels", sep=".")
# 			legend.palette <- paste(v2, "legend.palette", sep=".")
# 			legend.misc <- paste(v2, "legend.misc", sep=".")
# 			if (is.na(g[[legend.labels]][1])) NULL else {
# 				c(list(legend.type=v3,
# 					   legend.is.portrait=legend.is.portrait[v2],
# 					   legend.labels=g[[legend.labels]],
# 					   legend.palette=g[[legend.palette]]),
# 				  g[[legend.misc]])
# 			} 
# 		} else NULL
# 	}, varnames3, varnames2, ids2)
# 	
# 	if (!is.null(legelem$bubble.size)) {
# 		legelem$bubble.size$legend.sizes <- legelem$bubble.size$legend.sizes * scaleFactor
# 	}
# 	if (!is.null(legelem$bubble.col)) {
# 		legelem$bubble.col$bubble.max.size <- legelem$bubble.col$bubble.max.size * scaleFactor
# 	}
	
	if (all(sapply(legelem, is.null))) {
		return(NULL)
	} else {
		legelem
	}
}